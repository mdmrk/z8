const std = @import("std");

const Args = struct {
    help: bool = false,
    rom_path: ?[]const u8 = null,
};

fn parseArgs() !Args {
    const alloc = std.heap.page_allocator;
    var args_it = try std.process.argsWithAllocator(alloc);
    defer args_it.deinit();
    var args = Args{};

    const S = struct {
        inline fn makeArg(arg: []const u8, comptime short: []const u8, comptime long: []const u8) bool {
            return std.mem.eql(u8, arg, short) or std.mem.eql(u8, arg, long);
        }
    };

    _ = args_it.skip();
    while (args_it.next()) |arg| {
        if (S.makeArg(arg, "-h", "--help")) {
            args.help = true;
        } else {
            args.rom_path = arg;
        }
    }

    return args;
}

const Handler = *const fn (*Cpu) void;

const OpHandler = struct {
    opcode: u16,
    handler: Handler,
};

fn handlerU00E0(_: *Cpu) void {
    unreachable;
}

fn handlerU00EE(_: *Cpu) void {
    unreachable;
}

fn handlerUnreachable(_: *Cpu) void {
    unreachable;
}

const Bus = struct {
    const mem_size = 4 * 1024 * 1024;

    mem: [mem_size]u8 align(2),

    pub fn read(self: *const Bus, comptime T: type, addr: u16) T {
        return @as(*T, @alignCast(@constCast(@ptrCast(self.mem[addr..])))).*;
    }

    // pub fn write(self: *Bus, addr: u16,value: u8) void {}
};

const Cpu = struct {
    pc: u16,
    regs: [0xF]u8,
    bus: *Bus,
    cycle: usize,

    pub fn fetch(self: *Cpu) u16 {
        defer self.pc += 2;
        return self.bus.read(u16, self.pc);
    }

    pub fn decode(_: *const Cpu, opcode: u16) ?Handler {
        const op_map = [_]OpHandler{
            .{ .opcode = 0x00E0, .handler = handlerU00E0 },
            .{ .opcode = 0x00EE, .handler = handlerU00EE },
        };

        for (op_map) |op| {
            if (opcode & op.opcode == 0) {
                return op.handler;
            }
        }
        return null;
    }

    pub fn execute(self: *Cpu, handler: Handler) void {
        handler(self);
    }
};

const Z8 = struct {
    alloc: std.mem.Allocator,
    cpu: Cpu,
    bus: Bus,

    pub fn init() !Z8 {
        var bus = Bus{
            .mem = [_]u8{0} ** Bus.mem_size,
        };
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const alloc = gpa.allocator();

        return .{
            .alloc = alloc,
            .bus = bus,
            .cpu = Cpu{
                .pc = 0x200,
                .bus = &bus,
                .regs = [_]u8{0} ** 0xF,
                .cycle = 0,
            },
        };
    }

    fn loadRom(self: *Z8, rom_path: []const u8) !void {
        std.log.info("Loading {s}", .{rom_path});
        const file = try std.fs.cwd().openFile(rom_path, .{ .mode = .read_only });
        defer file.close();

        const end_addr = 0xE9F;
        const start_addr = 0x200;
        const max_size = end_addr - start_addr;
        const contents = try file.readToEndAlloc(self.alloc, max_size);
        defer self.alloc.free(contents);

        std.mem.copyForwards(u8, self.bus.mem[start_addr..], contents);
    }

    pub fn run(self: *Z8) void {
        while (true) {
            const opcode = self.cpu.fetch();
            const handler = self.cpu.decode(opcode);
            if (handler) |h| {
                self.cpu.execute(h);
            } else {
                std.log.warn("Invalid opcode 0x{x:0<4}", .{opcode});
            }
        }
    }
};

pub fn main() !void {
    const args = try parseArgs();

    if (args.help) {
        return;
    }
    if (args.rom_path == null) {
        std.log.err("Provide a rom path", .{});
        return error.MissingRomPath;
    }
    var z8 = try Z8.init();
    try z8.loadRom(args.rom_path.?);
    z8.run();
}
