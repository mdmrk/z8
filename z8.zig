const std = @import("std");

const Args = struct {
    rom_path: ?[]const u8,
};

fn parseArgs() !Args {
    const alloc = std.heap.page_allocator;
    var args_it = try std.process.argsWithAllocator(alloc);
    defer args_it.deinit();
    var args = Args{
        .rom_path = null,
    };

    _ = args_it.skip();
    while (args_it.next()) |arg| {
        if (false) {} else {
            args.rom_path = arg;
        }
    }

    return args;
}

const Handler = *const fn (*Cpu) void;

const Bus = struct {
    const MEM_SIZE: usize = 4 * 1024 * 1024;

    mem: [MEM_SIZE]u8,

    // pub fn read(self: *const Bus) u16 {}

    // pub fn write(self: *Bus, value: u8) void {}
};

const Cpu = struct {
    bus: *Bus,

    // pub fn fetch(self: *Cpu) u16 {}

    // pub fn decode(self: *Cpu, opcode: u16) Handler {}

    // pub fn execute(self: *Cpu, handler: Handler) void {}
};

const Z8 = struct {
    alloc: std.mem.Allocator,
    cpu: Cpu,
    bus: Bus,

    pub fn init() !Z8 {
        var bus = Bus{
            .mem = [_]u8{0} ** Bus.MEM_SIZE,
        };
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const alloc = gpa.allocator();

        return .{
            .alloc = alloc,
            .bus = bus,
            .cpu = Cpu{
                .bus = &bus,
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
        _ = self;
        while (true) {
            // const opcode = self.cpu.fetch();
            // const handler = self.cpu.decode(opcode);
            // self.cpu.execute(handler);
        }
    }
};

pub fn main() !void {
    const args = try parseArgs();

    if (args.rom_path == null) {
        std.log.err("Provide a rom path", .{});
        return error.MissingRomPath;
    }
    var z8 = try Z8.init();
    try z8.loadRom(args.rom_path.?);
    z8.run();
}
