const std = @import("std");
const sdl3 = @import("sdl3");

const Args = struct {
    help: bool = false,
    rom_path: ?[]const u8 = null,
};

fn parseArgs() !Args {
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

const Handler = *const fn (*Cpu, u16) void;

const OpHandler = struct {
    opcode: u16,
    mask: u16,
    handler: Handler,
};

fn handler0NNN(_: *Cpu, opcode: u16) void {
    _ = opcode;
}

fn handler00E0(_: *Cpu, opcode: u16) void {
    _ = opcode;
}

fn handler00EE(_: *Cpu, opcode: u16) void {
    _ = opcode;
}

fn handler6XNN(self: *Cpu, opcode: u16) void {
    const x: u8 = @truncate((opcode & 0x0F00) >> 8);
    const nn: u8 = @truncate(opcode & 0x00FF);
    self.regs[x] = nn;
}

fn handler7XNN(self: *Cpu, opcode: u16) void {
    const x: u8 = @truncate((opcode & 0x0F00) >> 8);
    const nn: u8 = @truncate(opcode & 0x00FF);
    self.regs[x] += nn;
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

    pub fn write(comptime T: type, self: *Bus, addr: u16, value: T) void {
        @as(*T, @alignCast(@constCast(@ptrCast(self.mem[addr..])))).* = value;
    }
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
            .{ .opcode = 0x00E0, .mask = 0x0000, .handler = handler00E0 },
            .{ .opcode = 0x00EE, .mask = 0x0000, .handler = handler00EE },
            .{ .opcode = 0x0000, .mask = 0x0111, .handler = handler0NNN },
            .{ .opcode = 0x6000, .mask = 0x0111, .handler = handler6XNN },
            .{ .opcode = 0x7000, .mask = 0x0111, .handler = handler7XNN },
        };

        for (op_map) |op| {
            if (opcode & ~op.mask == op.opcode) {
                return op.handler;
            }
        }
        return null;
    }

    pub fn execute(self: *Cpu, handler: Handler, opcode: u16) void {
        handler(self, opcode);
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
                self.cpu.execute(h, opcode);
            } else {
                std.log.warn("Invalid opcode 0x{x:0<4}", .{opcode});
            }
        }
    }
};

const alloc = std.heap.smp_allocator;
const fps = 60;
const screen_width = 640;
const screen_height = 480;

const AppState = struct {
    fps_capper: sdl3.extras.FramerateCapper(f32),
    window: sdl3.video.Window,
    renderer: sdl3.render.Renderer,
};

fn init(
    app_state: *?*AppState,
    _: [][*:0]u8,
) !sdl3.AppResult {
    const args = try parseArgs();

    if (args.help) {
        return .success;
    }
    if (args.rom_path == null) {
        std.log.err("Provide a rom path", .{});
        return error.MissingRomPath;
    }

    const init_flags: sdl3.InitFlags = .{ .video = true };
    try sdl3.init(init_flags);

    const wr = try sdl3.render.Renderer.initWithWindow("z8", screen_width, screen_height, .{});
    const window = wr.window;
    const renderer = wr.renderer;

    var z8 = try Z8.init();
    try z8.loadRom(args.rom_path.?);
    // z8.run();

    const state = try alloc.create(AppState);
    state.* = .{
        .fps_capper = .{
            .mode = .{
                .limited = fps,
            },
        },
        .window = window,
        .renderer = renderer,
    };
    app_state.* = state;
    return .run;
}

fn iterate(
    app_state: *AppState,
) !sdl3.AppResult {
    const dt = app_state.fps_capper.delay();
    _ = dt;

    try app_state.renderer.setDrawColorFloat(.{ .r = 0, .b = 0, .g = 0, .a = 1 });
    try app_state.renderer.clear();
    try app_state.renderer.present();

    return .run;
}

fn event(
    app_state: *AppState,
    curr_event: sdl3.events.Event,
) !sdl3.AppResult {
    _ = app_state;

    return switch (curr_event) {
        .quit => .success,
        .terminating => .success,
        else => .run,
    };
}

fn quit(
    app_state: ?*AppState,
    result: sdl3.AppResult,
) void {
    if (app_state) |state| {
        state.renderer.deinit();
        state.window.deinit();
        alloc.destroy(state);
    }
    if (result == .success) {
        std.log.info("Application quit successfully", .{});
    }
}

pub fn main() u8 {
    sdl3.main_funcs.setMainReady();
    var args = [_:null]?[*:0]u8{};
    return sdl3.main_funcs.enterAppMainCallbacks(&args, AppState, init, iterate, event, quit);
}
