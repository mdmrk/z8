const std = @import("std");
const sdl3 = @import("sdl3");

var args: struct {
    help: bool = false,
    step: bool = false,
    rom_path: ?[]const u8 = null,
} = .{};
var want_step = true;

fn parseArgs() !void {
    var args_it = try std.process.argsWithAllocator(alloc);
    defer args_it.deinit();

    const S = struct {
        inline fn checkArg(arg: []const u8, comptime short: []const u8, comptime long: []const u8) bool {
            return std.mem.eql(u8, arg, short) or std.mem.eql(u8, arg, long);
        }
    };

    _ = args_it.skip();
    while (args_it.next()) |arg| {
        if (S.checkArg(arg, "-h", "--help")) {
            args.help = true;
        } else if (S.checkArg(arg, "-s", "--step")) {
            args.step = true;
        } else {
            args.rom_path = arg;
        }
    }
}

const Handler = *const fn (*Cpu, *Ppu, u16) void;

const OpHandler = struct {
    opcode: u16,
    mask: u16,
    handler: Handler,
};

fn handler0NNN(_: *Cpu, _: *Ppu, opcode: u16) void {
    _ = opcode;
}

fn handler00E0(_: *Cpu, ppu: *Ppu, _: u16) void {
    ppu.clear();
}

fn handler00EE(_: *Cpu, _: *Ppu, opcode: u16) void {
    _ = opcode;
}

fn handler6XNN(self: *Cpu, _: *Ppu, opcode: u16) void {
    const x: u8 = @truncate((opcode & 0x0F00) >> 8);
    const nn: u8 = @truncate(opcode & 0x00FF);
    self.regs[x] = nn;
}

fn handler7XNN(self: *Cpu, _: *Ppu, opcode: u16) void {
    const x: u8 = @truncate((opcode & 0x0F00) >> 8);
    const nn: u8 = @truncate(opcode & 0x00FF);
    self.regs[x] +%= nn;
}

fn handlerANNN(self: *Cpu, _: *Ppu, opcode: u16) void {
    const nnn: u16 = @truncate(opcode & 0x0FFF);
    self.i = nnn;
}

fn handlerDXYN(cpu: *Cpu, ppu: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00F0) >> 4);
    const n: u8 = @truncate(opcode & 0x000F);

    const x_coord = cpu.regs[vx];
    const y_coord = cpu.regs[vy];
    const sprite_width = 8;
    const sprite_data = cpu.bus.readMany(cpu.i, n);

    cpu.regs[0xF] = 0;
    for (0..n) |y_offset| {
        if (y_coord + y_offset >= Ppu.screen_height) continue;

        const sprite_byte = sprite_data[y_offset];
        for (0..sprite_width) |x_offset| {
            if (x_coord + x_offset >= Ppu.screen_width) continue;

            const screen_idx = ((y_coord + y_offset) * Ppu.screen_width) + (x_coord + x_offset);
            const sprite_pixel = (sprite_byte >> @as(u3, @intCast(7 - x_offset))) & 1;

            if (sprite_pixel == 1) {
                if (ppu.pixels[screen_idx] == 1) {
                    cpu.regs[0xF] = 1;
                }
                ppu.pixels[screen_idx] ^= 1;
            }
        }
    }
}

const Bus = struct {
    const mem_size = 4 * 1024 * 1024;

    mem: [mem_size]u8 align(2),

    pub fn init() Bus {
        return .{
            .mem = [_]u8{0} ** Bus.mem_size,
        };
    }

    pub fn read(self: *const Bus, comptime T: type, addr: u16) T {
        return @as(*T, @alignCast(@constCast(@ptrCast(self.mem[addr..])))).*;
    }

    pub fn write(comptime T: type, self: *Bus, addr: u16, value: T) void {
        @as(*T, @alignCast(@constCast(@ptrCast(self.mem[addr..])))).* = value;
    }

    pub fn writeMany(self: *Bus, addr: u16, slice: []const u8) void {
        std.mem.copyForwards(u8, self.mem[addr..], slice);
    }

    pub fn readMany(self: *Bus, addr: u16, len: usize) []const u8 {
        return self.mem[addr .. addr + len];
    }
};

const Cpu = struct {
    pc: u16,
    regs: [16]u8,
    i: u16,
    bus: Bus,
    cycles: usize,

    pub fn init() Cpu {
        return .{
            .pc = 0x200,
            .regs = [_]u8{0} ** 16,
            .i = 0,
            .bus = Bus.init(),
            .cycles = 0,
        };
    }

    pub fn fetch(self: *Cpu) u16 {
        defer self.pc += 2;
        const opcode_be = self.bus.read(u16, self.pc);
        return std.mem.bigToNative(u16, opcode_be);
    }

    pub fn decode(self: *const Cpu, opcode: u16) ?Handler {
        const op_map = [_]OpHandler{
            .{ .opcode = 0x00E0, .mask = 0x0000, .handler = handler00E0 },
            .{ .opcode = 0x00EE, .mask = 0x0000, .handler = handler00EE },
            .{ .opcode = 0x0000, .mask = 0x0FFF, .handler = handler0NNN },
            .{ .opcode = 0x6000, .mask = 0x0FFF, .handler = handler6XNN },
            .{ .opcode = 0x7000, .mask = 0x0FFF, .handler = handler7XNN },
            .{ .opcode = 0xA000, .mask = 0x0FFF, .handler = handlerANNN },
            .{ .opcode = 0xD000, .mask = 0x0FFF, .handler = handlerDXYN },
        };

        for (op_map) |op| {
            if (opcode & ~op.mask == op.opcode) {
                std.log.debug("0x{x:0<4} cycles={}", .{ opcode, self.cycles });
                return op.handler;
            }
        }
        return null;
    }

    pub fn execute(self: *Cpu, ppu: *Ppu, handler: Handler, opcode: u16) void {
        handler(self, ppu, opcode);
    }
};

const Ppu = struct {
    const screen_width = 64;
    const screen_height = 32;

    const PixelColor = enum(u8) {
        unset = 0,
        set = 255,
    };

    canvas: sdl3.render.Texture,
    pixels: [screen_height * screen_width]u8,

    pub fn init(renderer: sdl3.render.Renderer) !Ppu {
        const canvas = try sdl3.render.Texture.init(
            renderer,
            .packed_rgba_8_8_8_8,
            .streaming,
            Ppu.screen_width,
            Ppu.screen_height,
        );
        try canvas.setScaleMode(.nearest);

        return .{
            .canvas = canvas,
            .pixels = .{0} ** (Ppu.screen_width * Ppu.screen_height),
        };
    }

    pub fn clear(self: *Ppu) void {
        @memset(self.pixels[0..], 0);
    }

    pub fn draw(self: *Ppu) !void {
        const texture_data = try self.canvas.lock(null);
        defer self.canvas.unlock();
        const pixel_size = 4;
        const pixels = texture_data.pixels[0 .. Ppu.screen_width * Ppu.screen_height * pixel_size];

        for (0..Ppu.screen_height) |i| {
            for (0..Ppu.screen_width) |j| {
                const idx = (i * Ppu.screen_width + j);

                if (self.pixels[idx] == 1) {
                    pixels[idx * pixel_size] = 255;
                    pixels[idx * pixel_size + 1] = 255;
                    pixels[idx * pixel_size + 2] = 255;
                    pixels[idx * pixel_size + 3] = 255;
                } else {
                    pixels[idx * pixel_size] = 0;
                    pixels[idx * pixel_size + 1] = 0;
                    pixels[idx * pixel_size + 2] = 0;
                    pixels[idx * pixel_size + 3] = 255;
                }
            }
        }
    }

    pub fn deinit(self: *Ppu) void {
        self.canvas.deinit();
    }
};

const Z8 = struct {
    alloc: std.mem.Allocator,
    cpu: Cpu,
    ppu: Ppu,
    window: sdl3.video.Window,
    renderer: sdl3.render.Renderer,

    pub fn init(
        window: sdl3.video.Window,
        renderer: sdl3.render.Renderer,
    ) !Z8 {
        return .{
            .alloc = alloc,
            .ppu = try Ppu.init(renderer),
            .cpu = Cpu.init(),
            .window = window,
            .renderer = renderer,
        };
    }

    fn loadRom(self: *Z8, rom_path: []const u8) !void {
        const file = try std.fs.cwd().openFile(rom_path, .{ .mode = .read_only });
        defer file.close();

        const end_addr = 0xE9F;
        const start_addr = 0x200;
        const max_size = end_addr - start_addr;
        const contents = try file.readToEndAlloc(self.alloc, max_size);
        defer self.alloc.free(contents);

        self.cpu.bus.writeMany(start_addr, contents);
        std.log.info("Loaded {s} ({d}B)", .{ rom_path, contents.len });
    }

    pub fn step(self: *Z8) void {
        const opcode = self.cpu.fetch();
        const handler = self.cpu.decode(opcode);
        if (handler) |h| {
            self.cpu.execute(&self.ppu, h, opcode);
        } else {
            std.log.warn("Invalid opcode 0x{x:0<4}", .{opcode});
        }
        for (0.., self.cpu.regs) |i, r| {
            std.debug.print("{x}={d} ", .{ i, r });
        }
        std.debug.print("i={d} \n", .{self.cpu.i});
        want_step = false;
    }

    pub fn deinit(self: *Z8) void {
        self.ppu.deinit();
        self.renderer.deinit();
        self.window.deinit();
    }
};

const alloc = std.heap.smp_allocator;
const fps = 60;
const window_width = Ppu.screen_width * 10;
const window_height = Ppu.screen_height * 10;

const AppState = struct {
    fps_capper: sdl3.extras.FramerateCapper(f32),
    z8: Z8,
};

fn init(
    app_state: *?*AppState,
    _: [][*:0]u8,
) !sdl3.AppResult {
    try parseArgs();

    if (args.help) {
        return .success;
    }
    if (args.rom_path == null) {
        std.log.err("Provide a rom path", .{});
        return error.MissingRomPath;
    }

    const init_flags: sdl3.InitFlags = .{ .video = true };
    try sdl3.init(init_flags);

    const win_and_rend = try sdl3.render.Renderer.initWithWindow(
        "z8",
        window_width,
        window_height,
        .{
            .resizable = true,
        },
    );
    const window = win_and_rend.window;
    const renderer = win_and_rend.renderer;

    var z8 = try Z8.init(window, renderer);
    try z8.loadRom(args.rom_path.?);

    const state = try alloc.create(AppState);
    state.* = .{
        .fps_capper = .{
            .mode = .{
                .limited = fps,
            },
        },
        .z8 = z8,
    };
    app_state.* = state;
    return .run;
}

fn iterate(
    app_state: *AppState,
) !sdl3.AppResult {
    _ = app_state.fps_capper.delay();

    if (args.step) {
        if (want_step) {
            app_state.z8.step();
        }
    } else {
        app_state.z8.step();
    }
    try app_state.z8.ppu.draw();
    try app_state.z8.renderer.setDrawColorFloat(.{ .r = 0, .b = 0, .g = 0, .a = 1 });
    try app_state.z8.renderer.clear();
    try app_state.z8.renderer.renderTexture(app_state.z8.ppu.canvas, null, null);
    try app_state.z8.renderer.present();

    return .run;
}

fn event(
    app_state: *AppState,
    curr_event: sdl3.events.Event,
) !sdl3.AppResult {
    _ = app_state;
    var result: sdl3.AppResult = .run;

    switch (curr_event) {
        .quit => {
            result = .success;
        },
        .terminating => {
            result = .success;
        },
        .key_down => |keyboard| {
            if (keyboard.key) |key| {
                switch (key) {
                    .escape, .q => {
                        result = .success;
                    },
                    .return_key => {
                        want_step = true;
                    },
                    else => {},
                }
            }
        },
        else => {},
    }
    return result;
}

fn quit(
    app_state: ?*AppState,
    result: sdl3.AppResult,
) void {
    if (app_state) |state| {
        state.z8.deinit();
        alloc.destroy(state);
    }
    if (result == .success) {
        std.log.info("Application quit successfully", .{});
    }
}

pub fn main() u8 {
    sdl3.main_funcs.setMainReady();
    var fake_args = [_:null]?[*:0]u8{};
    return sdl3.main_funcs.enterAppMainCallbacks(
        &fake_args,
        AppState,
        init,
        iterate,
        event,
        quit,
    );
}
