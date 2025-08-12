const std = @import("std");
const sdl3 = @import("sdl3");

var args: struct {
    help: bool = false,
    step: bool = false,
    rom_path: ?[]const u8 = null,
} = .{};
var want_step = true;
var rand: std.Random = undefined;

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

fn handler00EE(cpu: *Cpu, _: *Ppu, _: u16) void {
    if (cpu.sp == 0) {
        std.log.err("Reached stack bottom", .{});
        return;
    }
    cpu.sp -= 1;
    cpu.pc = cpu.stack[cpu.sp];
}

fn handler1NNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const nnn: u16 = @truncate(opcode & 0x0FFF);
    cpu.pc = nnn;
}

fn handler2NNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const nnn: u16 = @truncate(opcode & 0x0FFF);
    if (cpu.sp == cpu.stack.len - 1) {
        std.log.err("Stack overflow", .{});
        return;
    }
    cpu.stack[cpu.sp] = cpu.pc;
    cpu.sp += 1;
    cpu.pc = nnn;
}

fn handler3XNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const nn: u8 = @truncate(opcode & 0x00FF);
    if (cpu.regs[vx] == nn) {
        cpu.pc += 2;
    }
}

fn handler4XNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const nn: u8 = @truncate(opcode & 0x00FF);
    if (cpu.regs[vx] != nn) {
        cpu.pc += 2;
    }
}

fn handler5XY0(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00F0) >> 4);
    if (cpu.regs[vx] == cpu.regs[vy]) {
        cpu.pc += 2;
    }
}

fn handler6XNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const nn: u8 = @truncate(opcode & 0x00FF);
    cpu.regs[vx] = nn;
}

fn handler7XNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const nn: u8 = @truncate(opcode & 0x00FF);
    cpu.regs[vx] +%= nn;
}

fn handler8XY0(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    cpu.regs[vx] = cpu.regs[vy];
}

fn handler8XY1(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    cpu.regs[vx] |= cpu.regs[vy];
}

fn handler8XY2(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    cpu.regs[vx] &= cpu.regs[vy];
}

fn handler8XY3(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    cpu.regs[vx] ^= cpu.regs[vy];
}

fn handler8XY4(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    if ((vx +% vy) < vx or (vx +% vy) < vy) {
        cpu.regs[0xF] = 1;
    } else {
        cpu.regs[0xF] = 0;
    }
    cpu.regs[vx] +%= cpu.regs[vy];
}

fn handler8XY5(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    if (cpu.regs[vx] > cpu.regs[vy]) {
        cpu.regs[0xF] = 1;
    } else {
        cpu.regs[0xF] = 0;
    }
    cpu.regs[vx] -%= cpu.regs[vy];
}

fn handler8XY6(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    cpu.regs[0xF] = cpu.regs[vy] & 1;
    cpu.regs[vx] = cpu.regs[vy] >> 1;
}

fn handler8XY7(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    if (cpu.regs[vx] > cpu.regs[vy]) {
        cpu.regs[0xF] = 1;
    } else {
        cpu.regs[0xF] = 0;
    }
    cpu.regs[vx] = cpu.regs[vy] -% cpu.regs[vx];
}

fn handler8XYE(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00FF) >> 4);
    cpu.regs[0xF] = (cpu.regs[vy] >> 7) & 1;
    cpu.regs[vx] = cpu.regs[vy] << 1;
}

fn handler9XY0(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const vy: u8 = @truncate((opcode & 0x00F0) >> 4);
    if (cpu.regs[vx] != cpu.regs[vy]) {
        cpu.pc += 2;
    }
}

fn handlerANNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const nnn: u16 = @truncate(opcode & 0x0FFF);
    cpu.i = nnn;
}

fn handlerBNNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const nnn: u16 = @truncate(opcode & 0x0FFF);
    cpu.i = nnn + cpu.regs[0];
}

fn handlerCXNN(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const nn: u8 = @truncate(opcode & 0x00FF);
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    const random_n = rand.int(u8) & nn;
    cpu.regs[vx] = random_n;
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

fn handlerEX9E(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    if (cpu.pressed) |pressed| {
        if (cpu.regs[vx] == pressed) {
            cpu.pc += 2;
        }
    }
}

fn handlerEXA1(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    if (cpu.pressed) |pressed| {
        if (cpu.regs[vx] != pressed) {
            cpu.pc += 2;
        }
    } else {
        cpu.pc += 2;
    }
}

// fn handlerFX07(cpu: *Cpu, _: *Ppu, opcode: u16) void {}

fn handlerFX0A(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    if (cpu.pressed) |pressed| {
        cpu.regs[vx] = pressed;
        cpu.can_inc_pc = true;
    } else {
        cpu.can_inc_pc = false;
    }
}

// fn handlerFX15(cpu: *Cpu, _: *Ppu, opcode: u16) void {}

// fn handlerFX18(cpu: *Cpu, _: *Ppu, opcode: u16) void {}

fn handlerFX1E(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u8 = @truncate((opcode & 0x0F00) >> 8);
    cpu.i +%= cpu.regs[vx];
}

// fn handlerFX29(cpu: *Cpu, _: *Ppu, opcode: u16) void {}

fn handlerFX33(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u16 = @truncate((opcode & 0x0F00) >> 8);
    const S = struct {
        fn binToBCD(n: u8) u8 {
            return switch (n) {
                0 => 0b0000_0000,
                1 => 0b0000_0001,
                2 => 0b0000_0010,
                3 => 0b0000_0011,
                4 => 0b0000_0100,
                5 => 0b0000_0101,
                6 => 0b0000_0110,
                7 => 0b0000_0111,
                8 => 0b0000_1000,
                9 => 0b0000_1001,
                else => unreachable,
            };
        }
    };
    cpu.bus.write(u8, cpu.i + 2, S.binToBCD((cpu.regs[vx] / 100) % 10));
    cpu.bus.write(u8, cpu.i + 1, S.binToBCD((cpu.regs[vx] / 10) % 10));
    cpu.bus.write(u8, cpu.i + 0, S.binToBCD((cpu.regs[vx] / 1) % 10));
}

fn handlerFX55(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u16 = @truncate((opcode & 0x0F00) >> 8);
    for (0..vx + 1) |i| {
        cpu.bus.write(u8, cpu.i + @as(u16, @intCast(i)), cpu.regs[i]);
    }
    cpu.i = cpu.i + vx + 1;
}

fn handlerFX65(cpu: *Cpu, _: *Ppu, opcode: u16) void {
    const vx: u16 = @truncate((opcode & 0x0F00) >> 8);
    for (0..vx + 1) |i| {
        cpu.regs[i] = cpu.bus.read(u8, cpu.i);
    }
    cpu.i = cpu.i + vx + 1;
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

    pub fn write(self: *Bus, comptime T: type, addr: u16, value: T) void {
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
    stack: [16]u16,
    sp: u8,
    regs: [16]u8,
    i: u16,
    bus: Bus,
    cycles: usize,
    pressed: ?u8,
    can_inc_pc: bool,

    pub fn init() Cpu {
        return .{
            .pc = 0x200,
            .stack = [_]u16{0} ** 16,
            .sp = 0,
            .regs = [_]u8{0} ** 16,
            .i = 0,
            .bus = Bus.init(),
            .cycles = 0,
            .pressed = null,
            .can_inc_pc = true,
        };
    }

    pub fn fetch(self: *Cpu) u16 {
        const opcode_be = self.bus.read(u16, self.pc);
        const opcode = std.mem.bigToNative(u16, opcode_be);
        return opcode;
    }

    pub fn decode(self: *const Cpu, opcode: u16) ?Handler {
        const op_map = [_]OpHandler{
            .{ .opcode = 0x00E0, .mask = 0x0000, .handler = handler00E0 },
            .{ .opcode = 0x00EE, .mask = 0x0000, .handler = handler00EE },
            .{ .opcode = 0x0000, .mask = 0x0FFF, .handler = handler0NNN },
            .{ .opcode = 0x1000, .mask = 0x0FFF, .handler = handler1NNN },
            .{ .opcode = 0x2000, .mask = 0x0FFF, .handler = handler2NNN },
            .{ .opcode = 0x3000, .mask = 0x0FFF, .handler = handler3XNN },
            .{ .opcode = 0x4000, .mask = 0x0FFF, .handler = handler4XNN },
            .{ .opcode = 0x5000, .mask = 0x0FF0, .handler = handler5XY0 },
            .{ .opcode = 0x6000, .mask = 0x0FFF, .handler = handler6XNN },
            .{ .opcode = 0x7000, .mask = 0x0FFF, .handler = handler7XNN },
            .{ .opcode = 0x8000, .mask = 0x0FF0, .handler = handler8XY0 },
            .{ .opcode = 0x8001, .mask = 0x0FF0, .handler = handler8XY1 },
            .{ .opcode = 0x8002, .mask = 0x0FF0, .handler = handler8XY2 },
            .{ .opcode = 0x8003, .mask = 0x0FF0, .handler = handler8XY3 },
            .{ .opcode = 0x8004, .mask = 0x0FF0, .handler = handler8XY4 },
            .{ .opcode = 0x8005, .mask = 0x0FF0, .handler = handler8XY5 },
            .{ .opcode = 0x8006, .mask = 0x0FF0, .handler = handler8XY6 },
            .{ .opcode = 0x8007, .mask = 0x0FF0, .handler = handler8XY7 },
            .{ .opcode = 0x800E, .mask = 0x0FF0, .handler = handler8XYE },
            .{ .opcode = 0x9000, .mask = 0x0FF0, .handler = handler9XY0 },
            .{ .opcode = 0xA000, .mask = 0x0FFF, .handler = handlerANNN },
            .{ .opcode = 0xB000, .mask = 0x0FFF, .handler = handlerBNNN },
            .{ .opcode = 0xC000, .mask = 0x0FFF, .handler = handlerCXNN },
            .{ .opcode = 0xD000, .mask = 0x0FFF, .handler = handlerDXYN },
            .{ .opcode = 0xE09E, .mask = 0x0F00, .handler = handlerEX9E },
            .{ .opcode = 0xE0A1, .mask = 0x0F00, .handler = handlerEXA1 },
            // .{ .opcode = 0xF007, .mask = 0x0F00, .handler = handlerFX07 },
            .{ .opcode = 0xF00A, .mask = 0x0F00, .handler = handlerFX0A },
            // .{ .opcode = 0xF015, .mask = 0x0F00, .handler = handlerFX15 },
            // .{ .opcode = 0xF018, .mask = 0x0F00, .handler = handlerFX18 },
            .{ .opcode = 0xF01E, .mask = 0x0F00, .handler = handlerFX1E },
            // .{ .opcode = 0xF029, .mask = 0x0F00, .handler = handlerFX29 },
            .{ .opcode = 0xF033, .mask = 0x0F00, .handler = handlerFX33 },
            .{ .opcode = 0xF055, .mask = 0x0F00, .handler = handlerFX55 },
            .{ .opcode = 0xF065, .mask = 0x0F00, .handler = handlerFX65 },
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
        if (self.can_inc_pc) {
            self.pc += 2;
        }
    }
};

const Ppu = struct {
    const screen_width = 64;
    const screen_height = 32;

    const PixelColor = struct {
        var unset: sdl3.pixels.Color = .{
            .r = 0,
            .b = 255,
            .g = 0,
            .a = 255,
        };
        var set: sdl3.pixels.Color = .{
            .r = 255,
            .b = 156,
            .g = 255,
            .a = 255,
        };
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
                    pixels[idx * pixel_size] = Ppu.PixelColor.set.r;
                    pixels[idx * pixel_size + 1] = Ppu.PixelColor.set.g;
                    pixels[idx * pixel_size + 2] = Ppu.PixelColor.set.b;
                    pixels[idx * pixel_size + 3] = Ppu.PixelColor.set.a;
                } else {
                    pixels[idx * pixel_size] = Ppu.PixelColor.unset.r;
                    pixels[idx * pixel_size + 1] = Ppu.PixelColor.unset.g;
                    pixels[idx * pixel_size + 2] = Ppu.PixelColor.unset.b;
                    pixels[idx * pixel_size + 3] = Ppu.PixelColor.unset.a;
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
        std.debug.print("i={d}\n", .{self.cpu.i});
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
    errdefer |err| {
        if (err == error.SdlError) {
            std.log.err("{?s}\n", .{sdl3.errors.get()});
        }
    }
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

    var prng = std.Random.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    rand = prng.random();

    const window_title = try std.fmt.allocPrint(alloc, "z8 - {s}", .{args.rom_path.?});
    const win_and_rend = try sdl3.render.Renderer.initWithWindow(
        try alloc.dupeZ(u8, window_title),
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
    try app_state.z8.renderer.setDrawColorFloat(.{
        .r = @as(f32, @floatFromInt(Ppu.PixelColor.unset.r)) / 255,
        .g = @as(f32, @floatFromInt(Ppu.PixelColor.unset.g)) / 255,
        .b = @as(f32, @floatFromInt(Ppu.PixelColor.unset.b)) / 255,
        .a = @as(f32, @floatFromInt(Ppu.PixelColor.unset.a)) / 255,
    });
    try app_state.z8.renderer.clear();
    try app_state.z8.renderer.renderTexture(app_state.z8.ppu.canvas, null, null);
    try app_state.z8.renderer.present();

    return .run;
}

fn event(
    app_state: *AppState,
    curr_event: sdl3.events.Event,
) !sdl3.AppResult {
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
                    .kp_0 => {
                        app_state.z8.cpu.pressed = 0x0;
                    },
                    .kp_1 => {
                        app_state.z8.cpu.pressed = 0x1;
                    },
                    .kp_2 => {
                        app_state.z8.cpu.pressed = 0x2;
                    },
                    .kp_3 => {
                        app_state.z8.cpu.pressed = 0x3;
                    },
                    .kp_4 => {
                        app_state.z8.cpu.pressed = 0x4;
                    },
                    .kp_5 => {
                        app_state.z8.cpu.pressed = 0x5;
                    },
                    .kp_6 => {
                        app_state.z8.cpu.pressed = 0x6;
                    },
                    .kp_7 => {
                        app_state.z8.cpu.pressed = 0x7;
                    },
                    .kp_8 => {
                        app_state.z8.cpu.pressed = 0x8;
                    },
                    .kp_9 => {
                        app_state.z8.cpu.pressed = 0x9;
                    },
                    .a => {
                        app_state.z8.cpu.pressed = 0xA;
                    },
                    .b => {
                        app_state.z8.cpu.pressed = 0xB;
                    },
                    .c => {
                        app_state.z8.cpu.pressed = 0xC;
                    },
                    .d => {
                        app_state.z8.cpu.pressed = 0xD;
                    },
                    .e => {
                        app_state.z8.cpu.pressed = 0xE;
                    },
                    .f => {
                        app_state.z8.cpu.pressed = 0xF;
                    },
                    else => {
                        app_state.z8.cpu.pressed = null;
                    },
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
