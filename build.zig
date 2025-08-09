const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "z8",
        .root_module = b.createModule(.{
            .root_source_file = b.path("z8.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const sdl3 = b.dependency("sdl3", .{
        .target = target,
        .optimize = optimize,
        .c_sdl_preferred_linkage = .static,
        // .c_sdl_lto = .none,
    });
    exe.root_module.addImport("sdl3", sdl3.module("sdl3"));

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });

    const run_exe_tests = b.addRunArtifact(exe_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_exe_tests.step);
}
