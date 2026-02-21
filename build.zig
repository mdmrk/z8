const std = @import("std");

fn getVersion(b: *std.Build) ![]const u8 {
    const result = try std.process.Child.run(.{
        .allocator = b.allocator,
        .argv = &.{ "date", "+%Y%m%d" },
    });
    defer b.allocator.free(result.stdout);
    defer b.allocator.free(result.stderr);

    const date = std.mem.trim(u8, result.stdout, &std.ascii.whitespace);
    const git_result = try std.process.Child.run(.{
        .allocator = b.allocator,
        .argv = &.{ "git", "rev-parse", "--short", "HEAD" },
    });
    defer b.allocator.free(git_result.stdout);
    defer b.allocator.free(git_result.stderr);

    const commit = std.mem.trim(u8, git_result.stdout, &std.ascii.whitespace);
    return b.fmt("1.0.0-{s}-{s}", .{ date, commit });
}

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const version = getVersion(b) catch |err| {
        std.log.err("Failed to get version info: {}", .{err});
        return;
    };
    const exe = b.addExecutable(.{
        .name = "z8",
        .root_module = b.createModule(.{
            .root_source_file = b.path("z8.zig"),
            .target = target,
            .optimize = optimize,
            .strip = true,
        }),
        .version = try std.SemanticVersion.parse(version),
        .use_llvm = true,
    });

    const options = b.addOptions();
    options.addOption([]const u8, "version", version);
    exe.root_module.addOptions("z8_options", options);

    const sdl3 = b.dependency("sdl3", .{
        .target = target,
        .optimize = optimize,
        .c_sdl_preferred_linkage = .static,
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
}
