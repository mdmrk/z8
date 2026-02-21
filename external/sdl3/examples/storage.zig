const sdl3 = @import("sdl3");
const std = @import("std");

pub fn main() !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file_writer = std.fs.File.stdout().writer(&stdout_buffer);
    var out = stdout_file_writer.interface;

    // Setup an allocator.
    const allocator = std.heap.smp_allocator;

    // Get storage.
    const storage = try sdl3.storage.Storage.initTitle(null, null);
    defer storage.deinit() catch {};

    // Iterate a path above the application directory.
    var above_app_path = try sdl3.filesystem.Path.init(allocator, try sdl3.filesystem.getBasePath());
    defer above_app_path.deinit();
    _ = above_app_path.parent();
    try out.print("Enumerating: \"{s}\"\n", .{above_app_path.get()});

    // Show all the entries.
    const items = try sdl3.filesystem.getAllDirectoryItems(allocator, above_app_path.get());
    defer sdl3.filesystem.freeAllDirectoryItems(allocator, items);
    for (items.items) |item| {
        try out.print("Found: \"{s}\"\n", .{item});
    }

    try stdout_file_writer.interface.flush();
}
