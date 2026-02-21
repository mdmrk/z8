const c = @import("c");
const events = @import("events.zig");
const root = @import("root");
const std = @import("std");

comptime {
    _ = @cImport({
        @cDefine("SDL_MAIN_USE_CALLBACKS", {});
        @cInclude("SDL3/SDL_main.h");
    });
}

/// This will be called once before anything else.
/// The argc/argv work like they always do.
/// If this returns `sdl3.AppResult.run`, the app runs.
/// If it returns `sdl3.AppResult.failure`, the app calls `SDL_AppQuit()` and terminates with an exit code that reports an error to the platform.
/// If it returns `sdl3.AppResult.success`, the app calls `SDL_AppQuit()` and terminates with an exit code that reports success to the platform.
/// This function should not go into an infinite mainloop; it should do any one-time startup it requires and then return.
///
/// If you want to, you can assign a pointer to `app_state`, and this pointer will be made available to you in later functions calls in their appstate parameter.
/// This allows you to avoid global variables, but is totally optional.
/// If you don't set this, the pointer will be `null` in later function calls.
///
/// App-implemented initial entry point for main callback apps.
///
/// ## Function Parameters
/// * `app_state`: A place where the app can optionally store a pointer for future use.
/// * `arg_count`: The standard ANSI C main's argc; number of elements in `arg_values`.
/// * `arg_values`: The standard ANSI C main's argv; array of command line arguments.
///
/// ## Return Value
/// Returns `sdl3.AppResult.failure` to terminate with an error, `sdl3.AppResult.success` to terminate with success, `sdl3.AppResult.run` to continue.
///
/// ## Remarks
/// Apps implement this function when using main callbacks.
/// If using a standard "main" function, you should not supply this.
///
/// This function is called by SDL once, at startup.
/// The function should initialize whatever is necessary, possibly create windows and open audio devices, etc.
/// The argc and argv parameters work like they would with a standard "main" function.
///
/// This function should not go into an infinite mainloop; it should do any one-time setup it requires and then return.
///
/// The app may optionally assign a pointer to `app_state`.
/// This pointer will be provided on every future call to the other entry points,
/// to allow application state to be preserved between functions without the app needing to use a global variable.
/// If this isn't set, the pointer will be `null` in future entry points.
///
/// If this function returns `sdl3.AppResult.run`, the app will proceed to normal operation,
/// and will begin receiving repeated calls to `SDL_AppIterate()` and `SDL_AppEvent()` for the life of the program.
/// If this function returns `sdl3.AppResult.failure`,
/// SDL will call `SDL_AppQuit()` and terminate the process with an exit code that reports an error to the platform.
/// If it returns `sdl3.AppResult.success`, SDL calls `SDL_AppQuit()` and terminates with an exit code that reports success to the platform.
///
/// This function is called by SDL on the main thread.
///
/// ## Version
/// This function is available since SDL 3.2.0.
pub export fn SDL_AppInit(
    app_state: *?*anyopaque,
    arg_count: c_int,
    arg_values: [*][*:0]u8,
) callconv(.c) c.SDL_AppResult {
    if (@hasDecl(root, "init")) {
        const ret = root.init(@alignCast(@ptrCast(app_state)), arg_values[0..@intCast(arg_count)]) catch |err| {
            std.log.err("{s}", .{@errorName(err)});
            if (@errorReturnTrace()) |trace| {
                std.debug.dumpStackTrace(trace.*);
            }
            return c.SDL_APP_FAILURE;
        };
        return @intFromEnum(ret);
    } else return c.SDL_APP_CONTINUE;
}

/// This is called over and over, possibly at the refresh rate of the display or some other metric that the platform dictates.
/// This is where the heart of your app runs.
/// It should return as quickly as reasonably possible, but it's not a "run one memcpy and that's all the time you have" sort of thing.
/// The app should do any game updates, and render a frame of video.
/// If it returns `sdl3.AppResult.failure`, SDL will call `SDL_AppQuit()` and terminate the process with an exit code that reports an error to the platform.
/// If it returns `sdl3.AppResult.success`, the app calls `SDL_AppQuit()` and terminates with an exit code that reports success to the platform.
/// If it returns `sdl3.AppResult.run`, then `SDL_AppIterate()` will be called again at some regular frequency.
/// The platform may choose to run this more or less (perhaps less in the background, etc), or it might just call this function in a loop as fast as possible.
/// You do not check the event queue in this function (`SDL_AppEvent()` exists for that).
///
/// App-implemented iteration entry point for main callbacks apps.
///
/// ## Function Parameters
/// * `app_state`: An optional pointer, provided by the app in `SDL_AppInit()`.
///
/// ## Return Value
/// Returns `sdl3.AppResult.failure` to terminate with an error, `sdl3.AppResult.success` to terminate with success, `sdl3.AppResult.run` to continue.
///
/// ## Remarks
/// Apps implement this function when using main callbacks.
/// If using a standard "main" function, you should not supply this.
///
/// This function is called repeatedly by SDL after `SDL_AppInit()` returns `0`.
/// The function should operate as a single iteration the program's primary loop; it should update whatever state it needs and draw a new frame of video, usually.
///
/// On some platforms, this function will be called at the refresh rate of the display (which might change during the life of your app!).
/// There are no promises made about what frequency this function might run at.
/// You should use SDL's timer functions if you need to see how much time has passed since the last iteration.
///
/// There is no need to process the SDL event queue during this function; SDL will send events as they arrive in `SDL_AppEvent()`,
/// and in most cases the event queue will be empty when this function runs anyhow.
///
/// This function should not go into an infinite mainloop; it should do one iteration of whatever the program does and return.
///
/// The appstate parameter is an optional pointer provided by the app during `SDL_AppInit()`.
/// If the app never provided a pointer, this will be `null`.
///
/// If this function returns `sdl3.AppResult.run`, the app will continue normal operation,
/// receiving repeated calls to `SDL_AppIterate()` and `SDL_AppEvent()` for the life of the program.
/// If this function returns `sdl3.AppResult.failure`,
/// SDL will call `SDL_AppQuit() and terminate the process with an exit code that reports an error to the platform.
/// If it returns `sdl3.AppResult.success`, SDL calls `SDL_AppQuit()` and terminates with an exit code that reports success to the platform.
///
/// This function is called by SDL on the main thread.
///
/// ## Thread Safety
/// This function may get called concurrently with `SDL_AppEvent()` for events not pushed on the main thread.
///
/// ## Version
/// This function is available since SDL 3.2.0.
pub export fn SDL_AppIterate(
    app_state: ?*anyopaque,
) c.SDL_AppResult {
    if (@hasDecl(root, "iterate")) {
        const ret = root.iterate(@alignCast(@ptrCast(app_state))) catch |err| {
            std.log.err("{s}", .{@errorName(err)});
            if (@errorReturnTrace()) |trace| {
                std.debug.dumpStackTrace(trace.*);
            }
            return c.SDL_APP_FAILURE;
        };
        return @intFromEnum(ret);
    } else return c.SDL_APP_CONTINUE;
}

/// This will be called whenever an SDL event arrives.
/// Your app should not call `events.poll()`, `events.pump()`, etc, as SDL will manage all this for you.
/// Return values are the same as from `SDL_AppIterate()`, so you can terminate in response to `events.Type.quit`, etc.
///
/// App-implemented event entry point for main callbacks apps.
///
/// ## Function Parameters
/// * `app_state`: An optional pointer provided by the app in `SDL_AppInit()`.
/// * `event`: The new event for the app to examine.
///
/// ## Return Value
/// Returns `AppResult.failure` to terminate with an error, `AppResult.success` to terminate with success, `AppResult.run` to continue.
///
/// ## Remarks
/// Apps implement this function when using main callbacks.
/// If using a standard "main" function, you should not supply this.
///
/// This function is called as needed by SDL after `SDL_AppInit()` returns `AppResult.run`.
/// It is called once for each new event.
///
/// There is (currently) no guarantee about what thread this will be called from; whatever thread pushes an event onto SDL's queue will trigger this function.
/// SDL is responsible for pumping the event queue between each call to `SDL_AppIterate()`, so in normal operation one should only get events in a serial fashion,
/// but be careful if you have a thread that explicitly calls `events.push()` SDL itself will push events to the queue on the main thread.
///
/// Events sent to this function are not owned by the app; if you need to save the data, you should copy it.
///
/// This function should not go into an infinite mainloop; it should handle the provided event appropriately and return.
///
/// The appstate parameter is an optional pointer provided by the app during `SDL_AppInit()`.
/// If the app never provided a pointer, this will be `null`.
///
/// If this function returns `AppResult.run`, the app will continue normal operation,
/// receiving repeated calls to `SDL_AppIterate()` and `SDL_AppEvent()` for the life of the program.
/// If this function returns `AppResult.failure`, SDL will call `SDL_AppQuit()` and terminate the process with an exit code that reports an error to the platform.
/// If it returns `AppResult.success`, SDL calls `SDL_AppQuit()` and terminates with an exit code that reports success to the platform.
///
/// ## Thread Safety
/// This function may get called concurrently with `SDL_AppIterate()` or `SDL_AppQuit()` for events not pushed from the main thread.
///
/// ## Version
/// This function is available since SDL 3.2.0.
pub export fn SDL_AppEvent(
    app_state: ?*anyopaque,
    event: *c.SDL_Event,
) callconv(.c) c.SDL_AppResult {
    if (@hasDecl(root, "event")) {
        const ret = root.event(@alignCast(@ptrCast(app_state)), events.Event.fromSdl(event.*)) catch |err| {
            std.log.err("{s}", .{@errorName(err)});
            if (@errorReturnTrace()) |trace| {
                std.debug.dumpStackTrace(trace.*);
            }
            return c.SDL_APP_FAILURE;
        };
        return @intFromEnum(ret);
    } else return c.SDL_APP_CONTINUE;
}

/// This is called once before terminating the app--assuming the app isn't being forcibly killed or crashed--as a last chance to clean up.
/// After this returns, SDL will call `init.shutdown()` so the app doesn't have to (but it's safe for the app to call it, too).
/// Process termination proceeds as if the app returned normally from main(), so atexit handles will run, if your platform supports that.
///
/// If you set `app_state` during `SDL_AppInit()`, this is where you should free that data, as this pointer will not be provided to your app again.
///
/// The `SDL_AppResult` value that terminated the app is provided here, in case it's useful to know if this was a successful or failing run of the app.
///
/// App-implemented deinit entry point for main callbacks apps.
///
/// ## Function Parameters
/// * `app_state`: An optional pointer, provided by the app in `SDL_AppInit()`.
/// * `result`: The result code that terminated the app (success or failure).
///
/// ## Remarks
/// Apps implement this function when using main callbacks.
/// If using a standard "main" function, you should not supply this.
///
/// This function is called once by SDL before terminating the program.
///
/// This function will be called no matter what, even if `SDL_AppInit()` requests termination.
///
/// This function should not go into an infinite mainloop; it should deinitialize any resources necessary, perform whatever shutdown activities, and return.
///
/// You do not need to call `SDL_Quit()` in this function, as SDL will call it after this function returns and before the process terminates,
/// but it is safe to do so.
///
/// The appstate parameter is an optional pointer provided by the app during `SDL_AppInit()`.
/// If the app never provided a pointer, this will be `null`.
/// This function call is the last time this pointer will be provided, so any resources to it should be cleaned up here.
///
/// This function is called by SDL on the main thread.
///
/// ## Thread Safety
/// SDL_AppEvent() may get called concurrently with this function if other threads that push events are still active.
///
/// ## Version
/// This function is available since SDL 3.2.0.
pub export fn SDL_AppQuit(
    app_state: ?*anyopaque,
    result: c.SDL_AppResult,
) callconv(.c) void {
    if (@hasDecl(root, "quit")) {
        root.quit(@alignCast(@ptrCast(app_state)), @enumFromInt(result));
    }
}
