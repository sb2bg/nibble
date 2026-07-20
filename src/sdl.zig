// SDL2 bindings for Zig - avoids @cImport issues with ARM NEON headers
const std = @import("std");

// Opaque types
pub const Window = opaque {};
pub const Renderer = opaque {};
pub const Texture = opaque {};
pub const AudioDeviceId = u32;

pub const Rect = extern struct {
    x: c_int,
    y: c_int,
    w: c_int,
    h: c_int,
};

pub const WindowEvent = extern struct {
    type: u32,
    timestamp: u32,
    window_id: u32,
    event: u8,
    padding1: u8,
    padding2: u8,
    padding3: u8,
    data1: i32,
    data2: i32,
};

// SDL_Event is a 56-byte union on SDL2. The u64 member preserves the native
// union alignment required by event variants containing pointers.
pub const Event = extern union {
    type: u32,
    window: WindowEvent,
    padding: [56]u8,
    aligner: u64,
};

comptime {
    if (@sizeOf(Event) != 56) @compileError("SDL2 event ABI size changed");
    if (@offsetOf(WindowEvent, "window_id") != 8) @compileError("SDL2 window event ABI changed");
}

pub const AudioSpec = extern struct {
    freq: c_int,
    format: u16,
    channels: u8,
    silence: u8,
    samples: u16,
    padding: u16,
    size: u32,
    callback: ?*const anyopaque,
    userdata: ?*anyopaque,
};

// Event types
pub const QUIT = 0x100;
pub const WINDOWEVENT = 0x200;
pub const KEYDOWN = 0x300;
pub const KEYUP = 0x301;
pub const WINDOWEVENT_CLOSE: u8 = 14;

// Init flags
pub const INIT_VIDEO: u32 = 0x00000020;
pub const INIT_AUDIO: u32 = 0x00000010;

// Native-endian signed 16-bit PCM. Nibble currently targets SDL2 platforms
// supported by Zig where the host is little-endian.
pub const AUDIO_S16LSB: u16 = 0x8010;

// Window flags
pub const WINDOW_SHOWN: u32 = 0x00000004;
pub const WINDOW_RESIZABLE: u32 = 0x00000020;
pub const WINDOW_ALLOW_HIGHDPI: u32 = 0x00002000;
pub const WINDOW_FULLSCREEN_DESKTOP: u32 = 0x00001001;

// Renderer flags
pub const RENDERER_ACCELERATED: u32 = 0x00000002;
pub const RENDERER_PRESENTVSYNC: u32 = 0x00000004;

// Texture access
pub const TEXTUREACCESS_STREAMING: c_int = 1;

// Pixel formats
pub const PIXELFORMAT_ARGB8888: u32 = 0x16362004;
pub const PIXELFORMAT_RGB888: u32 = 0x16161804;

// Window position
pub const WINDOWPOS_CENTERED: c_int = 0x2FFF0000;

// SDL functions
extern fn SDL_Init(flags: u32) c_int;
extern fn SDL_InitSubSystem(flags: u32) c_int;
extern fn SDL_Quit() void;
extern fn SDL_GetError() [*:0]const u8;

extern fn SDL_CreateWindow(title: [*:0]const u8, x: c_int, y: c_int, w: c_int, h: c_int, flags: u32) ?*Window;
extern fn SDL_DestroyWindow(window: *Window) void;
extern fn SDL_SetWindowTitle(window: *Window, title: [*:0]const u8) void;
extern fn SDL_SetWindowMinimumSize(window: *Window, min_w: c_int, min_h: c_int) void;
extern fn SDL_SetWindowFullscreen(window: *Window, flags: u32) c_int;
extern fn SDL_GetWindowID(window: *Window) u32;
extern fn SDL_GetWindowPosition(window: *Window, x: *c_int, y: *c_int) void;
extern fn SDL_SetWindowPosition(window: *Window, x: c_int, y: c_int) void;
extern fn SDL_ShowWindow(window: *Window) void;
extern fn SDL_HideWindow(window: *Window) void;
extern fn SDL_RaiseWindow(window: *Window) void;

extern fn SDL_CreateRenderer(window: *Window, index: c_int, flags: u32) ?*Renderer;
extern fn SDL_DestroyRenderer(renderer: *Renderer) void;
extern fn SDL_RenderSetLogicalSize(renderer: *Renderer, w: c_int, h: c_int) c_int;
extern fn SDL_SetRenderDrawColor(renderer: *Renderer, r: u8, g: u8, b: u8, a: u8) c_int;
extern fn SDL_RenderFillRect(renderer: *Renderer, rect: *const Rect) c_int;

extern fn SDL_CreateTexture(renderer: *Renderer, format: u32, access: c_int, w: c_int, h: c_int) ?*Texture;
extern fn SDL_DestroyTexture(texture: *Texture) void;
extern fn SDL_UpdateTexture(texture: *Texture, rect: ?*const anyopaque, pixels: [*]const u8, pitch: c_int) c_int;
extern fn SDL_RenderCopy(renderer: *Renderer, texture: *Texture, srcrect: ?*const anyopaque, dstrect: ?*const anyopaque) c_int;
extern fn SDL_RenderPresent(renderer: *Renderer) void;
extern fn SDL_RenderClear(renderer: *Renderer) c_int;

extern fn SDL_PollEvent(event: *Event) c_int;
extern fn SDL_GetKeyboardState(numkeys: ?*c_int) [*c]const u8;
extern fn SDL_PumpEvents() void;
extern fn SDL_SetHint(name: [*:0]const u8, value: [*:0]const u8) c_int;

extern fn SDL_OpenAudioDevice(
    device: ?[*:0]const u8,
    iscapture: c_int,
    desired: *const AudioSpec,
    obtained: ?*AudioSpec,
    allowed_changes: c_int,
) AudioDeviceId;
extern fn SDL_CloseAudioDevice(device: AudioDeviceId) void;
extern fn SDL_PauseAudioDevice(device: AudioDeviceId, pause_on: c_int) void;
extern fn SDL_QueueAudio(device: AudioDeviceId, data: *const anyopaque, len: u32) c_int;
extern fn SDL_GetQueuedAudioSize(device: AudioDeviceId) u32;
extern fn SDL_ClearQueuedAudio(device: AudioDeviceId) void;

// Keyboard scancodes we map to DMG controls
pub const SCANCODE_X: usize = 27; // A
pub const SCANCODE_Z: usize = 29; // B
pub const SCANCODE_A: usize = 4; // A (alt)
pub const SCANCODE_S: usize = 22; // B (alt)
pub const SCANCODE_RETURN: usize = 40; // Start
pub const SCANCODE_KP_ENTER: usize = 88; // Start (alt)
pub const SCANCODE_SPACE: usize = 44; // Start (alt)
pub const SCANCODE_BACKSPACE: usize = 42; // Select
pub const SCANCODE_TAB: usize = 43; // Select (alt)
pub const SCANCODE_RIGHT: usize = 79;
pub const SCANCODE_LEFT: usize = 80;
pub const SCANCODE_DOWN: usize = 81;
pub const SCANCODE_UP: usize = 82;
pub const SCANCODE_ESCAPE: usize = 41;
pub const SCANCODE_P: usize = 19;
pub const SCANCODE_R: usize = 21;
pub const SCANCODE_LEFTBRACKET: usize = 47;
pub const SCANCODE_RIGHTBRACKET: usize = 48;
pub const SCANCODE_F5: usize = 62;
pub const SCANCODE_F9: usize = 66;
pub const SCANCODE_F10: usize = 67;
pub const SCANCODE_F11: usize = 68;
pub const SCANCODE_F1: usize = 58;
pub const SCANCODE_C: usize = 6;
pub const SCANCODE_M: usize = 16;

// Zig-friendly wrappers
pub fn init(flags: u32) !void {
    if (SDL_Init(flags) < 0) {
        return error.SdlInitFailed;
    }
}

pub fn quit() void {
    SDL_Quit();
}

pub fn initSubSystem(flags: u32) !void {
    if (SDL_InitSubSystem(flags) < 0) return error.SdlSubSystemInitFailed;
}

pub fn getError() [:0]const u8 {
    return std.mem.span(SDL_GetError());
}

pub fn createWindow(title: [:0]const u8, x: c_int, y: c_int, w: c_int, h: c_int, flags: u32) !*Window {
    return SDL_CreateWindow(title.ptr, x, y, w, h, flags) orelse error.SdlWindowCreationFailed;
}

pub fn destroyWindow(window: *Window) void {
    SDL_DestroyWindow(window);
}

pub fn setWindowTitle(window: *Window, title: [:0]const u8) void {
    SDL_SetWindowTitle(window, title.ptr);
}

pub fn setWindowMinimumSize(window: *Window, width: c_int, height: c_int) void {
    SDL_SetWindowMinimumSize(window, width, height);
}

pub fn setWindowFullscreen(window: *Window, fullscreen: bool) !void {
    const flags: u32 = if (fullscreen) WINDOW_FULLSCREEN_DESKTOP else 0;
    if (SDL_SetWindowFullscreen(window, flags) < 0) return error.SdlFullscreenFailed;
}

pub fn getWindowId(window: *Window) u32 {
    return SDL_GetWindowID(window);
}

pub fn getWindowPosition(window: *Window) struct { x: c_int, y: c_int } {
    var x: c_int = 0;
    var y: c_int = 0;
    SDL_GetWindowPosition(window, &x, &y);
    return .{ .x = x, .y = y };
}

pub fn setWindowPosition(window: *Window, x: c_int, y: c_int) void {
    SDL_SetWindowPosition(window, x, y);
}

pub fn showWindow(window: *Window) void {
    SDL_ShowWindow(window);
}

pub fn hideWindow(window: *Window) void {
    SDL_HideWindow(window);
}

pub fn raiseWindow(window: *Window) void {
    SDL_RaiseWindow(window);
}

pub fn createRenderer(window: *Window, index: c_int, flags: u32) !*Renderer {
    return SDL_CreateRenderer(window, index, flags) orelse error.SdlRendererCreationFailed;
}

pub fn destroyRenderer(renderer: *Renderer) void {
    SDL_DestroyRenderer(renderer);
}

pub fn setLogicalSize(renderer: *Renderer, width: c_int, height: c_int) !void {
    if (SDL_RenderSetLogicalSize(renderer, width, height) < 0) return error.SdlLogicalSizeFailed;
}

pub fn setRenderDrawColor(renderer: *Renderer, red: u8, green: u8, blue: u8, alpha: u8) !void {
    if (SDL_SetRenderDrawColor(renderer, red, green, blue, alpha) < 0) {
        return error.SdlRenderColorFailed;
    }
}

pub fn fillRect(renderer: *Renderer, rect: *const Rect) !void {
    if (SDL_RenderFillRect(renderer, rect) < 0) return error.SdlRenderFillFailed;
}

pub fn createTexture(renderer: *Renderer, format: u32, access: c_int, w: c_int, h: c_int) !*Texture {
    return SDL_CreateTexture(renderer, format, access, w, h) orelse error.SdlTextureCreationFailed;
}

pub fn destroyTexture(texture: *Texture) void {
    SDL_DestroyTexture(texture);
}

pub fn updateTexture(texture: *Texture, rect: ?*const anyopaque, pixels: []const u8, pitch: c_int) !void {
    if (SDL_UpdateTexture(texture, rect, pixels.ptr, pitch) < 0) {
        return error.SdlUpdateTextureFailed;
    }
}

pub fn renderCopy(renderer: *Renderer, texture: *Texture, srcrect: ?*const anyopaque, dstrect: ?*const anyopaque) !void {
    if (SDL_RenderCopy(renderer, texture, srcrect, dstrect) < 0) {
        return error.SdlRenderCopyFailed;
    }
}

pub fn renderPresent(renderer: *Renderer) void {
    SDL_RenderPresent(renderer);
}

pub fn renderClear(renderer: *Renderer) !void {
    if (SDL_RenderClear(renderer) < 0) {
        return error.SdlRenderClearFailed;
    }
}

pub fn pollEvent(event: *Event) bool {
    return SDL_PollEvent(event) != 0;
}

pub fn pumpEvents() void {
    SDL_PumpEvents();
}

pub fn getKeyboardState() []const u8 {
    var key_count: c_int = 0;
    const ptr = SDL_GetKeyboardState(&key_count);
    if (ptr == null or key_count <= 0) return &[_]u8{};
    return ptr[0..@intCast(key_count)];
}

pub fn setHint(name: [:0]const u8, value: [:0]const u8) bool {
    return SDL_SetHint(name.ptr, value.ptr) != 0;
}

pub fn openAudioDevice(desired: *const AudioSpec) !AudioDeviceId {
    const device = SDL_OpenAudioDevice(null, 0, desired, null, 0);
    return if (device == 0) error.SdlAudioDeviceFailed else device;
}

pub fn closeAudioDevice(device: AudioDeviceId) void {
    SDL_CloseAudioDevice(device);
}

pub fn pauseAudioDevice(device: AudioDeviceId, paused: bool) void {
    SDL_PauseAudioDevice(device, @intFromBool(paused));
}

pub fn queueAudio(device: AudioDeviceId, bytes: []const u8) !void {
    if (bytes.len == 0) return;
    if (SDL_QueueAudio(device, bytes.ptr, @intCast(bytes.len)) < 0) {
        return error.SdlQueueAudioFailed;
    }
}

pub fn queuedAudioSize(device: AudioDeviceId) u32 {
    return SDL_GetQueuedAudioSize(device);
}

pub fn clearQueuedAudio(device: AudioDeviceId) void {
    SDL_ClearQueuedAudio(device);
}
