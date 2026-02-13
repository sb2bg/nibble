// SDL2 bindings for Zig - avoids @cImport issues with ARM NEON headers
const std = @import("std");

// Opaque types
pub const Window = opaque {};
pub const Renderer = opaque {};
pub const Texture = opaque {};

// SDL Event (simplified - 56 bytes to match SDL2's union size)
pub const Event = extern struct {
    type: u32,
    padding: [52]u8 = undefined,
};

// Event types
pub const QUIT = 0x100;
pub const KEYDOWN = 0x300;
pub const KEYUP = 0x301;

// Init flags
pub const INIT_VIDEO: u32 = 0x00000020;
pub const INIT_AUDIO: u32 = 0x00000010;

// Window flags
pub const WINDOW_SHOWN: u32 = 0x00000004;
pub const WINDOW_RESIZABLE: u32 = 0x00000020;

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
extern fn SDL_Quit() void;
extern fn SDL_GetError() [*:0]const u8;

extern fn SDL_CreateWindow(title: [*:0]const u8, x: c_int, y: c_int, w: c_int, h: c_int, flags: u32) ?*Window;
extern fn SDL_DestroyWindow(window: *Window) void;

extern fn SDL_CreateRenderer(window: *Window, index: c_int, flags: u32) ?*Renderer;
extern fn SDL_DestroyRenderer(renderer: *Renderer) void;

extern fn SDL_CreateTexture(renderer: *Renderer, format: u32, access: c_int, w: c_int, h: c_int) ?*Texture;
extern fn SDL_DestroyTexture(texture: *Texture) void;
extern fn SDL_UpdateTexture(texture: *Texture, rect: ?*const anyopaque, pixels: [*]const u8, pitch: c_int) c_int;
extern fn SDL_RenderCopy(renderer: *Renderer, texture: *Texture, srcrect: ?*const anyopaque, dstrect: ?*const anyopaque) c_int;
extern fn SDL_RenderPresent(renderer: *Renderer) void;
extern fn SDL_RenderClear(renderer: *Renderer) c_int;

extern fn SDL_PollEvent(event: *Event) c_int;

// Zig-friendly wrappers
pub fn init(flags: u32) !void {
    if (SDL_Init(flags) < 0) {
        return error.SdlInitFailed;
    }
}

pub fn quit() void {
    SDL_Quit();
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

pub fn createRenderer(window: *Window, index: c_int, flags: u32) !*Renderer {
    return SDL_CreateRenderer(window, index, flags) orelse error.SdlRendererCreationFailed;
}

pub fn destroyRenderer(renderer: *Renderer) void {
    SDL_DestroyRenderer(renderer);
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
