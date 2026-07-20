const std = @import("std");
const sdl = @import("../sdl.zig");
const TextRenderer = @import("text_renderer.zig").TextRenderer;

pub const WIDTH = 380;
pub const HEIGHT = 480;

const background = 0x0A0F14;
const header_background = 0x0F171D;
const panel_background = 0x111A21;
const panel_border = 0x1B2A33;
const text_color = 0xE2ECE7;
const muted_color = 0x71848B;
const amber = 0xF4B860;

/// Presentation-only machine state copied from the deterministic core.
pub const State = struct {
    pc: u16 = 0,
    sp: u16 = 0,
    af: u16 = 0,
    bc: u16 = 0,
    de: u16 = 0,
    hl: u16 = 0,
    cycles: u64 = 0,
    frames: usize = 0,
    upper_rom_bank: u16 = 0,
    effective_ram_bank: usize = 0,
    fps_x100: u32 = 0,
    mnemonic: []const u8 = "nop",
};

pub const DebuggerWindow = struct {
    window: *sdl.Window,
    renderer: *sdl.Renderer,
    text: TextRenderer,
    window_id: u32,
    visible: bool = false,

    pub fn init(x: c_int, y: c_int) !DebuggerWindow {
        const window = try sdl.createWindow(
            "Nibble Debugger",
            x,
            y,
            WIDTH,
            HEIGHT,
            sdl.WINDOW_HIDDEN | sdl.WINDOW_RESIZABLE | sdl.WINDOW_ALLOW_HIGHDPI,
        );
        errdefer sdl.destroyWindow(window);
        sdl.setWindowMinimumSize(window, 340, 430);

        // The game renderer owns frame pacing. Waiting on two vsync renderers
        // can halve throughput on some SDL backends.
        const renderer = try sdl.createRenderer(window, -1, sdl.RENDERER_ACCELERATED);
        errdefer sdl.destroyRenderer(renderer);
        try sdl.setLogicalSize(renderer, WIDTH, HEIGHT);

        var text = try TextRenderer.init(renderer);
        errdefer text.deinit();

        return .{
            .window = window,
            .renderer = renderer,
            .text = text,
            .window_id = sdl.getWindowId(window),
        };
    }

    pub fn deinit(self: *DebuggerWindow) void {
        self.text.deinit();
        sdl.destroyRenderer(self.renderer);
        sdl.destroyWindow(self.window);
    }

    pub fn setVisible(self: *DebuggerWindow, visible: bool) void {
        if (self.visible == visible) return;
        self.visible = visible;
        if (visible) {
            sdl.showWindow(self.window);
            sdl.raiseWindow(self.window);
        } else {
            sdl.hideWindow(self.window);
        }
    }

    pub fn present(self: *DebuggerWindow, state: State, paused: bool, accent: u32) void {
        if (!self.visible) return;

        setColor(self.renderer, background);
        sdl.renderClear(self.renderer) catch {};

        fill(self.renderer, .{ .x = 0, .y = 0, .w = WIDTH, .h = 72 }, header_background);
        fill(self.renderer, .{ .x = 0, .y = 0, .w = 4, .h = 72 }, accent);
        self.label("NIBBLE DEBUGGER", 20, 31, 18, text_color, WIDTH - 20);
        self.label("LIVE HARDWARE STATE", 20, 53, 10, muted_color, WIDTH - 20);

        const status_background: u32 = if (paused) 0x3A2B16 else 0x173126;
        const status_color: u32 = if (paused) amber else accent;
        roundedFill(self.renderer, .{ .x = 288, .y = 19, .w = 72, .h = 28 }, 6, status_background);
        self.label(if (paused) "PAUSED" else "RUNNING", if (paused) 303 else 297, 38, 11, status_color, 354);

        self.card(.{ .x = 16, .y = 88, .w = 348, .h = 172 });
        self.label("PROCESSOR", 30, 113, 11, muted_color, 350);
        fill(self.renderer, .{ .x = 30, .y = 124, .w = 320, .h = 1 }, panel_border);

        self.label("PC", 30, 148, 10, muted_color, 100);
        self.formatted("{X:0>4}", .{state.pc}, 30, 177, 23, text_color, 110);
        self.label("SP", 124, 148, 10, muted_color, 185);
        self.formatted("{X:0>4}", .{state.sp}, 124, 172, 16, text_color, 192);
        self.label("NEXT", 214, 148, 10, muted_color, 350);
        self.label(state.mnemonic, 214, 172, 15, accent, 350);

        const register_x = [_]c_int{ 30, 112, 194, 276 };
        const register_names = [_][]const u8{ "AF", "BC", "DE", "HL" };
        const register_values = [_]u16{ state.af, state.bc, state.de, state.hl };
        for (register_x, register_names, register_values) |x, name, value| {
            self.label(name, x, 207, 10, muted_color, x + 70);
            self.formatted("{X:0>4}", .{value}, x, 235, 16, text_color, x + 72);
        }

        self.card(.{ .x = 16, .y = 276, .w = 168, .h = 112 });
        self.label("MAPPER", 30, 301, 11, muted_color, 170);
        self.label("ROM BANK", 30, 327, 9, muted_color, 100);
        self.formatted("{X:0>3}", .{state.upper_rom_bank}, 30, 355, 18, text_color, 100);
        self.label("RAM BANK", 108, 327, 9, muted_color, 170);
        self.formatted("{X:0>2}", .{state.effective_ram_bank}, 108, 355, 18, text_color, 170);

        self.card(.{ .x = 196, .y = 276, .w = 168, .h = 112 });
        self.label("TIMING", 210, 301, 11, muted_color, 350);
        self.label("FRAME", 210, 326, 9, muted_color, 350);
        self.formatted("{d}", .{state.frames}, 210, 347, 14, text_color, 350);
        self.label("DOT CLOCK", 210, 366, 9, muted_color, 350);
        self.formatted("{X:0>8}", .{@as(u32, @truncate(state.cycles))}, 274, 366, 11, text_color, 350);

        self.card(.{ .x = 16, .y = 404, .w = 348, .h = 60 });
        self.label("PRESENTATION", 30, 427, 9, muted_color, 145);
        self.formatted("{d}.{d:0>2} FPS", .{ state.fps_x100 / 100, state.fps_x100 % 100 }, 30, 452, 16, accent, 170);
        self.label("SINGLE STEP", 218, 427, 9, muted_color, 350);
        roundedFill(self.renderer, .{ .x = 218, .y = 437, .w = 126, .h = 19 }, 4, if (paused) 0x233A30 else 0x182229);
        self.label(if (paused) "F10  STEP" else "PAUSE TO ENABLE", 229, 451, 10, if (paused) accent else muted_color, 338);

        sdl.renderPresent(self.renderer);
    }

    fn card(self: *DebuggerWindow, rect: sdl.Rect) void {
        roundedFill(self.renderer, rect, 7, panel_border);
        roundedFill(self.renderer, .{
            .x = rect.x + 1,
            .y = rect.y + 1,
            .w = rect.w - 2,
            .h = rect.h - 2,
        }, 6, panel_background);
    }

    fn label(
        self: *const DebuggerWindow,
        value: []const u8,
        x: c_int,
        baseline_y: c_int,
        pixel_height: f32,
        color: u32,
        max_x: c_int,
    ) void {
        self.text.draw(self.renderer, value, x, baseline_y, pixel_height, color, max_x);
    }

    fn formatted(
        self: *const DebuggerWindow,
        comptime format: []const u8,
        args: anytype,
        x: c_int,
        baseline_y: c_int,
        pixel_height: f32,
        color: u32,
        max_x: c_int,
    ) void {
        var buffer: [48]u8 = undefined;
        const value = std.fmt.bufPrint(&buffer, format, args) catch return;
        self.label(value, x, baseline_y, pixel_height, color, max_x);
    }
};

fn setColor(renderer: *sdl.Renderer, rgb: u32) void {
    sdl.setRenderDrawColor(
        renderer,
        @intCast((rgb >> 16) & 0xFF),
        @intCast((rgb >> 8) & 0xFF),
        @intCast(rgb & 0xFF),
        0xFF,
    ) catch {};
}

fn fill(renderer: *sdl.Renderer, rect: sdl.Rect, rgb: u32) void {
    setColor(renderer, rgb);
    sdl.fillRect(renderer, &rect) catch {};
}

/// SDL2's basic renderer has no rounded-rectangle primitive, so draw the arc
/// one horizontal strip at a time and fill the rectangular center.
fn roundedFill(renderer: *sdl.Renderer, rect: sdl.Rect, radius: c_int, rgb: u32) void {
    if (radius <= 0 or radius * 2 > rect.w or radius * 2 > rect.h) {
        fill(renderer, rect, rgb);
        return;
    }

    setColor(renderer, rgb);
    const radius_f: f32 = @floatFromInt(radius);
    var row: c_int = 0;
    while (row < radius) : (row += 1) {
        const distance: f32 = @as(f32, @floatFromInt(radius - row)) - 0.5;
        const inset: c_int = @intFromFloat(@ceil(radius_f - @sqrt(radius_f * radius_f - distance * distance)));
        const strip: sdl.Rect = .{
            .x = rect.x + inset,
            .y = rect.y + row,
            .w = rect.w - inset * 2,
            .h = 1,
        };
        sdl.fillRect(renderer, &strip) catch {};

        const mirrored = sdl.Rect{
            .x = strip.x,
            .y = rect.y + rect.h - row - 1,
            .w = strip.w,
            .h = 1,
        };
        sdl.fillRect(renderer, &mirrored) catch {};
    }

    const center: sdl.Rect = .{
        .x = rect.x,
        .y = rect.y + radius,
        .w = rect.w,
        .h = rect.h - radius * 2,
    };
    sdl.fillRect(renderer, &center) catch {};
}
