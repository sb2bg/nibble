const std = @import("std");
const sdl = @import("../sdl.zig");
const ppu_mod = @import("../ppu/ppu.zig");
const DmgColor = ppu_mod.DmgColor;
const apu_mod = @import("../apu.zig");
const StereoSample = apu_mod.StereoSample;
const SCREEN_WIDTH = ppu_mod.SCREEN_WIDTH;
const SCREEN_HEIGHT = ppu_mod.SCREEN_HEIGHT;

const WINDOW_SCALE = 3;
const VIEWPORT_X = 8;
const VIEWPORT_Y = 8;
const LOGICAL_WIDTH = SCREEN_WIDTH + VIEWPORT_X * 2;
const LOGICAL_HEIGHT = SCREEN_HEIGHT + VIEWPORT_Y * 2;
const INSPECTOR_WIDTH = 128;
const INSPECTOR_HEIGHT = 160;
const INSPECTOR_SCALE = 3;
const WINDOW_GAP = 24;
const AUDIO_BATCH_SAMPLES = 128;
const MAX_QUEUED_AUDIO_BYTES = apu_mod.SAMPLE_RATE * @sizeOf(StereoSample) / 10;

const Palette = struct {
    name: []const u8,
    colors: [4]u32,
};

const palettes = [_]Palette{
    .{ .name = "Classic", .colors = .{ 0xE0F8D0, 0x88C070, 0x346856, 0x081820 } },
    .{ .name = "Pocket", .colors = .{ 0xF8F8E8, 0xB8B8A0, 0x686858, 0x181810 } },
    .{ .name = "Mono", .colors = .{ 0xF4F4F4, 0xAAAAAA, 0x555555, 0x101010 } },
    .{ .name = "Amber", .colors = .{ 0xFFF1B8, 0xD99B42, 0x8A4B20, 0x26150D } },
};

pub const UiActions = struct {
    quit: bool = false,
    toggle_pause: bool = false,
    reset: bool = false,
    save_state: bool = false,
    load_state: bool = false,
    prev_slot: bool = false,
    next_slot: bool = false,
    toggle_mute: bool = false,
    step_instruction: bool = false,
    redraw: bool = false,
};

/// A copied, presentation-only view of the machine. Static mnemonic strings
/// are supplied by the decoder; all mutable emulator state remains in Machine.
pub const InspectorState = struct {
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

/// SDL owns host presentation and input. It deliberately has no authority
/// over PPU timing, which keeps the emulation core usable in tests/headless runs.
pub const SdlFrontend = struct {
    window: *sdl.Window,
    renderer: *sdl.Renderer,
    texture: *sdl.Texture,
    inspector_window: *sdl.Window,
    inspector_renderer: *sdl.Renderer,
    main_window_id: u32,
    inspector_window_id: u32,
    audio_device: ?sdl.AudioDeviceId = null,
    audio_muted: bool = false,

    ui_paused: bool = false,
    ui_slot: u8 = 0,
    ui_slot_has_state: bool = false,
    ui_message: [48]u8 = [_]u8{0} ** 48,
    ui_message_len: usize = 0,
    inspector: InspectorState = .{},
    inspector_visible: bool = true,

    prev_pause_key: bool = false,
    prev_reset_key: bool = false,
    prev_save_key: bool = false,
    prev_load_key: bool = false,
    prev_prev_slot_key: bool = false,
    prev_next_slot_key: bool = false,
    prev_quit_key: bool = false,
    prev_palette_key: bool = false,
    prev_fullscreen_key: bool = false,
    prev_mute_key: bool = false,
    prev_inspector_key: bool = false,
    prev_step_key: bool = false,
    palette_index: usize = 0,
    fullscreen: bool = false,

    pub fn init() !SdlFrontend {
        sdl.init(sdl.INIT_VIDEO) catch {
            std.debug.print("SDL_Init Error: {s}\n", .{sdl.getError()});
            return error.SdlInitFailed;
        };
        errdefer sdl.quit();

        _ = sdl.setHint("SDL_RENDER_SCALE_QUALITY", "nearest");

        const window = sdl.createWindow(
            "Nibble",
            sdl.WINDOWPOS_CENTERED,
            sdl.WINDOWPOS_CENTERED,
            LOGICAL_WIDTH * WINDOW_SCALE,
            LOGICAL_HEIGHT * WINDOW_SCALE,
            sdl.WINDOW_SHOWN | sdl.WINDOW_RESIZABLE | sdl.WINDOW_ALLOW_HIGHDPI,
        ) catch {
            std.debug.print("SDL_CreateWindow Error: {s}\n", .{sdl.getError()});
            return error.SdlWindowFailed;
        };
        errdefer sdl.destroyWindow(window);
        sdl.setWindowMinimumSize(window, LOGICAL_WIDTH * 2, LOGICAL_HEIGHT * 2);

        const renderer = sdl.createRenderer(
            window,
            -1,
            sdl.RENDERER_ACCELERATED | sdl.RENDERER_PRESENTVSYNC,
        ) catch sdl.createRenderer(window, -1, sdl.RENDERER_ACCELERATED) catch {
            std.debug.print("SDL_CreateRenderer Error: {s}\n", .{sdl.getError()});
            return error.SdlRendererFailed;
        };
        errdefer sdl.destroyRenderer(renderer);
        try sdl.setLogicalSize(renderer, LOGICAL_WIDTH, LOGICAL_HEIGHT);
        try sdl.setRenderDrawColor(renderer, 0x0B, 0x0F, 0x12, 0xFF);

        const texture = sdl.createTexture(
            renderer,
            sdl.PIXELFORMAT_RGB888,
            sdl.TEXTUREACCESS_STREAMING,
            SCREEN_WIDTH,
            SCREEN_HEIGHT,
        ) catch {
            std.debug.print("SDL_CreateTexture Error: {s}\n", .{sdl.getError()});
            return error.SdlTextureFailed;
        };
        errdefer sdl.destroyTexture(texture);

        const inspector_window = sdl.createWindow(
            "Nibble Debugger",
            sdl.WINDOWPOS_CENTERED,
            sdl.WINDOWPOS_CENTERED,
            INSPECTOR_WIDTH * INSPECTOR_SCALE,
            INSPECTOR_HEIGHT * INSPECTOR_SCALE,
            sdl.WINDOW_SHOWN | sdl.WINDOW_RESIZABLE | sdl.WINDOW_ALLOW_HIGHDPI,
        ) catch {
            std.debug.print("SDL_CreateWindow Error: {s}\n", .{sdl.getError()});
            return error.SdlInspectorWindowFailed;
        };
        errdefer sdl.destroyWindow(inspector_window);
        sdl.setWindowMinimumSize(inspector_window, INSPECTOR_WIDTH * 2, INSPECTOR_HEIGHT * 2);

        // The game renderer owns frame pacing. A second vsync renderer could
        // make every presented frame wait twice on some SDL backends.
        const inspector_renderer = sdl.createRenderer(
            inspector_window,
            -1,
            sdl.RENDERER_ACCELERATED,
        ) catch {
            std.debug.print("SDL_CreateRenderer Error: {s}\n", .{sdl.getError()});
            return error.SdlInspectorRendererFailed;
        };
        errdefer sdl.destroyRenderer(inspector_renderer);
        try sdl.setLogicalSize(inspector_renderer, INSPECTOR_WIDTH, INSPECTOR_HEIGHT);
        try sdl.setRenderDrawColor(inspector_renderer, 0x0B, 0x0F, 0x12, 0xFF);

        const main_position = sdl.getWindowPosition(window);
        sdl.setWindowPosition(
            inspector_window,
            main_position.x + LOGICAL_WIDTH * WINDOW_SCALE + WINDOW_GAP,
            main_position.y,
        );

        const audio_device = initAudio() catch |err| blk: {
            std.debug.print("Warning: SDL audio unavailable ({s}): {s}\n", .{ @errorName(err), sdl.getError() });
            break :blk null;
        };

        var frontend: SdlFrontend = .{
            .window = window,
            .renderer = renderer,
            .texture = texture,
            .inspector_window = inspector_window,
            .inspector_renderer = inspector_renderer,
            .main_window_id = sdl.getWindowId(window),
            .inspector_window_id = sdl.getWindowId(inspector_window),
            .audio_device = audio_device,
        };
        frontend.refreshWindowTitle();
        return frontend;
    }

    pub fn deinit(self: *SdlFrontend) void {
        if (self.audio_device) |device| sdl.closeAudioDevice(device);
        sdl.destroyRenderer(self.inspector_renderer);
        sdl.destroyWindow(self.inspector_window);
        sdl.destroyTexture(self.texture);
        sdl.destroyRenderer(self.renderer);
        sdl.destroyWindow(self.window);
        sdl.quit();
    }

    pub fn present(self: *SdlFrontend, frame: *const [SCREEN_HEIGHT][SCREEN_WIDTH]DmgColor) void {
        var pixels: [SCREEN_HEIGHT * SCREEN_WIDTH * 4]u8 =
            [_]u8{0} ** (SCREEN_HEIGHT * SCREEN_WIDTH * 4);

        for (0..SCREEN_HEIGHT) |y| {
            for (0..SCREEN_WIDTH) |x| {
                const offset = (y * SCREEN_WIDTH + x) * 4;
                const rgb = self.toRgb(frame[y][x]);
                pixels[offset + 0] = @intCast((rgb >> 16) & 0xFF);
                pixels[offset + 1] = @intCast((rgb >> 8) & 0xFF);
                pixels[offset + 2] = @intCast(rgb & 0xFF);
                pixels[offset + 3] = 0xFF;
            }
        }

        sdl.updateTexture(self.texture, null, pixels[0..], SCREEN_WIDTH * 4) catch {};
        sdl.setRenderDrawColor(self.renderer, 0x08, 0x0D, 0x11, 0xFF) catch {};
        sdl.renderClear(self.renderer) catch {};
        const viewport: sdl.Rect = .{
            .x = VIEWPORT_X,
            .y = VIEWPORT_Y,
            .w = SCREEN_WIDTH,
            .h = SCREEN_HEIGHT,
        };
        sdl.renderCopy(self.renderer, self.texture, null, &viewport) catch {};
        sdl.renderPresent(self.renderer);
        if (self.inspector_visible) self.drawInspector();
    }

    pub fn pollEvents(self: *SdlFrontend, bus: anytype) UiActions {
        var actions: UiActions = .{};

        var event: sdl.Event = undefined;
        while (sdl.pollEvent(&event)) {
            switch (event.type) {
                sdl.QUIT => actions.quit = true,
                sdl.WINDOWEVENT => if (event.window.event == sdl.WINDOWEVENT_CLOSE) {
                    if (event.window.window_id == self.inspector_window_id) {
                        self.setInspectorVisible(false);
                    } else if (event.window.window_id == self.main_window_id) {
                        actions.quit = true;
                    }
                },
                else => {},
            }
        }

        sdl.pumpEvents();
        const keys = sdl.getKeyboardState();

        var state: u8 = 0xFF;
        if (keys.len != 0) {
            if (isPressed(keys, sdl.SCANCODE_RIGHT)) state &= ~@as(u8, 0x01);
            if (isPressed(keys, sdl.SCANCODE_LEFT)) state &= ~@as(u8, 0x02);
            if (isPressed(keys, sdl.SCANCODE_UP)) state &= ~@as(u8, 0x04);
            if (isPressed(keys, sdl.SCANCODE_DOWN)) state &= ~@as(u8, 0x08);
            if (isPressed(keys, sdl.SCANCODE_X) or isPressed(keys, sdl.SCANCODE_A)) state &= ~@as(u8, 0x10);
            if (isPressed(keys, sdl.SCANCODE_Z) or isPressed(keys, sdl.SCANCODE_S)) state &= ~@as(u8, 0x20);
            if (isPressed(keys, sdl.SCANCODE_BACKSPACE) or isPressed(keys, sdl.SCANCODE_TAB)) state &= ~@as(u8, 0x40);
            if (isPressed(keys, sdl.SCANCODE_RETURN) or isPressed(keys, sdl.SCANCODE_KP_ENTER) or isPressed(keys, sdl.SCANCODE_SPACE)) state &= ~@as(u8, 0x80);
        }
        bus.io.setJoypadState(state);

        if (keys.len != 0) {
            actions.quit = actions.quit or edgePressed(&self.prev_quit_key, isPressed(keys, sdl.SCANCODE_ESCAPE));
            actions.toggle_pause = edgePressed(&self.prev_pause_key, isPressed(keys, sdl.SCANCODE_P));
            actions.reset = edgePressed(&self.prev_reset_key, isPressed(keys, sdl.SCANCODE_R));
            actions.save_state = edgePressed(&self.prev_save_key, isPressed(keys, sdl.SCANCODE_F5));
            actions.load_state = edgePressed(&self.prev_load_key, isPressed(keys, sdl.SCANCODE_F9));
            actions.prev_slot = edgePressed(&self.prev_prev_slot_key, isPressed(keys, sdl.SCANCODE_LEFTBRACKET));
            actions.next_slot = edgePressed(&self.prev_next_slot_key, isPressed(keys, sdl.SCANCODE_RIGHTBRACKET));
            actions.toggle_mute = edgePressed(&self.prev_mute_key, isPressed(keys, sdl.SCANCODE_M));
            actions.step_instruction = edgePressed(&self.prev_step_key, isPressed(keys, sdl.SCANCODE_F10));

            if (edgePressed(&self.prev_inspector_key, isPressed(keys, sdl.SCANCODE_F1))) {
                self.setInspectorVisible(!self.inspector_visible);
                actions.redraw = true;
            }

            if (edgePressed(&self.prev_palette_key, isPressed(keys, sdl.SCANCODE_C))) {
                self.palette_index = (self.palette_index + 1) % palettes.len;
                self.refreshWindowTitle();
                actions.redraw = true;
            }
            if (edgePressed(&self.prev_fullscreen_key, isPressed(keys, sdl.SCANCODE_F11))) {
                const next = !self.fullscreen;
                if (sdl.setWindowFullscreen(self.window, next)) |_| {
                    self.fullscreen = next;
                } else |_| {}
            }
        } else {
            self.prev_quit_key = false;
            self.prev_pause_key = false;
            self.prev_reset_key = false;
            self.prev_save_key = false;
            self.prev_load_key = false;
            self.prev_prev_slot_key = false;
            self.prev_next_slot_key = false;
            self.prev_palette_key = false;
            self.prev_fullscreen_key = false;
            self.prev_mute_key = false;
            self.prev_inspector_key = false;
            self.prev_step_key = false;
        }

        return actions;
    }

    pub fn setUiStatus(
        self: *SdlFrontend,
        paused: bool,
        slot: u8,
        slot_has_state: bool,
        message: []const u8,
    ) void {
        const message_len = @min(message.len, self.ui_message.len);
        if (self.ui_paused == paused and
            self.ui_slot == slot and
            self.ui_slot_has_state == slot_has_state and
            std.mem.eql(u8, self.ui_message[0..self.ui_message_len], message[0..message_len]))
        {
            return;
        }

        self.ui_paused = paused;
        if (self.audio_device) |device| {
            if (paused) sdl.clearQueuedAudio(device);
            sdl.pauseAudioDevice(device, paused or self.audio_muted);
        }
        self.ui_slot = slot;
        self.ui_slot_has_state = slot_has_state;
        self.ui_message_len = message_len;
        if (self.ui_message_len > 0) {
            @memcpy(self.ui_message[0..self.ui_message_len], message[0..self.ui_message_len]);
        }
        self.refreshWindowTitle();
    }

    pub fn redraw(self: *SdlFrontend, frame: *const [SCREEN_HEIGHT][SCREEN_WIDTH]DmgColor) void {
        self.refreshWindowTitle();
        self.present(frame);
    }

    pub fn setInspector(self: *SdlFrontend, inspector: InspectorState) void {
        self.inspector = inspector;
    }

    fn setInspectorVisible(self: *SdlFrontend, visible: bool) void {
        if (self.inspector_visible == visible) return;
        self.inspector_visible = visible;
        if (visible) {
            sdl.showWindow(self.inspector_window);
            sdl.raiseWindow(self.inspector_window);
        } else {
            sdl.hideWindow(self.inspector_window);
        }
    }

    pub fn audioBatchReady(self: *const SdlFrontend, sample_count: usize) bool {
        _ = self;
        return sample_count >= AUDIO_BATCH_SAMPLES;
    }

    /// Queue emulated PCM without letting host stalls accumulate input lag.
    /// Dropping an overfull batch is preferable to playing stale audio later.
    pub fn queueAudio(self: *SdlFrontend, samples: []const StereoSample) void {
        const device = self.audio_device orelse return;
        if (self.audio_muted or self.ui_paused) return;
        if (sdl.queuedAudioSize(device) >= MAX_QUEUED_AUDIO_BYTES) return;
        sdl.queueAudio(device, std.mem.sliceAsBytes(samples)) catch {};
    }

    pub fn toggleAudioMute(self: *SdlFrontend) bool {
        self.audio_muted = !self.audio_muted;
        if (self.audio_device) |device| {
            sdl.clearQueuedAudio(device);
            sdl.pauseAudioDevice(device, self.audio_muted or self.ui_paused);
        }
        self.refreshWindowTitle();
        return self.audio_muted;
    }

    pub fn clearAudioQueue(self: *SdlFrontend) void {
        if (self.audio_device) |device| sdl.clearQueuedAudio(device);
    }

    fn refreshWindowTitle(self: *SdlFrontend) void {
        var title_buf: [192:0]u8 = undefined;
        const state = if (self.ui_paused) "Paused" else "Running";
        const slot_state = if (self.ui_slot_has_state) "set" else "empty";
        const audio_state = if (self.audio_device == null)
            "Audio unavailable"
        else if (self.audio_muted)
            "Audio muted"
        else
            "Audio on";
        const title = if (self.ui_message_len > 0)
            std.fmt.bufPrintZ(
                &title_buf,
                "Nibble | {s} | {s} | {s} | Slot {d} {s} | {s}",
                .{ state, palettes[self.palette_index].name, audio_state, self.ui_slot, slot_state, self.ui_message[0..self.ui_message_len] },
            ) catch "Nibble"
        else
            std.fmt.bufPrintZ(
                &title_buf,
                "Nibble | {s} | {s} | {s} | Slot {d} {s}",
                .{ state, palettes[self.palette_index].name, audio_state, self.ui_slot, slot_state },
            ) catch "Nibble";

        sdl.setWindowTitle(self.window, title);
    }

    fn drawInspector(self: *SdlFrontend) void {
        const renderer = self.inspector_renderer;
        sdl.setRenderDrawColor(renderer, 0x0B, 0x0F, 0x12, 0xFF) catch {};
        sdl.renderClear(renderer) catch {};

        const header: sdl.Rect = .{ .x = 0, .y = 0, .w = INSPECTOR_WIDTH, .h = 28 };
        sdl.setRenderDrawColor(renderer, 0x10, 0x1B, 0x21, 0xFF) catch {};
        sdl.fillRect(renderer, &header) catch {};

        const text = 0xD8E8E0;
        const muted = 0x7A918A;
        const accent = palettes[self.palette_index].colors[1];
        const accent_bar: sdl.Rect = .{ .x = 0, .y = 0, .w = 3, .h = 28 };
        sdl.setRenderDrawColor(
            renderer,
            @intCast((accent >> 16) & 0xFF),
            @intCast((accent >> 8) & 0xFF),
            @intCast(accent & 0xFF),
            0xFF,
        ) catch {};
        sdl.fillRect(renderer, &accent_bar) catch {};

        drawText(renderer, "NIBBLE DEBUGGER", 8, 7, accent);
        drawText(renderer, if (self.ui_paused) "PAUSED" else "RUNNING", 8, 17, if (self.ui_paused) 0xFFBE55 else text);

        var buf: [32]u8 = undefined;
        drawText(renderer, "CPU", 8, 34, muted);
        drawFormatted(renderer, &buf, "PC {X:0>4}", .{self.inspector.pc}, 8, 44, text);
        drawText(renderer, self.inspector.mnemonic, 72, 44, accent);
        drawFormatted(renderer, &buf, "AF {X:0>4}", .{self.inspector.af}, 8, 54, text);
        drawFormatted(renderer, &buf, "BC {X:0>4}", .{self.inspector.bc}, 72, 54, text);
        drawFormatted(renderer, &buf, "DE {X:0>4}", .{self.inspector.de}, 8, 64, text);
        drawFormatted(renderer, &buf, "HL {X:0>4}", .{self.inspector.hl}, 72, 64, text);
        drawFormatted(renderer, &buf, "SP {X:0>4}", .{self.inspector.sp}, 8, 74, text);

        drawText(renderer, "MAPPER", 8, 88, muted);
        drawFormatted(renderer, &buf, "ROM {X:0>3}", .{self.inspector.upper_rom_bank}, 8, 98, text);
        drawFormatted(renderer, &buf, "RAM {X:0>2}", .{self.inspector.effective_ram_bank}, 72, 98, text);

        drawText(renderer, "TIMING", 8, 112, muted);
        drawFormatted(renderer, &buf, "FRAME {d}", .{self.inspector.frames}, 8, 122, text);
        drawFormatted(renderer, &buf, "DOT {X:0>8}", .{@as(u32, @truncate(self.inspector.cycles))}, 8, 132, text);
        drawFormatted(renderer, &buf, "{d}.{d:0>2} FPS", .{ self.inspector.fps_x100 / 100, self.inspector.fps_x100 % 100 }, 8, 142, accent);
        drawText(renderer, "F10 STEP", 84, 150, if (self.ui_paused) accent else muted);
        sdl.renderPresent(renderer);
    }

    fn edgePressed(previous: *bool, current: bool) bool {
        const pressed = current and !previous.*;
        previous.* = current;
        return pressed;
    }

    fn isPressed(keys: []const u8, scancode: usize) bool {
        return scancode < keys.len and keys[scancode] != 0;
    }

    fn toRgb(self: *const SdlFrontend, color: DmgColor) u32 {
        return palettes[self.palette_index].colors[@intFromEnum(color)];
    }
};

fn drawFormatted(
    renderer: *sdl.Renderer,
    buffer: []u8,
    comptime format: []const u8,
    args: anytype,
    x: c_int,
    y: c_int,
    rgb: u32,
) void {
    const rendered = std.fmt.bufPrint(buffer, format, args) catch return;
    drawText(renderer, rendered, x, y, rgb);
}

/// Draw a dependency-free 3x5 pixel font in the inspector's logical
/// coordinate space so it remains crisp under integer and HiDPI scaling.
fn drawText(renderer: *sdl.Renderer, text: []const u8, start_x: c_int, y: c_int, rgb: u32) void {
    sdl.setRenderDrawColor(
        renderer,
        @intCast((rgb >> 16) & 0xFF),
        @intCast((rgb >> 8) & 0xFF),
        @intCast(rgb & 0xFF),
        0xFF,
    ) catch return;

    var x = start_x;
    for (text) |raw| {
        const bits = glyphBits(std.ascii.toUpper(raw));
        for (0..5) |row| {
            for (0..3) |column| {
                const shift: u4 = @intCast(14 - (row * 3 + column));
                if ((bits & (@as(u15, 1) << shift)) == 0) continue;
                const pixel: sdl.Rect = .{
                    .x = x + @as(c_int, @intCast(column)),
                    .y = y + @as(c_int, @intCast(row)),
                    .w = 1,
                    .h = 1,
                };
                sdl.fillRect(renderer, &pixel) catch return;
            }
        }
        x += 4;
        if (x >= INSPECTOR_WIDTH - 3) return;
    }
}

fn glyphBits(character: u8) u15 {
    return switch (character) {
        'A' => 0b010_101_111_101_101,
        'B' => 0b110_101_110_101_110,
        'C' => 0b011_100_100_100_011,
        'D' => 0b110_101_101_101_110,
        'E' => 0b111_100_110_100_111,
        'F' => 0b111_100_110_100_100,
        'G' => 0b011_100_101_101_011,
        'H' => 0b101_101_111_101_101,
        'I' => 0b111_010_010_010_111,
        'J' => 0b001_001_001_101_010,
        'K' => 0b101_101_110_101_101,
        'L' => 0b100_100_100_100_111,
        'M' => 0b101_111_111_101_101,
        'N' => 0b101_111_111_111_101,
        'O' => 0b010_101_101_101_010,
        'P' => 0b110_101_110_100_100,
        'Q' => 0b010_101_101_111_011,
        'R' => 0b110_101_110_101_101,
        'S' => 0b011_100_010_001_110,
        'T' => 0b111_010_010_010_010,
        'U' => 0b101_101_101_101_111,
        'V' => 0b101_101_101_101_010,
        'W' => 0b101_101_111_111_101,
        'X' => 0b101_101_010_101_101,
        'Y' => 0b101_101_010_010_010,
        'Z' => 0b111_001_010_100_111,
        '0' => 0b111_101_101_101_111,
        '1' => 0b010_110_010_010_111,
        '2' => 0b110_001_010_100_111,
        '3' => 0b110_001_010_001_110,
        '4' => 0b101_101_111_001_001,
        '5' => 0b111_100_110_001_110,
        '6' => 0b011_100_110_101_010,
        '7' => 0b111_001_010_010_010,
        '8' => 0b010_101_010_101_010,
        '9' => 0b010_101_011_001_110,
        '.' => 0b000_000_000_000_010,
        '-' => 0b000_000_111_000_000,
        ':' => 0b000_010_000_010_000,
        '/' => 0b001_001_010_100_100,
        ' ' => 0,
        else => 0b111_001_010_000_010,
    };
}

fn initAudio() !sdl.AudioDeviceId {
    try sdl.initSubSystem(sdl.INIT_AUDIO);
    const desired: sdl.AudioSpec = .{
        .freq = apu_mod.SAMPLE_RATE,
        .format = sdl.AUDIO_S16LSB,
        .channels = 2,
        .silence = 0,
        .samples = 512,
        .padding = 0,
        .size = 0,
        .callback = null,
        .userdata = null,
    };
    const device = try sdl.openAudioDevice(&desired);
    sdl.pauseAudioDevice(device, false);
    return device;
}

test "inspector font covers labels and digits" {
    try std.testing.expectEqual(@as(u15, 0), glyphBits(' '));
    try std.testing.expect(glyphBits('N') != 0);
    try std.testing.expect(glyphBits('0') != 0);
}
