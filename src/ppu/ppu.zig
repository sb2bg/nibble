const std = @import("std");
const sdl = @import("../sdl.zig");
const Interrupt = @import("../memory/io.zig").Interrupt;

/// Game Boy screen dimensions
pub const SCREEN_WIDTH = 160;
pub const SCREEN_HEIGHT = 144;
const WINDOW_SCALE = 3;

/// PPU modes (STAT register bits 0-1)
pub const PpuMode = enum(u2) {
    HBlank = 0,
    VBlank = 1,
    OamSearch = 2,
    PixelTransfer = 3,
};

/// DMG color palette (4 shades of gray/green)
pub const DmgColor = enum(u2) {
    White = 0,
    LightGray = 1,
    DarkGray = 2,
    Black = 3,

    pub fn toRgb(self: DmgColor) u32 {
        return switch (self) {
            .White => 0xE0F8D0,
            .LightGray => 0x88C070,
            .DarkGray => 0x346856,
            .Black => 0x081820,
        };
    }
};

pub const UiActions = struct {
    quit: bool = false,
    toggle_pause: bool = false,
    reset: bool = false,
    save_state: bool = false,
    load_state: bool = false,
    prev_slot: bool = false,
    next_slot: bool = false,
};

/// Picture Processing Unit
pub const Ppu = struct {
    frame_buffer: [SCREEN_HEIGHT][SCREEN_WIDTH]DmgColor,

    window: ?*sdl.Window,
    renderer: ?*sdl.Renderer,
    texture: ?*sdl.Texture,
    sdl_initialized: bool,

    mode: PpuMode,
    mode_cycles: u32,
    mode3_duration: u16,
    ly: u8,
    window_line: u8,
    enabled: bool,

    ui_paused: bool,
    ui_slot: u8,
    ui_slot_has_state: bool,
    ui_message: [48]u8,
    ui_message_len: usize,

    prev_pause_key: bool,
    prev_reset_key: bool,
    prev_save_key: bool,
    prev_load_key: bool,
    prev_prev_slot_key: bool,
    prev_next_slot_key: bool,
    prev_quit_key: bool,

    pub fn init() !Ppu {
        sdl.init(sdl.INIT_VIDEO) catch {
            std.debug.print("SDL_Init Error: {s}\n", .{sdl.getError()});
            return error.SdlInitFailed;
        };

        const window = sdl.createWindow(
            "Nibble",
            sdl.WINDOWPOS_CENTERED,
            sdl.WINDOWPOS_CENTERED,
            SCREEN_WIDTH * WINDOW_SCALE,
            SCREEN_HEIGHT * WINDOW_SCALE,
            sdl.WINDOW_SHOWN,
        ) catch {
            std.debug.print("SDL_CreateWindow Error: {s}\n", .{sdl.getError()});
            sdl.quit();
            return error.SdlWindowFailed;
        };

        const renderer = sdl.createRenderer(window, -1, sdl.RENDERER_ACCELERATED) catch {
            std.debug.print("SDL_CreateRenderer Error: {s}\n", .{sdl.getError()});
            sdl.destroyWindow(window);
            sdl.quit();
            return error.SdlRendererFailed;
        };

        const texture = sdl.createTexture(
            renderer,
            sdl.PIXELFORMAT_RGB888,
            sdl.TEXTUREACCESS_STREAMING,
            SCREEN_WIDTH,
            SCREEN_HEIGHT,
        ) catch {
            std.debug.print("SDL_CreateTexture Error: {s}\n", .{sdl.getError()});
            sdl.destroyRenderer(renderer);
            sdl.destroyWindow(window);
            sdl.quit();
            return error.SdlTextureFailed;
        };

        var ppu = initCommon();
        ppu.window = window;
        ppu.renderer = renderer;
        ppu.texture = texture;
        ppu.sdl_initialized = true;
        ppu.refreshWindowTitle();
        return ppu;
    }

    pub fn initHeadless() Ppu {
        return initCommon();
    }

    fn initCommon() Ppu {
        return Ppu{
            .frame_buffer = [_][SCREEN_WIDTH]DmgColor{[_]DmgColor{.White} ** SCREEN_WIDTH} ** SCREEN_HEIGHT,
            .window = null,
            .renderer = null,
            .texture = null,
            .sdl_initialized = false,
            .mode = .VBlank,
            .mode_cycles = 0,
            .mode3_duration = 172,
            .ly = 0x91,
            .window_line = 0,
            .enabled = false,
            .ui_paused = false,
            .ui_slot = 0,
            .ui_slot_has_state = false,
            .ui_message = [_]u8{0} ** 48,
            .ui_message_len = 0,
            .prev_pause_key = false,
            .prev_reset_key = false,
            .prev_save_key = false,
            .prev_load_key = false,
            .prev_prev_slot_key = false,
            .prev_next_slot_key = false,
            .prev_quit_key = false,
        };
    }

    pub fn deinit(self: *Ppu) void {
        if (self.texture) |t| sdl.destroyTexture(t);
        if (self.renderer) |r| sdl.destroyRenderer(r);
        if (self.window) |w| sdl.destroyWindow(w);
        if (self.sdl_initialized) sdl.quit();
    }

    pub fn reset(self: *Ppu) void {
        self.mode = .OamSearch;
        self.mode_cycles = 0;
        self.mode3_duration = 172;
        self.ly = 0;
        self.window_line = 0;
        self.enabled = false;
        @memset(&self.frame_buffer, [_]DmgColor{.White} ** SCREEN_WIDTH);
        self.ui_paused = false;
        self.ui_slot = 0;
        self.ui_slot_has_state = false;
        self.ui_message_len = 0;
        self.prev_pause_key = false;
        self.prev_reset_key = false;
        self.prev_save_key = false;
        self.prev_load_key = false;
        self.prev_prev_slot_key = false;
        self.prev_next_slot_key = false;
        self.prev_quit_key = false;
        self.refreshWindowTitle();
    }

    fn setMode(self: *Ppu, mode: PpuMode, bus: anytype) void {
        self.mode = mode;
        bus.io.setPpuMode(@intFromEnum(mode));
    }

    pub fn tick(self: *Ppu, cycles: u32, bus: anytype) void {
        if (!self.enabled) return;

        if (self.mode == .OamSearch) {
            const row: u8 = @intCast(@min(self.mode_cycles / 4, 19));
            bus.io.setOamScanRow(row);
        }

        self.mode_cycles += cycles;

        switch (self.mode) {
            .OamSearch => {
                if (self.mode_cycles >= 80) {
                    self.mode_cycles -= 80;
                    self.mode3_duration = self.calculateMode3Duration(bus);
                    self.setMode(.PixelTransfer, bus);
                }
            },
            .PixelTransfer => {
                if (self.mode_cycles >= self.mode3_duration) {
                    self.mode_cycles -= self.mode3_duration;
                    self.setMode(.HBlank, bus);
                    if (self.renderScanline(bus)) self.window_line +%= 1;
                }
            },
            .HBlank => {
                const hblank_duration = 456 - 80 - @as(u32, self.mode3_duration);
                if (self.mode_cycles >= hblank_duration) {
                    self.mode_cycles -= hblank_duration;
                    self.ly += 1;
                    bus.io.setLy(self.ly);

                    if (self.ly == 144) {
                        self.setMode(.VBlank, bus);
                        bus.io.requestInterrupt(Interrupt.VBLANK);
                        self.present();
                    } else {
                        self.setMode(.OamSearch, bus);
                    }
                }
            },
            .VBlank => {
                if (self.mode_cycles >= 456) {
                    self.mode_cycles -= 456;
                    self.ly += 1;
                    bus.io.setLy(self.ly);

                    if (self.ly > 153) {
                        self.ly = 0;
                        self.window_line = 0;
                        bus.io.setLy(self.ly);
                        self.setMode(.OamSearch, bus);
                    }
                }
            },
        }
    }

    /// Render a completed visible line. Returns whether the window actually
    /// emitted pixels, which is what advances the hardware's window counter.
    fn renderScanline(self: *Ppu, bus: anytype) bool {
        if (self.ly >= SCREEN_HEIGHT) return false;

        const lcdc = bus.io.getLcdc();
        const bg_enabled = (lcdc & 0x01) != 0;
        const bgp = bus.io.getBgp();
        var bg_color_ids: [SCREEN_WIDTH]u2 = [_]u2{0} ** SCREEN_WIDTH;

        if (bg_enabled) {
            const scy = bus.io.getScy();
            const scx = bus.io.getScx();

            const y = self.ly +% scy;
            const tile_y = y / 8;
            const pixel_y = y % 8;

            for (0..SCREEN_WIDTH) |x| {
                const scroll_x = @as(u8, @intCast(x)) +% scx;
                const tile_x = scroll_x / 8;
                const pixel_x = scroll_x % 8;

                const tile_map_addr: u16 = if (lcdc & 0x08 != 0) 0x9C00 else 0x9800;
                const tile_index = bus.readVram(tile_map_addr + @as(u16, tile_y) * 32 + tile_x);

                const tile_data_addr: u16 = if (lcdc & 0x10 != 0)
                    0x8000 + @as(u16, tile_index) * 16
                else blk: {
                    const signed_index: i32 = @as(i8, @bitCast(tile_index));
                    const addr: i32 = 0x9000 + signed_index * 16;
                    break :blk @intCast(addr);
                };

                const byte1 = bus.readVram(tile_data_addr + @as(u16, pixel_y) * 2);
                const byte2 = bus.readVram(tile_data_addr + @as(u16, pixel_y) * 2 + 1);

                const bit_pos: u3 = @intCast(7 - pixel_x);
                const color_id: u2 = @intCast(
                    (((byte2 >> bit_pos) & 1) << 1) | ((byte1 >> bit_pos) & 1),
                );

                const palette_shift: u3 = @as(u3, color_id) * 2;
                const palette_color: DmgColor = @enumFromInt((bgp >> palette_shift) & 0x03);

                self.frame_buffer[self.ly][x] = palette_color;
                bg_color_ids[x] = color_id;
            }
        } else {
            @memset(&self.frame_buffer[self.ly], .White);
        }

        const window_visible = self.isWindowVisible(bus);
        if (window_visible) {
            const wx = bus.io.getWx();
            {
                const window_y = self.window_line;
                const window_tile_y = window_y / 8;
                const window_pixel_y = window_y % 8;
                const window_map_addr: u16 = if (lcdc & 0x40 != 0) 0x9C00 else 0x9800;

                for (0..SCREEN_WIDTH) |x| {
                    const screen_x = @as(u8, @intCast(x));
                    const wx_adjusted: i16 = @as(i16, wx) - 7;

                    if (@as(i16, screen_x) >= wx_adjusted) {
                        const window_x: u8 = @intCast(@as(i16, screen_x) - wx_adjusted);
                        const window_tile_x = window_x / 8;
                        const window_pixel_x = window_x % 8;

                        const tile_index = bus.readVram(window_map_addr + @as(u16, window_tile_y) * 32 + window_tile_x);
                        const tile_data_addr: u16 = if (lcdc & 0x10 != 0)
                            0x8000 + @as(u16, tile_index) * 16
                        else blk: {
                            const signed_index: i32 = @as(i8, @bitCast(tile_index));
                            const addr: i32 = 0x9000 + signed_index * 16;
                            break :blk @intCast(addr);
                        };

                        const byte1 = bus.readVram(tile_data_addr + @as(u16, window_pixel_y) * 2);
                        const byte2 = bus.readVram(tile_data_addr + @as(u16, window_pixel_y) * 2 + 1);

                        const bit_pos: u3 = @intCast(7 - window_pixel_x);
                        const color_id: u2 = @intCast(
                            (((byte2 >> bit_pos) & 1) << 1) | ((byte1 >> bit_pos) & 1),
                        );

                        const palette_shift: u3 = @as(u3, color_id) * 2;
                        const palette_color: DmgColor = @enumFromInt((bgp >> palette_shift) & 0x03);

                        self.frame_buffer[self.ly][x] = palette_color;
                        bg_color_ids[x] = color_id;
                    }
                }
            }
        }

        if ((lcdc & 0x02) != 0) {
            const sprite_height: u8 = if ((lcdc & 0x04) != 0) 16 else 8;
            const obp0 = bus.io.getObp0();
            const obp1 = bus.io.getObp1();

            const Sprite = struct {
                x: i16,
                y: i16,
                tile: u8,
                attr: u8,
                index: u8,
            };

            var scanline_sprites: [10]Sprite = undefined;
            var sprite_count: usize = 0;

            var i: u8 = 0;
            while (i < 40 and sprite_count < scanline_sprites.len) : (i += 1) {
                const base: u16 = 0xFE00 + @as(u16, i) * 4;
                const oam_y = bus.readOam(base);
                const oam_x = bus.readOam(base + 1);
                const tile = bus.readOam(base + 2);
                const attr = bus.readOam(base + 3);

                const sprite_y = @as(i16, oam_y) - 16;
                const sprite_x = @as(i16, oam_x) - 8;
                const line = @as(i16, self.ly) - sprite_y;
                if (line < 0 or line >= @as(i16, sprite_height)) continue;

                scanline_sprites[sprite_count] = .{
                    .x = sprite_x,
                    .y = sprite_y,
                    .tile = tile,
                    .attr = attr,
                    .index = i,
                };
                sprite_count += 1;
            }

            for (0..SCREEN_WIDTH) |x| {
                const screen_x: i16 = @intCast(x);

                var best_found = false;
                var best_sprite_x: i16 = 0;
                var best_oam_index: u8 = 0;
                var best_attr: u8 = 0;
                var best_color_id: u2 = 0;

                var si: usize = 0;
                while (si < sprite_count) : (si += 1) {
                    const spr = scanline_sprites[si];
                    if (screen_x < spr.x or screen_x >= spr.x + 8) continue;

                    var line = @as(i16, self.ly) - spr.y;
                    if ((spr.attr & 0x40) != 0) {
                        line = @as(i16, sprite_height) - 1 - line;
                    }

                    var pixel_x = screen_x - spr.x;
                    if ((spr.attr & 0x20) != 0) {
                        pixel_x = 7 - pixel_x;
                    }

                    var tile_num = spr.tile;
                    if (sprite_height == 16) {
                        tile_num &= 0xFE;
                        if (line >= 8) {
                            tile_num +%= 1;
                            line -= 8;
                        }
                    }

                    const tile_addr: u16 =
                        0x8000 + @as(u16, tile_num) * 16 + @as(u16, @intCast(line)) * 2;
                    const lo = bus.readVram(tile_addr);
                    const hi = bus.readVram(tile_addr + 1);
                    const pixel_x_u8: u8 = @intCast(pixel_x);
                    const bit_pos: u3 = @intCast(7 - pixel_x_u8);
                    const color_id: u2 = @intCast((((hi >> bit_pos) & 1) << 1) | ((lo >> bit_pos) & 1));
                    if (color_id == 0) continue;

                    if (!best_found or spr.x < best_sprite_x or (spr.x == best_sprite_x and spr.index < best_oam_index)) {
                        best_found = true;
                        best_sprite_x = spr.x;
                        best_oam_index = spr.index;
                        best_attr = spr.attr;
                        best_color_id = color_id;
                    }
                }

                if (!best_found) continue;
                if ((best_attr & 0x80) != 0 and bg_color_ids[x] != 0) continue;

                const palette = if ((best_attr & 0x10) != 0) obp1 else obp0;
                const palette_shift: u3 = @as(u3, best_color_id) * 2;
                const sprite_color: DmgColor = @enumFromInt((palette >> palette_shift) & 0x03);
                self.frame_buffer[self.ly][x] = sprite_color;
            }
        }

        return window_visible;
    }

    fn isWindowVisible(self: *const Ppu, bus: anytype) bool {
        const lcdc = bus.io.getLcdc();
        return (lcdc & 0x21) == 0x21 and
            self.ly >= bus.io.getWy() and
            bus.io.getWx() <= 166;
    }

    /// Mode 3 starts at 172 dots, then stalls for fine scrolling, window
    /// startup, and object fetches. This models the documented DMG penalties
    /// without turning the renderer into a dot-level pixel FIFO.
    fn calculateMode3Duration(self: *const Ppu, bus: anytype) u16 {
        const lcdc = bus.io.getLcdc();
        var duration: u16 = 172 + @as(u16, bus.io.getScx() & 0x07);

        if (self.isWindowVisible(bus)) duration += 6;
        if ((lcdc & 0x02) == 0) return duration;

        const sprite_height: i16 = if ((lcdc & 0x04) != 0) 16 else 8;
        var sprite_xs: [10]u8 = undefined;
        var sprite_count: usize = 0;

        var index: u8 = 0;
        while (index < 40 and sprite_count < sprite_xs.len) : (index += 1) {
            const base: u16 = 0xFE00 + @as(u16, index) * 4;
            const y = @as(i16, bus.readOam(base)) - 16;
            const line = @as(i16, self.ly) - y;
            if (line < 0 or line >= sprite_height) continue;
            sprite_xs[sprite_count] = bus.readOam(base + 1);
            sprite_count += 1;
        }

        // The fetcher sees selected objects left-to-right, with OAM order
        // breaking ties. Insertion sort preserves that tie order.
        var i: usize = 1;
        while (i < sprite_count) : (i += 1) {
            const x = sprite_xs[i];
            var j = i;
            while (j > 0 and sprite_xs[j - 1] > x) : (j -= 1) {
                sprite_xs[j] = sprite_xs[j - 1];
            }
            sprite_xs[j] = x;
        }

        var last_tile: ?i16 = null;
        for (sprite_xs[0..sprite_count]) |oam_x| {
            if (oam_x == 0) {
                duration += 11;
                continue;
            }
            if (oam_x > 167) continue;

            const screen_x = @as(i16, oam_x) - 8;
            const window_start = @as(i16, bus.io.getWx()) - 7;
            const using_window = self.isWindowVisible(bus) and screen_x >= window_start;
            const fetch_x = if (using_window)
                screen_x - window_start
            else
                screen_x + @as(i16, bus.io.getScx());
            // Keep background and window tile identities separate even when
            // their numeric X coordinates happen to match.
            const tile = @divFloor(fetch_x, 8) + (if (using_window) @as(i16, 0x100) else 0);
            var penalty: u16 = 6;
            if (last_tile == null or last_tile.? != tile) {
                const pixel_in_tile: u4 = @intCast(@mod(fetch_x, 8));
                if (pixel_in_tile < 5) penalty += 5 - pixel_in_tile;
                last_tile = tile;
            }
            duration += penalty;
        }

        return @min(duration, 289);
    }

    fn present(self: *Ppu) void {
        if (!self.sdl_initialized) return;

        var pixels: [SCREEN_HEIGHT * SCREEN_WIDTH * 4]u8 =
            [_]u8{0} ** (SCREEN_HEIGHT * SCREEN_WIDTH * 4);

        for (0..SCREEN_HEIGHT) |y| {
            for (0..SCREEN_WIDTH) |x| {
                const offset = (y * SCREEN_WIDTH + x) * 4;
                const rgb = self.frame_buffer[y][x].toRgb();
                pixels[offset + 0] = @intCast((rgb >> 16) & 0xFF);
                pixels[offset + 1] = @intCast((rgb >> 8) & 0xFF);
                pixels[offset + 2] = @intCast(rgb & 0xFF);
                pixels[offset + 3] = 0xFF;
            }
        }

        if (self.texture) |tex| {
            sdl.updateTexture(tex, null, pixels[0..], SCREEN_WIDTH * 4) catch {};
        }

        if (self.renderer) |ren| {
            sdl.renderClear(ren) catch {};
            if (self.texture) |tex| {
                sdl.renderCopy(ren, tex, null, null) catch {};
            }
            sdl.renderPresent(ren);
        }
    }

    pub fn pollEvents(self: *Ppu, bus: anytype) UiActions {
        var actions = UiActions{};
        if (!self.sdl_initialized) return actions;

        var event: sdl.Event = undefined;
        while (sdl.pollEvent(&event)) {
            if (event.type == sdl.QUIT) {
                actions.quit = true;
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
        } else {
            self.prev_quit_key = false;
            self.prev_pause_key = false;
            self.prev_reset_key = false;
            self.prev_save_key = false;
            self.prev_load_key = false;
            self.prev_prev_slot_key = false;
            self.prev_next_slot_key = false;
        }

        return actions;
    }

    pub fn setUiStatus(
        self: *Ppu,
        paused: bool,
        slot: u8,
        slot_has_state: bool,
        message: []const u8,
    ) void {
        self.ui_paused = paused;
        self.ui_slot = slot;
        self.ui_slot_has_state = slot_has_state;
        self.ui_message_len = @min(message.len, self.ui_message.len);
        if (self.ui_message_len > 0) {
            @memcpy(self.ui_message[0..self.ui_message_len], message[0..self.ui_message_len]);
        }
        self.refreshWindowTitle();
    }

    pub fn redraw(self: *Ppu) void {
        self.refreshWindowTitle();
        self.present();
    }

    fn refreshWindowTitle(self: *Ppu) void {
        if (!self.sdl_initialized) return;
        const window = self.window orelse return;

        var title_buf: [128:0]u8 = undefined;
        const state = if (self.ui_paused) "Paused" else "Running";
        const slot_state = if (self.ui_slot_has_state) "set" else "empty";
        const title = if (self.ui_message_len > 0)
            std.fmt.bufPrintZ(
                &title_buf,
                "Nibble | {s} | Slot {d} {s} | {s}",
                .{ state, self.ui_slot, slot_state, self.ui_message[0..self.ui_message_len] },
            ) catch "Nibble"
        else
            std.fmt.bufPrintZ(
                &title_buf,
                "Nibble | {s} | Slot {d} {s}",
                .{ state, self.ui_slot, slot_state },
            ) catch "Nibble";

        sdl.setWindowTitle(window, title);
    }

    fn edgePressed(previous: *bool, current: bool) bool {
        const pressed = current and !previous.*;
        previous.* = current;
        return pressed;
    }

    pub fn setEnabled(self: *Ppu, enabled: bool) void {
        const was_enabled = self.enabled;
        self.enabled = enabled;

        if (!enabled) {
            self.ly = 0;
            self.mode = .HBlank;
            self.mode_cycles = 0;
            self.mode3_duration = 172;
            self.window_line = 0;
        } else if (!was_enabled) {
            self.ly = 0;
            self.mode = .OamSearch;
            self.mode_cycles = 4;
            self.mode3_duration = 172;
            self.window_line = 0;
        }
    }

    fn isPressed(keys: []const u8, scancode: usize) bool {
        return scancode < keys.len and keys[scancode] != 0;
    }
};

test "disabling LCD leaves PPU in HBlank mode" {
    var ppu = Ppu.initHeadless();
    ppu.mode = .PixelTransfer;
    ppu.setEnabled(false);

    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    try std.testing.expectEqual(@as(u8, 0), ppu.ly);
}

test "mode 3 timing includes fine scroll, window, and visible objects" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const IoReg = @import("../memory/io.zig").IoReg;
    const TestBus = struct {
        io: IoRegisters,
        oam: [0xA0]u8 = [_]u8{0} ** 0xA0,

        fn readOam(self: *const @This(), addr: u16) u8 {
            return self.oam[addr - 0xFE00];
        }
    };

    var bus = TestBus{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    var ppu = Ppu.initHeadless();
    ppu.ly = 0;

    bus.io.data[@intFromEnum(IoReg.SCX)] = 5;
    try std.testing.expectEqual(@as(u16, 177), ppu.calculateMode3Duration(&bus));

    bus.io.data[@intFromEnum(IoReg.LCDC)] |= 0x20;
    bus.io.data[@intFromEnum(IoReg.WY)] = 0;
    bus.io.data[@intFromEnum(IoReg.WX)] = 7;
    try std.testing.expectEqual(@as(u16, 183), ppu.calculateMode3Duration(&bus));

    bus.io.data[@intFromEnum(IoReg.LCDC)] |= 0x02;
    bus.oam[0] = 16;
    bus.oam[1] = 8;
    try std.testing.expectEqual(@as(u16, 194), ppu.calculateMode3Duration(&bus));
}
