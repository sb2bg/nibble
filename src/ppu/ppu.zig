const std = @import("std");
const sdl = @import("../sdl.zig");
const IoReg = @import("../memory/io.zig").IoReg;
const Interrupt = @import("../memory/io.zig").Interrupt;

/// Game Boy screen dimensions
pub const SCREEN_WIDTH = 160;
pub const SCREEN_HEIGHT = 144;
pub const UI_LEFT_WIDTH = 112;
pub const UI_RIGHT_WIDTH = 156;
pub const OUTPUT_WIDTH = UI_LEFT_WIDTH + SCREEN_WIDTH + UI_RIGHT_WIDTH;
const WINDOW_SCALE = 3;

/// PPU modes (STAT register bits 0-1)
pub const PpuMode = enum(u2) {
    HBlank = 0, // Mode 0: HBlank (after scanline rendering)
    VBlank = 1, // Mode 1: VBlank (after frame)
    OamSearch = 2, // Mode 2: Searching OAM
    PixelTransfer = 3, // Mode 3: Transferring pixels to LCD
};

/// DMG color palette (4 shades of gray/green)
pub const DmgColor = enum(u2) {
    White = 0,
    LightGray = 1,
    DarkGray = 2,
    Black = 3,

    /// Convert to RGB888
    pub fn toRgb(self: DmgColor) u32 {
        return switch (self) {
            .White => 0xE0F8D0, // Lightest (GB green tint)
            .LightGray => 0x88C070,
            .DarkGray => 0x346856,
            .Black => 0x081820, // Darkest
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
    toggle_panel: bool = false,
};

/// Picture Processing Unit
pub const Ppu = struct {
    // Frame buffer: 160x144 pixels, each pixel is 2 bits (4 colors)
    frame_buffer: [SCREEN_HEIGHT][SCREEN_WIDTH]DmgColor,

    // SDL context
    window: ?*sdl.Window,
    renderer: ?*sdl.Renderer,
    texture: ?*sdl.Texture,
    sdl_initialized: bool,

    // PPU state
    mode: PpuMode,
    mode_cycles: u32, // Cycles spent in current mode
    ly: u8, // Current scanline (0-153)

    // Enable/disable rendering
    enabled: bool,

    // UI panel state
    ui_show_panel: bool,
    ui_paused: bool,
    ui_slot: u8,
    ui_slot_has_state: bool,
    ui_joypad_state: u8,
    ui_message: [48]u8,
    ui_message_len: usize,

    // Key edge tracking for emulator controls
    prev_pause_key: bool,
    prev_reset_key: bool,
    prev_save_key: bool,
    prev_load_key: bool,
    prev_prev_slot_key: bool,
    prev_next_slot_key: bool,
    prev_panel_key: bool,
    prev_quit_key: bool,
    prev_mouse_left: bool,

    pub fn init() !Ppu {
        // Initialize SDL
        sdl.init(sdl.INIT_VIDEO) catch {
            std.debug.print("SDL_Init Error: {s}\n", .{sdl.getError()});
            return error.SdlInitFailed;
        };

        // Create window
        const window = sdl.createWindow(
            "Nibble - Game Boy Emulator",
            sdl.WINDOWPOS_CENTERED,
            sdl.WINDOWPOS_CENTERED,
            OUTPUT_WIDTH * WINDOW_SCALE,
            SCREEN_HEIGHT * WINDOW_SCALE,
            sdl.WINDOW_SHOWN,
        ) catch {
            std.debug.print("SDL_CreateWindow Error: {s}\n", .{sdl.getError()});
            sdl.quit();
            return error.SdlWindowFailed;
        };

        // Create renderer
        const renderer = sdl.createRenderer(window, -1, sdl.RENDERER_ACCELERATED) catch {
            std.debug.print("SDL_CreateRenderer Error: {s}\n", .{sdl.getError()});
            sdl.destroyWindow(window);
            sdl.quit();
            return error.SdlRendererFailed;
        };

        // Create texture for frame buffer
        const texture = sdl.createTexture(
            renderer,
            sdl.PIXELFORMAT_RGB888,
            sdl.TEXTUREACCESS_STREAMING,
            OUTPUT_WIDTH,
            SCREEN_HEIGHT,
        ) catch {
            std.debug.print("SDL_CreateTexture Error: {s}\n", .{sdl.getError()});
            sdl.destroyRenderer(renderer);
            sdl.destroyWindow(window);
            sdl.quit();
            return error.SdlTextureFailed;
        };

        return Ppu{
            .frame_buffer = [_][SCREEN_WIDTH]DmgColor{[_]DmgColor{.White} ** SCREEN_WIDTH} ** SCREEN_HEIGHT,
            .window = window,
            .renderer = renderer,
            .texture = texture,
            .sdl_initialized = true,
            .mode = .VBlank, // Start in VBlank (post-boot ROM state)
            .mode_cycles = 0,
            .ly = 0x91, // Post-boot LY value (145, in VBlank)
            .enabled = false,
            .ui_show_panel = true,
            .ui_paused = false,
            .ui_slot = 0,
            .ui_slot_has_state = false,
            .ui_joypad_state = 0xFF,
            .ui_message = [_]u8{0} ** 48,
            .ui_message_len = 0,
            .prev_pause_key = false,
            .prev_reset_key = false,
            .prev_save_key = false,
            .prev_load_key = false,
            .prev_prev_slot_key = false,
            .prev_next_slot_key = false,
            .prev_panel_key = false,
            .prev_quit_key = false,
            .prev_mouse_left = false,
        };
    }

    pub fn initHeadless() Ppu {
        return Ppu{
            .frame_buffer = [_][SCREEN_WIDTH]DmgColor{[_]DmgColor{.White} ** SCREEN_WIDTH} ** SCREEN_HEIGHT,
            .window = null,
            .renderer = null,
            .texture = null,
            .sdl_initialized = false,
            .mode = .VBlank, // Start in VBlank (post-boot ROM state)
            .mode_cycles = 0,
            .ly = 0x91, // Post-boot LY value (145, in VBlank)
            .enabled = false,
            .ui_show_panel = true,
            .ui_paused = false,
            .ui_slot = 0,
            .ui_slot_has_state = false,
            .ui_joypad_state = 0xFF,
            .ui_message = [_]u8{0} ** 48,
            .ui_message_len = 0,
            .prev_pause_key = false,
            .prev_reset_key = false,
            .prev_save_key = false,
            .prev_load_key = false,
            .prev_prev_slot_key = false,
            .prev_next_slot_key = false,
            .prev_panel_key = false,
            .prev_quit_key = false,
            .prev_mouse_left = false,
        };
    }

    pub fn deinit(self: *Ppu) void {
        if (self.texture) |t| sdl.destroyTexture(t);
        if (self.renderer) |r| sdl.destroyRenderer(r);
        if (self.window) |w| sdl.destroyWindow(w);
        if (self.sdl_initialized) sdl.quit();
    }

    /// Reset PPU state
    pub fn reset(self: *Ppu) void {
        self.mode = .OamSearch;
        self.mode_cycles = 0;
        self.ly = 0;
        self.enabled = false;
        @memset(&self.frame_buffer, [_]DmgColor{.White} ** SCREEN_WIDTH);
        self.ui_paused = false;
        self.ui_slot = 0;
        self.ui_slot_has_state = false;
        self.ui_joypad_state = 0xFF;
        self.ui_message_len = 0;
        self.prev_pause_key = false;
        self.prev_reset_key = false;
        self.prev_save_key = false;
        self.prev_load_key = false;
        self.prev_prev_slot_key = false;
        self.prev_next_slot_key = false;
        self.prev_panel_key = false;
        self.prev_quit_key = false;
        self.prev_mouse_left = false;
    }

    /// Update STAT register mode bits
    fn setMode(self: *Ppu, mode: PpuMode, bus: anytype) void {
        const old_mode = self.mode;
        self.mode = mode;

        // Update STAT register bits 0-1 with the current mode
        const stat = bus.io.getStat();
        const new_stat = (stat & 0xFC) | @intFromEnum(mode);
        bus.io.data[@intFromEnum(IoReg.STAT)] = new_stat;

        if (mode != old_mode) {
            const mode_interrupt_enabled = switch (mode) {
                .HBlank => (new_stat & 0x08) != 0, // Mode 0 interrupt enable
                .VBlank => (new_stat & 0x10) != 0, // Mode 1 interrupt enable
                .OamSearch => (new_stat & 0x20) != 0, // Mode 2 interrupt enable
                .PixelTransfer => false, // No STAT interrupt for mode 3
            };

            if (mode_interrupt_enabled) {
                bus.io.requestInterrupt(Interrupt.LCD_STAT);
            }
        }
    }

    /// Tick the PPU with the given number of cycles
    pub fn tick(self: *Ppu, cycles: u32, bus: anytype) void {
        if (!self.enabled) return;

        if (self.mode == .OamSearch) {
            // OAM scan advances one row every 4 cycles during mode 2.
            const row: u8 = @intCast(@min(self.mode_cycles / 4, 19));
            bus.io.setOamScanRow(row);
        }

        self.mode_cycles += cycles;

        switch (self.mode) {
            .OamSearch => {
                // Mode 2: OAM Search - 80 cycles
                if (self.mode_cycles >= 80) {
                    self.mode_cycles -= 80;
                    self.setMode(.PixelTransfer, bus);
                }
            },
            .PixelTransfer => {
                // Mode 3: Pixel Transfer - 172 cycles (variable, we use fixed)
                if (self.mode_cycles >= 172) {
                    self.mode_cycles -= 172;
                    self.setMode(.HBlank, bus);
                    self.renderScanline(bus);
                }
            },
            .HBlank => {
                // Mode 0: HBlank - 204 cycles
                if (self.mode_cycles >= 204) {
                    self.mode_cycles -= 204;
                    self.ly += 1;
                    bus.io.setLy(self.ly); // Update LY and check LYC

                    if (self.ly == 144) {
                        // Enter VBlank
                        self.setMode(.VBlank, bus);
                        bus.io.requestInterrupt(Interrupt.VBLANK);
                        self.present(); // Display frame
                    } else {
                        // Next scanline
                        self.setMode(.OamSearch, bus);
                    }
                }
            },
            .VBlank => {
                // Mode 1: VBlank - 456 cycles per line, 10 lines (144-153)
                if (self.mode_cycles >= 456) {
                    self.mode_cycles -= 456;
                    self.ly += 1;
                    bus.io.setLy(self.ly); // Update LY and check LYC

                    if (self.ly > 153) {
                        // Start new frame
                        self.ly = 0;
                        bus.io.setLy(self.ly); // Update LY and check LYC
                        self.setMode(.OamSearch, bus);
                    }
                }
            },
        }
    }

    /// Render a single scanline to the frame buffer
    fn renderScanline(self: *Ppu, bus: anytype) void {
        if (self.ly >= SCREEN_HEIGHT) return;

        const lcdc = bus.io.getLcdc();
        const bg_enabled = (lcdc & 0x01) != 0;
        const bgp = bus.io.getBgp();
        var bg_color_ids: [SCREEN_WIDTH]u2 = [_]u2{0} ** SCREEN_WIDTH;

        // Render background layer
        if (bg_enabled) {
            const scy = bus.io.getScy();
            const scx = bus.io.getScx();

            const y = self.ly +% scy; // Wrapping add for scrolling
            const tile_y = y / 8;
            const pixel_y = y % 8;

            // Render each pixel in the scanline
            for (0..SCREEN_WIDTH) |x| {
                const scroll_x = @as(u8, @intCast(x)) +% scx;
                const tile_x = scroll_x / 8;
                const pixel_x = scroll_x % 8;

                // Get tile index from background map
                const tile_map_addr: u16 = if (lcdc & 0x08 != 0) 0x9C00 else 0x9800;
                const tile_index = bus.readVram(tile_map_addr + @as(u16, tile_y) * 32 + tile_x);

                // Get tile data
                const tile_data_addr: u16 = if (lcdc & 0x10 != 0)
                    0x8000 + @as(u16, tile_index) * 16
                else blk: {
                    // Signed tile index mode: base is 0x9000, tile_index is signed (-128 to 127)
                    const signed_index: i32 = @as(i8, @bitCast(tile_index));
                    const addr: i32 = 0x9000 + signed_index * 16;
                    break :blk @intCast(addr);
                };

                // Read 2 bytes for this row of the tile
                const byte1 = bus.readVram(tile_data_addr + @as(u16, pixel_y) * 2);
                const byte2 = bus.readVram(tile_data_addr + @as(u16, pixel_y) * 2 + 1);

                // Extract color for this pixel (bit 7-pixel_x)
                const bit_pos: u3 = @intCast(7 - pixel_x);
                const color_id: u2 = @intCast(
                    (((byte2 >> bit_pos) & 1) << 1) | ((byte1 >> bit_pos) & 1),
                );

                // Apply palette
                const palette_shift: u3 = @as(u3, color_id) * 2;
                const palette_color: DmgColor = @enumFromInt((bgp >> palette_shift) & 0x03);

                self.frame_buffer[self.ly][x] = palette_color;
                bg_color_ids[x] = color_id;
            }
        } else {
            // Background disabled - fill with color 0
            @memset(&self.frame_buffer[self.ly], .White);
        }

        // Render window layer on top of background
        const window_enabled = (lcdc & 0x20) != 0;
        if (window_enabled) {
            const wy = bus.io.getWy();
            const wx = bus.io.getWx();

            // Check if window is visible on this scanline
            if (self.ly >= wy) {
                const window_y = self.ly - wy;
                const window_tile_y = window_y / 8;
                const window_pixel_y = window_y % 8;

                // Window tile map address (LCDC bit 6)
                const window_map_addr: u16 = if (lcdc & 0x40 != 0) 0x9C00 else 0x9800;

                for (0..SCREEN_WIDTH) |x| {
                    // WX is offset by 7, so WX=7 means window starts at screen x=0
                    const screen_x = @as(u8, @intCast(x));
                    const wx_adjusted: i16 = @as(i16, wx) - 7;

                    if (@as(i16, screen_x) >= wx_adjusted) {
                        const window_x: u8 = @intCast(@as(i16, screen_x) - wx_adjusted);
                        const window_tile_x = window_x / 8;
                        const window_pixel_x = window_x % 8;

                        // Get tile index from window map
                        const tile_index = bus.readVram(window_map_addr + @as(u16, window_tile_y) * 32 + window_tile_x);

                        // Get tile data (same addressing mode as background)
                        const tile_data_addr: u16 = if (lcdc & 0x10 != 0)
                            0x8000 + @as(u16, tile_index) * 16
                        else blk: {
                            const signed_index: i32 = @as(i8, @bitCast(tile_index));
                            const addr: i32 = 0x9000 + signed_index * 16;
                            break :blk @intCast(addr);
                        };

                        // Read 2 bytes for this row of the tile
                        const byte1 = bus.readVram(tile_data_addr + @as(u16, window_pixel_y) * 2);
                        const byte2 = bus.readVram(tile_data_addr + @as(u16, window_pixel_y) * 2 + 1);

                        // Extract color for this pixel
                        const bit_pos: u3 = @intCast(7 - window_pixel_x);
                        const color_id: u2 = @intCast(
                            (((byte2 >> bit_pos) & 1) << 1) | ((byte1 >> bit_pos) & 1),
                        );

                        // Apply palette
                        const palette_shift: u3 = @as(u3, color_id) * 2;
                        const palette_color: DmgColor = @enumFromInt((bgp >> palette_shift) & 0x03);

                        self.frame_buffer[self.ly][x] = palette_color;
                        bg_color_ids[x] = color_id;
                    }
                }
            }
        }

        // Render sprites on top of BG/window.
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

                // Priority bit: behind BG/window colors 1-3 (but in front of color 0).
                if ((best_attr & 0x80) != 0 and bg_color_ids[x] != 0) continue;

                const palette = if ((best_attr & 0x10) != 0) obp1 else obp0;
                const palette_shift: u3 = @as(u3, best_color_id) * 2;
                const sprite_color: DmgColor = @enumFromInt((palette >> palette_shift) & 0x03);
                self.frame_buffer[self.ly][x] = sprite_color;
            }
        }
    }

    /// Present the frame buffer to the screen
    fn present(self: *Ppu) void {
        // Convert frame buffer to RGB888 and composite UI chrome.
        var pixels: [SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8 = [_]u8{0} ** (SCREEN_HEIGHT * OUTPUT_WIDTH * 4);
        fillRect(&pixels, OUTPUT_WIDTH, 0, 0, OUTPUT_WIDTH, SCREEN_HEIGHT, 0x0A0F17);
        fillRect(&pixels, OUTPUT_WIDTH, 0, 0, UI_LEFT_WIDTH, SCREEN_HEIGHT, 0x111B2A);
        fillRect(
            &pixels,
            OUTPUT_WIDTH,
            UI_LEFT_WIDTH + SCREEN_WIDTH,
            0,
            UI_RIGHT_WIDTH,
            SCREEN_HEIGHT,
            0x111B2A,
        );
        fillRect(&pixels, OUTPUT_WIDTH, UI_LEFT_WIDTH - 1, 0, 1, SCREEN_HEIGHT, 0x334155);
        fillRect(
            &pixels,
            OUTPUT_WIDTH,
            UI_LEFT_WIDTH + SCREEN_WIDTH,
            0,
            1,
            SCREEN_HEIGHT,
            0x334155,
        );

        // Subtle bezel around the game viewport.
        fillRect(
            &pixels,
            OUTPUT_WIDTH,
            UI_LEFT_WIDTH - 2,
            0,
            SCREEN_WIDTH + 4,
            SCREEN_HEIGHT,
            0x0B1220,
        );

        for (0..SCREEN_HEIGHT) |y| {
            for (0..SCREEN_WIDTH) |x| {
                setPixelRgb(
                    &pixels,
                    OUTPUT_WIDTH,
                    UI_LEFT_WIDTH + x,
                    y,
                    self.frame_buffer[y][x].toRgb(),
                );
            }
        }

        if (self.ui_show_panel) {
            self.drawUiPanels(&pixels);
        }

        // Update texture
        if (self.texture) |tex| {
            sdl.updateTexture(tex, null, &pixels, OUTPUT_WIDTH * 4) catch {};
        }

        // Render
        if (self.renderer) |ren| {
            sdl.renderClear(ren) catch {};
            if (self.texture) |tex| {
                sdl.renderCopy(ren, tex, null, null) catch {};
            }
            sdl.renderPresent(ren);
        }
    }

    /// Check SDL events and keyboard state. Returns emulator management actions.
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

        var state: u8 = 0xFF; // 0 = pressed, 1 = released
        if (keys.len != 0) {
            if (isPressed(keys, sdl.SCANCODE_RIGHT)) state &= ~@as(u8, 0x01);
            if (isPressed(keys, sdl.SCANCODE_LEFT)) state &= ~@as(u8, 0x02);
            if (isPressed(keys, sdl.SCANCODE_UP)) state &= ~@as(u8, 0x04);
            if (isPressed(keys, sdl.SCANCODE_DOWN)) state &= ~@as(u8, 0x08);
            if (isPressed(keys, sdl.SCANCODE_X) or isPressed(keys, sdl.SCANCODE_A)) state &= ~@as(u8, 0x10); // A
            if (isPressed(keys, sdl.SCANCODE_Z) or isPressed(keys, sdl.SCANCODE_S)) state &= ~@as(u8, 0x20); // B
            if (isPressed(keys, sdl.SCANCODE_BACKSPACE) or isPressed(keys, sdl.SCANCODE_TAB)) state &= ~@as(u8, 0x40); // Select
            if (isPressed(keys, sdl.SCANCODE_RETURN) or isPressed(keys, sdl.SCANCODE_KP_ENTER) or isPressed(keys, sdl.SCANCODE_SPACE)) state &= ~@as(u8, 0x80); // Start
        }

        var mouse_x: c_int = 0;
        var mouse_y: c_int = 0;
        const mouse_mask = sdl.getMouseState(&mouse_x, &mouse_y);
        const mouse_left = (mouse_mask & sdl.BUTTON_LMASK) != 0;
        const logical_x: usize = if (mouse_x <= 0) 0 else @intCast(@divFloor(mouse_x, WINDOW_SCALE));
        const logical_y: usize = if (mouse_y <= 0) 0 else @intCast(@divFloor(mouse_y, WINDOW_SCALE));

        if (self.ui_show_panel and mouse_left) {
            self.applyMouseJoypad(&state, logical_x, logical_y);
        }

        const previous = bus.io.getJoypadState();
        bus.io.setJoypadState(state);
        if ((previous & ~state) != 0) {
            bus.io.requestInterrupt(Interrupt.JOYPAD);
        }

        if (keys.len != 0) {
            actions.quit = actions.quit or edgePressed(&self.prev_quit_key, isPressed(keys, sdl.SCANCODE_ESCAPE));
            actions.toggle_pause = edgePressed(&self.prev_pause_key, isPressed(keys, sdl.SCANCODE_P));
            actions.reset = edgePressed(&self.prev_reset_key, isPressed(keys, sdl.SCANCODE_R));
            actions.save_state = edgePressed(&self.prev_save_key, isPressed(keys, sdl.SCANCODE_F5));
            actions.load_state = edgePressed(&self.prev_load_key, isPressed(keys, sdl.SCANCODE_F9));
            actions.prev_slot = edgePressed(&self.prev_prev_slot_key, isPressed(keys, sdl.SCANCODE_LEFTBRACKET));
            actions.next_slot = edgePressed(&self.prev_next_slot_key, isPressed(keys, sdl.SCANCODE_RIGHTBRACKET));
            actions.toggle_panel = edgePressed(&self.prev_panel_key, isPressed(keys, sdl.SCANCODE_F1));
        } else {
            self.prev_quit_key = false;
            self.prev_pause_key = false;
            self.prev_reset_key = false;
            self.prev_save_key = false;
            self.prev_load_key = false;
            self.prev_prev_slot_key = false;
            self.prev_next_slot_key = false;
            self.prev_panel_key = false;
        }

        if (self.ui_show_panel and mouse_left and !self.prev_mouse_left) {
            self.applyMouseManagement(&actions, logical_x, logical_y);
        }
        self.prev_mouse_left = mouse_left;

        return actions;
    }

    pub fn setUiPanelState(
        self: *Ppu,
        paused: bool,
        slot: u8,
        slot_has_state: bool,
        joypad_state: u8,
        show_panel: bool,
        message: []const u8,
    ) void {
        self.ui_paused = paused;
        self.ui_slot = slot;
        self.ui_slot_has_state = slot_has_state;
        self.ui_joypad_state = joypad_state;
        self.ui_show_panel = show_panel;
        self.ui_message_len = @min(message.len, self.ui_message.len);
        if (self.ui_message_len > 0) {
            @memcpy(self.ui_message[0..self.ui_message_len], message[0..self.ui_message_len]);
        }
    }

    pub fn redraw(self: *Ppu) void {
        if (!self.sdl_initialized) return;
        self.present();
    }

    fn edgePressed(previous: *bool, current: bool) bool {
        const pressed = current and !previous.*;
        previous.* = current;
        return pressed;
    }

    fn applyMouseJoypad(self: *const Ppu, state: *u8, x: usize, y: usize) void {
        _ = self;
        const card_x: usize = 6;
        const card_y: usize = 6;
        const card_h: usize = SCREEN_HEIGHT - 12;

        const dpad_x: usize = card_x + 18;
        const dpad_y: usize = card_y + 28;

        if (pointInRect(x, y, dpad_x + 36, dpad_y + 18, 18, 18)) state.* &= ~@as(u8, 0x01); // Right
        if (pointInRect(x, y, dpad_x, dpad_y + 18, 18, 18)) state.* &= ~@as(u8, 0x02); // Left
        if (pointInRect(x, y, dpad_x + 18, dpad_y, 18, 18)) state.* &= ~@as(u8, 0x04); // Up
        if (pointInRect(x, y, dpad_x + 18, dpad_y + 36, 18, 18)) state.* &= ~@as(u8, 0x08); // Down

        if (pointInCircle(x, y, card_x + 70, card_y + 56, 12)) state.* &= ~@as(u8, 0x10); // A
        if (pointInCircle(x, y, card_x + 48, card_y + 82, 10)) state.* &= ~@as(u8, 0x20); // B
        if (pointInRect(x, y, card_x + 8, card_y + card_h - 28, 38, 14)) state.* &= ~@as(u8, 0x40); // Select
        if (pointInRect(x, y, card_x + 52, card_y + card_h - 28, 38, 14)) state.* &= ~@as(u8, 0x80); // Start
    }

    fn applyMouseManagement(self: *const Ppu, actions: *UiActions, x: usize, y: usize) void {
        _ = self;
        const card_x: usize = UI_LEFT_WIDTH + SCREEN_WIDTH + 8;
        const card_y: usize = 6;
        const b0 = card_y + 44;

        if (pointInRect(x, y, card_x + 8, b0, 58, 14)) actions.toggle_pause = true;
        if (pointInRect(x, y, card_x + 72, b0, 58, 14)) actions.reset = true;
        if (pointInRect(x, y, card_x + 8, b0 + 18, 58, 14)) actions.save_state = true;
        if (pointInRect(x, y, card_x + 72, b0 + 18, 58, 14)) actions.load_state = true;
        if (pointInRect(x, y, card_x + 8, b0 + 36, 58, 14)) actions.prev_slot = true;
        if (pointInRect(x, y, card_x + 72, b0 + 36, 58, 14)) actions.next_slot = true;
    }

    fn pointInRect(px: usize, py: usize, x: usize, y: usize, w: usize, h: usize) bool {
        return px >= x and px < x + w and py >= y and py < y + h;
    }

    fn pointInCircle(px: usize, py: usize, cx: usize, cy: usize, r: usize) bool {
        const px_i: i32 = @intCast(px);
        const py_i: i32 = @intCast(py);
        const cx_i: i32 = @intCast(cx);
        const cy_i: i32 = @intCast(cy);
        const r_i: i32 = @intCast(r);
        const dx = px_i - cx_i;
        const dy = py_i - cy_i;
        return dx * dx + dy * dy <= r_i * r_i;
    }

    fn drawUiPanels(self: *const Ppu, pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8) void {
        self.drawJoypadPanel(pixels);
        self.drawManagementPanel(pixels);
    }

    fn drawJoypadPanel(self: *const Ppu, pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8) void {
        const card_x: usize = 6;
        const card_y: usize = 6;
        const card_w: usize = UI_LEFT_WIDTH - 12;
        const card_h: usize = SCREEN_HEIGHT - 12;
        fillRect(pixels, OUTPUT_WIDTH, card_x, card_y, card_w, card_h, 0x1A2638);
        fillRect(pixels, OUTPUT_WIDTH, card_x, card_y, card_w, 1, 0x36475F);
        fillRect(pixels, OUTPUT_WIDTH, card_x, card_y + card_h - 1, card_w, 1, 0x0B1018);
        drawText(pixels, OUTPUT_WIDTH, card_x + 8, card_y + 8, "JOY PAD", 0xDDE7F3, 1);

        const up_pressed = (self.ui_joypad_state & 0x04) == 0;
        const down_pressed = (self.ui_joypad_state & 0x08) == 0;
        const left_pressed = (self.ui_joypad_state & 0x02) == 0;
        const right_pressed = (self.ui_joypad_state & 0x01) == 0;
        const a_pressed = (self.ui_joypad_state & 0x10) == 0;
        const b_pressed = (self.ui_joypad_state & 0x20) == 0;
        const select_pressed = (self.ui_joypad_state & 0x40) == 0;
        const start_pressed = (self.ui_joypad_state & 0x80) == 0;

        const dpad_x: usize = card_x + 18;
        const dpad_y: usize = card_y + 28;
        drawButton(pixels, dpad_x + 18, dpad_y, 18, 18, "U", up_pressed);
        drawButton(pixels, dpad_x, dpad_y + 18, 18, 18, "L", left_pressed);
        drawButton(pixels, dpad_x + 18, dpad_y + 18, 18, 18, " ", false);
        drawButton(pixels, dpad_x + 36, dpad_y + 18, 18, 18, "R", right_pressed);
        drawButton(pixels, dpad_x + 18, dpad_y + 36, 18, 18, "D", down_pressed);

        drawCircleButton(pixels, card_x + 70, card_y + 56, 12, "A", a_pressed);
        drawCircleButton(pixels, card_x + 48, card_y + 82, 10, "B", b_pressed);

        drawButton(
            pixels,
            card_x + 8,
            card_y + card_h - 28,
            38,
            14,
            "SELECT",
            select_pressed,
        );
        drawButton(
            pixels,
            card_x + 52,
            card_y + card_h - 28,
            38,
            14,
            "START",
            start_pressed,
        );
    }

    fn drawManagementPanel(self: *const Ppu, pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8) void {
        const card_x: usize = UI_LEFT_WIDTH + SCREEN_WIDTH + 8;
        const card_y: usize = 6;
        const card_w: usize = UI_RIGHT_WIDTH - 16;
        const card_h: usize = SCREEN_HEIGHT - 12;
        fillRect(pixels, OUTPUT_WIDTH, card_x, card_y, card_w, card_h, 0x1A2638);
        fillRect(pixels, OUTPUT_WIDTH, card_x, card_y, card_w, 1, 0x36475F);
        fillRect(pixels, OUTPUT_WIDTH, card_x, card_y + card_h - 1, card_w, 1, 0x0B1018);

        fillRect(
            pixels,
            OUTPUT_WIDTH,
            card_x + 8,
            card_y + 8,
            card_w - 16,
            16,
            if (self.ui_paused) 0x8A5A15 else 0x1F5F4A,
        );
        drawText(
            pixels,
            OUTPUT_WIDTH,
            card_x + 14,
            card_y + 12,
            if (self.ui_paused) "PAUSED" else "RUNNING",
            0xF8FAFC,
            1,
        );

        var slot_buf: [24]u8 = undefined;
        const slot_text = std.fmt.bufPrint(&slot_buf, "SLOT {d}", .{self.ui_slot}) catch "SLOT ?";
        drawText(pixels, OUTPUT_WIDTH, card_x + 10, card_y + 30, slot_text, 0xE2E8F0, 1);
        drawText(
            pixels,
            OUTPUT_WIDTH,
            card_x + 68,
            card_y + 30,
            if (self.ui_slot_has_state) "SET" else "EMPTY",
            if (self.ui_slot_has_state) 0x86EFAC else 0x94A3B8,
            1,
        );

        const b0 = card_y + 44;
        drawButton(pixels, card_x + 8, b0, 58, 14, "PAUSE P", self.ui_paused);
        drawButton(pixels, card_x + 72, b0, 58, 14, "RESET R", false);
        drawButton(pixels, card_x + 8, b0 + 18, 58, 14, "SAVE F5", false);
        drawButton(pixels, card_x + 72, b0 + 18, 58, 14, "LOAD F9", false);
        drawButton(pixels, card_x + 8, b0 + 36, 58, 14, "SLOT DN [", false);
        drawButton(pixels, card_x + 72, b0 + 36, 58, 14, "SLOT UP ]", false);

        fillRect(pixels, OUTPUT_WIDTH, card_x + 8, card_y + 102, card_w - 16, 28, 0x101826);
        fillRect(pixels, OUTPUT_WIDTH, card_x + 8, card_y + 102, card_w - 16, 1, 0x334155);
        drawText(pixels, OUTPUT_WIDTH, card_x + 12, card_y + 106, "MESSAGE", 0x93C5FD, 1);
        if (self.ui_message_len > 0) {
            drawText(
                pixels,
                OUTPUT_WIDTH,
                card_x + 12,
                card_y + 116,
                self.ui_message[0..self.ui_message_len],
                0xF8FAFC,
                1,
            );
        } else {
            drawText(pixels, OUTPUT_WIDTH, card_x + 12, card_y + 116, "READY", 0x94A3B8, 1);
        }

        drawText(pixels, OUTPUT_WIDTH, card_x + 10, card_y + 136 - 10, "F1 UI  ESC QUIT", 0x94A3B8, 1);
    }

    fn drawButton(
        pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8,
        x: usize,
        y: usize,
        w: usize,
        h: usize,
        label: []const u8,
        pressed: bool,
    ) void {
        const bg: u32 = if (pressed) 0x3B82F6 else 0x223247;
        const top: u32 = if (pressed) 0x93C5FD else 0x4B5F7A;
        const bottom: u32 = if (pressed) 0x1E3A8A else 0x0B1220;
        fillRect(pixels, OUTPUT_WIDTH, x, y, w, h, bg);
        fillRect(pixels, OUTPUT_WIDTH, x, y, w, 1, top);
        fillRect(pixels, OUTPUT_WIDTH, x, y + h - 1, w, 1, bottom);
        fillRect(pixels, OUTPUT_WIDTH, x, y, 1, h, top);
        fillRect(pixels, OUTPUT_WIDTH, x + w - 1, y, 1, h, bottom);
        if (label.len > 0) {
            const text_w = label.len * 6;
            const text_x = if (w > text_w) x + (w - text_w) / 2 else x + 1;
            const text_y = if (h > 7) y + (h - 7) / 2 else y;
            drawText(pixels, OUTPUT_WIDTH, text_x, text_y, label, 0xF8FAFC, 1);
        }
    }

    fn drawCircleButton(
        pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8,
        cx: usize,
        cy: usize,
        r: usize,
        label: []const u8,
        pressed: bool,
    ) void {
        fillCircle(pixels, OUTPUT_WIDTH, cx, cy, r, if (pressed) 0x3B82F6 else 0x2A3B52);
        if (r > 2) {
            fillCircle(pixels, OUTPUT_WIDTH, cx, cy, r - 2, if (pressed) 0x60A5FA else 0x42556E);
        }
        const text_w = label.len * 6;
        drawText(
            pixels,
            OUTPUT_WIDTH,
            cx - @min(cx, text_w / 2),
            cy - 3,
            label,
            0xF8FAFC,
            1,
        );
    }

    fn drawText(
        pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8,
        width: usize,
        x: usize,
        y: usize,
        text: []const u8,
        rgb: u32,
        scale: usize,
    ) void {
        var pen_x = x;
        for (text) |c| {
            drawChar(pixels, width, pen_x, y, c, rgb, scale);
            pen_x += (5 + 1) * scale;
        }
    }

    fn drawChar(
        pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8,
        width: usize,
        x: usize,
        y: usize,
        c: u8,
        rgb: u32,
        scale: usize,
    ) void {
        const glyph = glyph5x7(std.ascii.toUpper(c));
        for (glyph, 0..) |row_bits, row| {
            var col: usize = 0;
            while (col < 5) : (col += 1) {
                if ((row_bits & (@as(u8, 1) << @intCast(4 - col))) == 0) continue;
                var dy: usize = 0;
                while (dy < scale) : (dy += 1) {
                    var dx: usize = 0;
                    while (dx < scale) : (dx += 1) {
                        setPixelRgb(
                            pixels,
                            width,
                            x + col * scale + dx,
                            y + row * scale + dy,
                            rgb,
                        );
                    }
                }
            }
        }
    }

    fn glyph5x7(c: u8) [7]u8 {
        return switch (c) {
            'A' => .{ 0b01110, 0b10001, 0b10001, 0b11111, 0b10001, 0b10001, 0b10001 },
            'B' => .{ 0b11110, 0b10001, 0b10001, 0b11110, 0b10001, 0b10001, 0b11110 },
            'C' => .{ 0b01110, 0b10001, 0b10000, 0b10000, 0b10000, 0b10001, 0b01110 },
            'D' => .{ 0b11110, 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b11110 },
            'E' => .{ 0b11111, 0b10000, 0b10000, 0b11110, 0b10000, 0b10000, 0b11111 },
            'F' => .{ 0b11111, 0b10000, 0b10000, 0b11110, 0b10000, 0b10000, 0b10000 },
            'G' => .{ 0b01110, 0b10001, 0b10000, 0b10111, 0b10001, 0b10001, 0b01110 },
            'H' => .{ 0b10001, 0b10001, 0b10001, 0b11111, 0b10001, 0b10001, 0b10001 },
            'I' => .{ 0b11111, 0b00100, 0b00100, 0b00100, 0b00100, 0b00100, 0b11111 },
            'J' => .{ 0b00111, 0b00010, 0b00010, 0b00010, 0b10010, 0b10010, 0b01100 },
            'K' => .{ 0b10001, 0b10010, 0b10100, 0b11000, 0b10100, 0b10010, 0b10001 },
            'L' => .{ 0b10000, 0b10000, 0b10000, 0b10000, 0b10000, 0b10000, 0b11111 },
            'M' => .{ 0b10001, 0b11011, 0b10101, 0b10101, 0b10001, 0b10001, 0b10001 },
            'N' => .{ 0b10001, 0b11001, 0b10101, 0b10011, 0b10001, 0b10001, 0b10001 },
            'O' => .{ 0b01110, 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b01110 },
            'P' => .{ 0b11110, 0b10001, 0b10001, 0b11110, 0b10000, 0b10000, 0b10000 },
            'Q' => .{ 0b01110, 0b10001, 0b10001, 0b10001, 0b10101, 0b10010, 0b01101 },
            'R' => .{ 0b11110, 0b10001, 0b10001, 0b11110, 0b10100, 0b10010, 0b10001 },
            'S' => .{ 0b01111, 0b10000, 0b10000, 0b01110, 0b00001, 0b00001, 0b11110 },
            'T' => .{ 0b11111, 0b00100, 0b00100, 0b00100, 0b00100, 0b00100, 0b00100 },
            'U' => .{ 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b01110 },
            'V' => .{ 0b10001, 0b10001, 0b10001, 0b10001, 0b10001, 0b01010, 0b00100 },
            'W' => .{ 0b10001, 0b10001, 0b10001, 0b10101, 0b10101, 0b10101, 0b01010 },
            'X' => .{ 0b10001, 0b10001, 0b01010, 0b00100, 0b01010, 0b10001, 0b10001 },
            'Y' => .{ 0b10001, 0b10001, 0b01010, 0b00100, 0b00100, 0b00100, 0b00100 },
            'Z' => .{ 0b11111, 0b00001, 0b00010, 0b00100, 0b01000, 0b10000, 0b11111 },
            '0' => .{ 0b01110, 0b10001, 0b10011, 0b10101, 0b11001, 0b10001, 0b01110 },
            '1' => .{ 0b00100, 0b01100, 0b00100, 0b00100, 0b00100, 0b00100, 0b01110 },
            '2' => .{ 0b01110, 0b10001, 0b00001, 0b00010, 0b00100, 0b01000, 0b11111 },
            '3' => .{ 0b11110, 0b00001, 0b00001, 0b01110, 0b00001, 0b00001, 0b11110 },
            '4' => .{ 0b00010, 0b00110, 0b01010, 0b10010, 0b11111, 0b00010, 0b00010 },
            '5' => .{ 0b11111, 0b10000, 0b10000, 0b11110, 0b00001, 0b00001, 0b11110 },
            '6' => .{ 0b01110, 0b10000, 0b10000, 0b11110, 0b10001, 0b10001, 0b01110 },
            '7' => .{ 0b11111, 0b00001, 0b00010, 0b00100, 0b01000, 0b01000, 0b01000 },
            '8' => .{ 0b01110, 0b10001, 0b10001, 0b01110, 0b10001, 0b10001, 0b01110 },
            '9' => .{ 0b01110, 0b10001, 0b10001, 0b01111, 0b00001, 0b00001, 0b01110 },
            '[' => .{ 0b01110, 0b01000, 0b01000, 0b01000, 0b01000, 0b01000, 0b01110 },
            ']' => .{ 0b01110, 0b00010, 0b00010, 0b00010, 0b00010, 0b00010, 0b01110 },
            '/' => .{ 0b00001, 0b00010, 0b00100, 0b01000, 0b10000, 0b00000, 0b00000 },
            '-' => .{ 0b00000, 0b00000, 0b00000, 0b11111, 0b00000, 0b00000, 0b00000 },
            ':' => .{ 0b00000, 0b00100, 0b00100, 0b00000, 0b00100, 0b00100, 0b00000 },
            '.' => .{ 0b00000, 0b00000, 0b00000, 0b00000, 0b00000, 0b00110, 0b00110 },
            ' ' => .{ 0, 0, 0, 0, 0, 0, 0 },
            else => .{ 0b11111, 0b00001, 0b00110, 0b00100, 0b00000, 0b00100, 0b00000 }, // '?'
        };
    }

    fn fillRect(
        pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8,
        width: usize,
        x: usize,
        y: usize,
        w: usize,
        h: usize,
        rgb: u32,
    ) void {
        var py = y;
        while (py < y + h and py < SCREEN_HEIGHT) : (py += 1) {
            var px = x;
            while (px < x + w and px < width) : (px += 1) {
                setPixelRgb(pixels, width, px, py, rgb);
            }
        }
    }

    fn fillCircle(
        pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8,
        width: usize,
        cx: usize,
        cy: usize,
        r: usize,
        rgb: u32,
    ) void {
        const r_i: i32 = @intCast(r);
        const cx_i: i32 = @intCast(cx);
        const cy_i: i32 = @intCast(cy);
        var dy: i32 = -r_i;
        while (dy <= r_i) : (dy += 1) {
            var dx: i32 = -r_i;
            while (dx <= r_i) : (dx += 1) {
                if (dx * dx + dy * dy > r_i * r_i) continue;
                const x = cx_i + dx;
                const y = cy_i + dy;
                if (x < 0 or y < 0) continue;
                setPixelRgb(pixels, width, @intCast(x), @intCast(y), rgb);
            }
        }
    }

    fn setPixelRgb(
        pixels: *[SCREEN_HEIGHT * OUTPUT_WIDTH * 4]u8,
        width: usize,
        x: usize,
        y: usize,
        rgb: u32,
    ) void {
        if (x >= width or y >= SCREEN_HEIGHT) return;
        const offset = (y * width + x) * 4;
        pixels[offset + 0] = @intCast((rgb >> 16) & 0xFF);
        pixels[offset + 1] = @intCast((rgb >> 8) & 0xFF);
        pixels[offset + 2] = @intCast(rgb & 0xFF);
        pixels[offset + 3] = 0xFF;
    }

    /// Enable/disable PPU (LCDC bit 7)
    pub fn setEnabled(self: *Ppu, enabled: bool) void {
        const was_enabled = self.enabled;
        self.enabled = enabled;

        if (!enabled) {
            self.ly = 0;
            self.mode = .OamSearch;
            self.mode_cycles = 0;
        } else if (!was_enabled) {
            // DMG LCD enable starts part-way into the first visible line.
            self.ly = 0;
            self.mode = .OamSearch;
            self.mode_cycles = 4;
        }
    }

    fn isPressed(keys: []const u8, scancode: usize) bool {
        return scancode < keys.len and keys[scancode] != 0;
    }
};
