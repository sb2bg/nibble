const std = @import("std");
const sdl = @import("../sdl.zig");
const IoReg = @import("../memory/io.zig").IoReg;
const Interrupt = @import("../memory/io.zig").Interrupt;

/// Game Boy screen dimensions
pub const SCREEN_WIDTH = 160;
pub const SCREEN_HEIGHT = 144;

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
            SCREEN_WIDTH * 4, // Scale 4x for visibility
            SCREEN_HEIGHT * 4,
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
            SCREEN_WIDTH,
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
                    }
                }
            }
        }
    }

    /// Present the frame buffer to the screen
    fn present(self: *Ppu) void {
        // Convert frame buffer to RGB888
        var pixels: [SCREEN_HEIGHT * SCREEN_WIDTH * 4]u8 = undefined;
        for (0..SCREEN_HEIGHT) |y| {
            for (0..SCREEN_WIDTH) |x| {
                const rgb = self.frame_buffer[y][x].toRgb();
                const offset = (y * SCREEN_WIDTH + x) * 4;
                pixels[offset + 0] = @intCast((rgb >> 16) & 0xFF); // R
                pixels[offset + 1] = @intCast((rgb >> 8) & 0xFF); // G
                pixels[offset + 2] = @intCast(rgb & 0xFF); // B
                pixels[offset + 3] = 0xFF; // A
            }
        }

        // Update texture
        if (self.texture) |tex| {
            sdl.updateTexture(tex, null, &pixels, SCREEN_WIDTH * 4) catch {};
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

    /// Check for SDL events (window close, etc)
    pub fn pollEvents(self: *Ppu) bool {
        _ = self;
        var event: sdl.Event = undefined;
        while (sdl.pollEvent(&event)) {
            if (event.type == sdl.QUIT) {
                return false; // Request quit
            }
        }
        return true; // Continue running
    }

    /// Enable/disable PPU (LCDC bit 7)
    pub fn setEnabled(self: *Ppu, enabled: bool) void {
        self.enabled = enabled;
        if (!enabled) {
            self.ly = 0;
            self.mode = .OamSearch;
            self.mode_cycles = 0;
        }
    }
};
