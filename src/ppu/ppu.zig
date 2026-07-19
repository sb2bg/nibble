const std = @import("std");
const Interrupt = @import("../memory/io.zig").Interrupt;

/// Game Boy screen dimensions
pub const SCREEN_WIDTH = 160;
pub const SCREEN_HEIGHT = 144;

/// PPU modes (STAT register bits 0-1)
pub const PpuMode = enum(u2) {
    HBlank = 0,
    VBlank = 1,
    OamSearch = 2,
    PixelTransfer = 3,
};

/// Logical DMG palette index after applying BGP/OBP mapping.
pub const DmgColor = enum(u2) {
    White = 0,
    LightGray = 1,
    DarkGray = 2,
    Black = 3,
};

/// Picture Processing Unit
pub const Ppu = struct {
    frame_buffer: [SCREEN_HEIGHT][SCREEN_WIDTH]DmgColor,

    mode: PpuMode,
    mode_cycles: u32,
    mode3_duration: u16,
    ly: u8,
    window_line: u8,
    enabled: bool,
    frame_ready: bool,

    pub fn init() Ppu {
        return Ppu{
            .frame_buffer = [_][SCREEN_WIDTH]DmgColor{[_]DmgColor{.White} ** SCREEN_WIDTH} ** SCREEN_HEIGHT,
            .mode = .VBlank,
            .mode_cycles = 0,
            .mode3_duration = 172,
            .ly = 0x91,
            .window_line = 0,
            .enabled = false,
            .frame_ready = false,
        };
    }

    pub fn reset(self: *Ppu) void {
        self.mode = .OamSearch;
        self.mode_cycles = 0;
        self.mode3_duration = 172;
        self.ly = 0;
        self.window_line = 0;
        self.enabled = false;
        self.frame_ready = true;
        @memset(&self.frame_buffer, [_]DmgColor{.White} ** SCREEN_WIDTH);
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
                        self.frame_ready = true;
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

    /// The frontend consumes this edge; framebuffer ownership stays in PPU.
    pub fn takeFrameReady(self: *Ppu) bool {
        const ready = self.frame_ready;
        self.frame_ready = false;
        return ready;
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
};

test "disabling LCD leaves PPU in HBlank mode" {
    var ppu = Ppu.init();
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
    var ppu = Ppu.init();
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
