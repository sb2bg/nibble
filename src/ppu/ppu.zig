const std = @import("std");
const Interrupt = @import("../memory/io.zig").Interrupt;
const fifo_mod = @import("fifo.zig");

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

    // Mode 3 pipeline state. These fields are intentionally persistent so a
    // save state taken mid-scanline resumes the same fetch/pop sequence.
    fetcher: fifo_mod.BackgroundFetcher,
    pixel_x: u16,
    startup_dots: u8,
    discard_pixels: u8,
    window_started: bool,
    window_drew_line: bool,
    bg_color_ids: [SCREEN_WIDTH]u2,
    sprite_stalls: [SCREEN_WIDTH]u8,

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
            .fetcher = .{},
            .pixel_x = 0,
            .startup_dots = 0,
            .discard_pixels = 0,
            .window_started = false,
            .window_drew_line = false,
            .bg_color_ids = [_]u2{0} ** SCREEN_WIDTH,
            .sprite_stalls = [_]u8{0} ** SCREEN_WIDTH,
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
        self.fetcher.reset(false);
        self.pixel_x = 0;
        self.startup_dots = 0;
        self.discard_pixels = 0;
        self.window_started = false;
        self.window_drew_line = false;
        @memset(&self.bg_color_ids, 0);
        @memset(&self.sprite_stalls, 0);
        @memset(&self.frame_buffer, [_]DmgColor{.White} ** SCREEN_WIDTH);
    }

    fn setMode(self: *Ppu, mode: PpuMode, bus: anytype) void {
        self.mode = mode;
        bus.io.setPpuMode(@intFromEnum(mode));
    }

    pub fn tick(self: *Ppu, cycles: u32, bus: anytype) void {
        if (!self.enabled) return;
        for (0..cycles) |_| self.tickDot(bus);
    }

    fn tickDot(self: *Ppu, bus: anytype) void {
        self.mode_cycles += 1;

        switch (self.mode) {
            .OamSearch => {
                const row: u8 = @intCast(@min(self.mode_cycles / 4, 19));
                bus.io.setOamScanRow(row);
                if (self.mode_cycles == 80) self.beginPixelTransfer(bus);
            },
            .PixelTransfer => self.tickPixelTransfer(bus),
            .HBlank => {
                const hblank_duration = 376 - @min(@as(u32, self.mode3_duration), 376);
                if (self.mode_cycles >= hblank_duration) {
                    self.mode_cycles = 0;
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
                if (self.mode_cycles == 456) {
                    self.mode_cycles = 0;
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

    fn beginPixelTransfer(self: *Ppu, bus: anytype) void {
        self.mode_cycles = 0;
        self.pixel_x = 0;
        self.startup_dots = 12;
        self.discard_pixels = bus.io.getScx() & 0x07;
        self.window_started = false;
        self.window_drew_line = false;
        @memset(&self.bg_color_ids, 0);
        @memset(&self.sprite_stalls, 0);
        self.fetcher.reset(false);

        const lcdc = bus.io.getLcdc();
        if (self.isWindowVisible(bus) and bus.io.getWx() <= 7) {
            self.fetcher.reset(true);
            self.window_started = true;
            self.discard_pixels = 7 - bus.io.getWx();
        }

        self.mode3_duration = 172 + @as(u16, bus.io.getScx() & 0x07);
        if (self.isWindowVisible(bus)) self.mode3_duration += 6;
        if ((lcdc & 0x02) != 0) {
            self.mode3_duration += self.collectSpritePenalties(
                bus,
                &self.sprite_stalls,
                289 - self.mode3_duration,
            );
        }
        self.setMode(.PixelTransfer, bus);
    }

    fn tickPixelTransfer(self: *Ppu, bus: anytype) void {
        const lcdc = bus.io.getLcdc();

        if (self.pixel_x < SCREEN_WIDTH and self.sprite_stalls[self.pixel_x] > 0) {
            self.sprite_stalls[self.pixel_x] -= 1;
            return;
        }

        if (!self.window_started and self.isWindowVisible(bus)) {
            const window_start: u16 = if (bus.io.getWx() <= 7) 0 else bus.io.getWx() - 7;
            if (self.pixel_x == window_start) {
                self.fetcher.startWindow(bus, lcdc, self.window_line);
                self.window_started = true;
            }
        }

        if (self.pixel_x < SCREEN_WIDTH) {
            self.fetcher.tick(
                bus,
                lcdc,
                self.ly,
                bus.io.getScx(),
                bus.io.getScy(),
                self.window_line,
            );

            if (self.startup_dots > 0) {
                self.startup_dots -= 1;
            } else if (self.fetcher.fifo.pop()) |pixel| {
                if (self.discard_pixels > 0) {
                    self.discard_pixels -= 1;
                } else {
                    self.outputPixel(bus, pixel.color_id);
                }
            }
        }

        if (self.pixel_x >= SCREEN_WIDTH and self.mode_cycles >= self.mode3_duration) {
            // A FIFO underflow may extend mode 3; keep the scanline total fixed
            // by shortening HBlank by the same amount.
            self.mode3_duration = @intCast(@min(self.mode_cycles, 376));
            self.renderSprites(bus);
            if (self.window_drew_line) self.window_line +%= 1;
            self.mode_cycles = 0;
            self.setMode(.HBlank, bus);
        }
    }

    fn outputPixel(self: *Ppu, bus: anytype, fetched_color_id: u2) void {
        const x: usize = @intCast(self.pixel_x);
        const bg_enabled = (bus.io.getLcdc() & 0x01) != 0;
        const color_id: u2 = if (bg_enabled) fetched_color_id else 0;
        self.bg_color_ids[x] = color_id;

        const shift: u3 = @as(u3, color_id) * 2;
        self.frame_buffer[self.ly][x] = @enumFromInt((bus.io.getBgp() >> shift) & 0x03);
        self.pixel_x += 1;
        if (self.fetcher.using_window) self.window_drew_line = true;
    }

    /// Mix the selected DMG objects over the background pixels already emitted
    /// by the FIFO. Object fetching itself is still represented by dot stalls;
    /// a dedicated object FIFO is the next fidelity boundary.
    fn renderSprites(self: *Ppu, bus: anytype) void {
        if (self.ly >= SCREEN_HEIGHT) return;

        const lcdc = bus.io.getLcdc();
        if ((lcdc & 0x02) == 0) return;

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

            for (scanline_sprites[0..sprite_count]) |sprite| {
                if (screen_x < sprite.x or screen_x >= sprite.x + 8) continue;

                var line = @as(i16, self.ly) - sprite.y;
                if ((sprite.attr & 0x40) != 0) line = @as(i16, sprite_height) - 1 - line;

                var sprite_x = screen_x - sprite.x;
                if ((sprite.attr & 0x20) != 0) sprite_x = 7 - sprite_x;

                var tile_num = sprite.tile;
                if (sprite_height == 16) {
                    tile_num &= 0xFE;
                    if (line >= 8) {
                        tile_num +%= 1;
                        line -= 8;
                    }
                }

                const tile_addr: u16 =
                    0x8000 + @as(u16, tile_num) * 16 + @as(u16, @intCast(line)) * 2;
                const low = bus.readVram(tile_addr);
                const high = bus.readVram(tile_addr + 1);
                const bit: u3 = @intCast(7 - @as(u8, @intCast(sprite_x)));
                const color_id: u2 = @intCast((((high >> bit) & 1) << 1) | ((low >> bit) & 1));
                if (color_id == 0) continue;

                if (!best_found or sprite.x < best_sprite_x or
                    (sprite.x == best_sprite_x and sprite.index < best_oam_index))
                {
                    best_found = true;
                    best_sprite_x = sprite.x;
                    best_oam_index = sprite.index;
                    best_attr = sprite.attr;
                    best_color_id = color_id;
                }
            }

            if (!best_found) continue;
            if ((best_attr & 0x80) != 0 and self.bg_color_ids[x] != 0) continue;

            const palette = if ((best_attr & 0x10) != 0) obp1 else obp0;
            const shift: u3 = @as(u3, best_color_id) * 2;
            self.frame_buffer[self.ly][x] = @enumFromInt((palette >> shift) & 0x03);
        }
    }

    fn isWindowVisible(self: *const Ppu, bus: anytype) bool {
        const lcdc = bus.io.getLcdc();
        return (lcdc & 0x21) == 0x21 and
            self.ly >= bus.io.getWy() and
            bus.io.getWx() <= 166;
    }

    /// Calculate mode 3 timing from the same penalty schedule used by the
    /// dot pipeline. Kept separate from `beginPixelTransfer` for focused tests.
    fn calculateMode3Duration(self: *const Ppu, bus: anytype) u16 {
        const lcdc = bus.io.getLcdc();
        var duration: u16 = 172 + @as(u16, bus.io.getScx() & 0x07);

        if (self.isWindowVisible(bus)) duration += 6;
        if ((lcdc & 0x02) == 0) return duration;

        duration += self.collectSpritePenalties(bus, null, 289 - duration);
        return duration;
    }

    fn collectSpritePenalties(
        self: *const Ppu,
        bus: anytype,
        stalls: ?*[SCREEN_WIDTH]u8,
        max_penalty: u16,
    ) u16 {
        const lcdc = bus.io.getLcdc();

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

        var total: u16 = 0;
        var last_tile: ?i16 = null;
        for (sprite_xs[0..sprite_count]) |oam_x| {
            var penalty: u16 = 0;
            var stall_x: usize = 0;

            if (oam_x == 0) {
                penalty = 11;
            } else {
                if (oam_x > 167) continue;

                const screen_x = @as(i16, oam_x) - 8;
                stall_x = @intCast(@max(screen_x, 0));
                const window_start = @as(i16, bus.io.getWx()) - 7;
                const using_window = self.isWindowVisible(bus) and screen_x >= window_start;
                const fetch_x = if (using_window)
                    screen_x - window_start
                else
                    screen_x + @as(i16, bus.io.getScx());
                // Keep background and window tile identities separate even
                // when their numeric X coordinates happen to match.
                const tile = @divFloor(fetch_x, 8) + (if (using_window) @as(i16, 0x100) else 0);
                penalty = 6;
                if (last_tile == null or last_tile.? != tile) {
                    const pixel_in_tile: u4 = @intCast(@mod(fetch_x, 8));
                    if (pixel_in_tile < 5) penalty += 5 - pixel_in_tile;
                    last_tile = tile;
                }
            }

            const accepted = @min(penalty, max_penalty - total);
            if (stalls) |schedule| schedule[stall_x] +|= @intCast(accepted);
            total += accepted;
            if (total == max_penalty) break;
        }

        return total;
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

test "mode 3 emits background pixels through the dot FIFO" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const TestBus = struct {
        io: IoRegisters,
        vram: [0x2000]u8 = [_]u8{0} ** 0x2000,
        oam: [0xA0]u8 = [_]u8{0} ** 0xA0,

        fn readVram(self: *const @This(), address: u16) u8 {
            return self.vram[address - 0x8000];
        }

        fn readOam(self: *const @This(), address: u16) u8 {
            return self.oam[address - 0xFE00];
        }
    };

    var bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    // Tile zero is color ID 1 across the row; the post-boot BGP maps it to
    // shade 3, making it easy to verify that fetch and palette stages ran.
    bus.vram[0] = 0xFF;

    var ppu = Ppu.init();
    ppu.setEnabled(true);
    ppu.tick(76, &bus);
    try std.testing.expectEqual(PpuMode.PixelTransfer, ppu.mode);

    ppu.tick(172, &bus);
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    try std.testing.expectEqual(@as(u16, SCREEN_WIDTH), ppu.pixel_x);
    try std.testing.expectEqual(@as(u2, 1), ppu.bg_color_ids[0]);
    try std.testing.expectEqual(DmgColor.Black, ppu.frame_buffer[0][0]);
}
