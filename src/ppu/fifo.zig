const std = @import("std");

pub const Pixel = struct {
    color_id: u2,
};

pub const ObjectPixel = struct {
    color_id: u2 = 0,
    palette: u1 = 0,
    behind_background: bool = false,
};

/// Eight object pixels aligned with the next eight LCD positions. Sprites are
/// fetched in DMG priority order, so an opaque queued pixel is never replaced
/// by a later fetch; transparent holes may still be filled.
pub const ObjectFifo = struct {
    pixels: [8]ObjectPixel = [_]ObjectPixel{.{}} ** 8,

    pub fn clear(self: *ObjectFifo) void {
        @memset(&self.pixels, .{});
    }

    pub fn pop(self: *ObjectFifo) ObjectPixel {
        const pixel = self.pixels[0];
        for (0..7) |index| self.pixels[index] = self.pixels[index + 1];
        self.pixels[7] = .{};
        return pixel;
    }

    pub fn overlayRow(
        self: *ObjectFifo,
        low: u8,
        high: u8,
        horizontal_flip: bool,
        palette: u1,
        behind_background: bool,
        screen_offset: i8,
    ) void {
        for (0..8) |column| {
            const destination = screen_offset + @as(i8, @intCast(column));
            if (destination < 0 or destination >= 8) continue;

            const bit: u3 = if (horizontal_flip)
                @intCast(column)
            else
                @intCast(7 - column);
            const color_id: u2 = @intCast((((high >> bit) & 1) << 1) | ((low >> bit) & 1));
            if (color_id == 0) continue;

            const index: usize = @intCast(destination);
            if (self.pixels[index].color_id != 0) continue;
            self.pixels[index] = .{
                .color_id = color_id,
                .palette = palette,
                .behind_background = behind_background,
            };
        }
    }
};

/// DMG background/window pixels waiting to be sent to the LCD.
pub const PixelFifo = struct {
    pixels: [16]Pixel = [_]Pixel{.{ .color_id = 0 }} ** 16,
    head: u4 = 0,
    len: u5 = 0,

    pub fn clear(self: *PixelFifo) void {
        self.head = 0;
        self.len = 0;
    }

    pub fn pop(self: *PixelFifo) ?Pixel {
        if (self.len == 0) return null;
        const pixel = self.pixels[self.head];
        self.head +%= 1;
        self.len -= 1;
        return pixel;
    }

    fn pushRow(self: *PixelFifo, low: u8, high: u8) bool {
        if (self.len > 8) return false;

        for (0..8) |offset| {
            const bit: u3 = @intCast(7 - offset);
            const color_id: u2 = @intCast((((high >> bit) & 1) << 1) | ((low >> bit) & 1));
            const tail: u4 = @intCast((@as(u5, self.head) + self.len) & 0x0F);
            self.pixels[tail] = .{ .color_id = color_id };
            self.len += 1;
        }
        return true;
    }
};

const FetchStep = enum(u2) {
    tile,
    low,
    high,
    push,
};

/// Tile fetcher feeding the background FIFO. Each stage consumes two dots.
/// Window startup can begin at the low-byte stage because the tile lookup is
/// performed on the dot that detects WX, reproducing its nominal six-dot stall.
pub const BackgroundFetcher = struct {
    fifo: PixelFifo = .{},
    step: FetchStep = .tile,
    step_dot: u1 = 0,
    fetch_x: u5 = 0,
    tile_index: u8 = 0,
    tile_row: u3 = 0,
    tile_low: u8 = 0,
    tile_high: u8 = 0,
    using_window: bool = false,

    pub fn reset(self: *BackgroundFetcher, using_window: bool) void {
        self.fifo.clear();
        self.step = .tile;
        self.step_dot = 0;
        self.fetch_x = 0;
        self.tile_index = 0;
        self.tile_row = 0;
        self.tile_low = 0;
        self.tile_high = 0;
        self.using_window = using_window;
    }

    pub fn startWindow(self: *BackgroundFetcher, bus: anytype, lcdc: u8, window_line: u8) void {
        self.reset(true);
        self.fetchTile(bus, lcdc, 0, 0, 0, window_line);
        self.step = .low;
    }

    pub fn tick(
        self: *BackgroundFetcher,
        bus: anytype,
        lcdc: u8,
        ly: u8,
        scx: u8,
        scy: u8,
        window_line: u8,
    ) void {
        self.step_dot +%= 1;
        if (self.step_dot != 0) return;

        switch (self.step) {
            .tile => {
                self.fetchTile(bus, lcdc, ly, scx, scy, window_line);
                self.step = .low;
            },
            .low => {
                self.tile_low = bus.readVram(self.tileDataAddress(lcdc));
                self.step = .high;
            },
            .high => {
                self.tile_high = bus.readVram(self.tileDataAddress(lcdc) + 1);
                self.step = .push;
            },
            .push => {
                if (self.fifo.pushRow(self.tile_low, self.tile_high)) {
                    self.fetch_x +%= 1;
                    self.step = .tile;
                } else {
                    // A blocked push is retried every dot, not every two dots.
                    self.step_dot = 1;
                }
            },
        }
    }

    fn fetchTile(
        self: *BackgroundFetcher,
        bus: anytype,
        lcdc: u8,
        ly: u8,
        scx: u8,
        scy: u8,
        window_line: u8,
    ) void {
        const y = if (self.using_window) window_line else ly +% scy;
        const tile_x: u5 = if (self.using_window)
            self.fetch_x
        else
            @truncate((@as(u8, scx / 8) +% @as(u8, self.fetch_x)) & 0x1F);
        const map_base: u16 = if (self.using_window)
            (if ((lcdc & 0x40) != 0) 0x9C00 else 0x9800)
        else
            (if ((lcdc & 0x08) != 0) 0x9C00 else 0x9800);

        self.tile_index = bus.readVram(map_base + @as(u16, y / 8) * 32 + @as(u16, tile_x));
        self.tile_row = @truncate(y);
    }

    fn tileDataAddress(self: *const BackgroundFetcher, lcdc: u8) u16 {
        const tile_base: u16 = if ((lcdc & 0x10) != 0)
            0x8000 + @as(u16, self.tile_index) * 16
        else blk: {
            const signed_index: i16 = @as(i8, @bitCast(self.tile_index));
            break :blk @intCast(@as(i32, 0x9000) + @as(i32, signed_index) * 16);
        };
        return tile_base + @as(u16, self.tile_row) * 2;
    }
};

test "pixel FIFO preserves 2bpp left-to-right order" {
    var fifo: PixelFifo = .{};
    try std.testing.expect(fifo.pushRow(0b1010_0101, 0b1100_0011));

    const expected = [_]u2{ 3, 2, 1, 0, 0, 1, 2, 3 };
    for (expected) |color_id| {
        try std.testing.expectEqual(color_id, fifo.pop().?.color_id);
    }
    try std.testing.expectEqual(@as(?Pixel, null), fifo.pop());
}

test "object FIFO clips pixels and preserves earlier sprite priority" {
    var fifo: ObjectFifo = .{};
    fifo.overlayRow(0xFF, 0, false, 0, false, -2);
    fifo.overlayRow(0, 0xFF, false, 1, true, 0);

    // The clipped first sprite owns positions 0-5. The later sprite only fills
    // the still-transparent positions 6-7.
    for (0..6) |_| {
        const pixel = fifo.pop();
        try std.testing.expectEqual(@as(u2, 1), pixel.color_id);
        try std.testing.expectEqual(@as(u1, 0), pixel.palette);
    }
    for (0..2) |_| {
        const pixel = fifo.pop();
        try std.testing.expectEqual(@as(u2, 2), pixel.color_id);
        try std.testing.expectEqual(@as(u1, 1), pixel.palette);
        try std.testing.expect(pixel.behind_background);
    }
}

test "fetcher produces its first tile after eight dots" {
    const TestBus = struct {
        vram: [0x2000]u8 = [_]u8{0} ** 0x2000,

        fn readVram(self: *const @This(), address: u16) u8 {
            return self.vram[address - 0x8000];
        }
    };

    var bus: TestBus = .{};
    bus.vram[0x1800] = 0;
    bus.vram[0] = 0xFF;
    bus.vram[1] = 0;

    var fetcher: BackgroundFetcher = .{};
    fetcher.reset(false);
    for (0..7) |_| fetcher.tick(&bus, 0x91, 0, 0, 0, 0);
    try std.testing.expectEqual(@as(u5, 0), fetcher.fifo.len);
    fetcher.tick(&bus, 0x91, 0, 0, 0, 0);
    try std.testing.expectEqual(@as(u5, 8), fetcher.fifo.len);
}
