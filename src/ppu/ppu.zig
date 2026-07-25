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

const LineSprite = struct {
    oam_x: u8 = 0,
    oam_y: u8 = 0,
    tile: u8 = 0,
    attributes: u8 = 0,
    oam_index: u8 = 0,
};

const ObjectFetchPhase = enum(u3) {
    idle,
    alignment,
    tile,
    low,
    high,
};

/// Picture Processing Unit
pub const Ppu = struct {
    frame_buffer: [SCREEN_HEIGHT][SCREEN_WIDTH]DmgColor,

    // Pixel capture is a host observation policy, not emulated hardware state.
    // The fetcher, FIFOs, sprite arbitration, and timing continue to run when
    // this is false; only the final palette-mapped framebuffer stores are
    // omitted.
    capture_pixels: bool,

    mode: PpuMode,
    mode_cycles: u32,
    mode3_duration: u16,
    ly: u8,
    window_line: u8,
    enabled: bool,
    frame_ready: bool,
    lcd_startup: bool,
    line_start_delay: u8,
    mode0_stat_delay: u8,
    vblank_startup: bool,

    // Mode 3 pipeline state. These fields are intentionally persistent so a
    // save state taken mid-scanline resumes the same fetch/pop sequence.
    fetcher: fifo_mod.BackgroundFetcher,
    pixel_x: u16,
    startup_dots: u8,
    discard_pixels: u8,
    window_started: bool,
    window_drew_line: bool,
    object_fifo: fifo_mod.ObjectFifo,
    line_sprites: [10]LineSprite,
    line_sprite_count: u4,
    oam_scan_index: u6,
    next_sprite: u4,
    object_fetch_phase: ObjectFetchPhase,
    object_phase_dots: u2,
    object_align_dots: u4,
    object_sprite: LineSprite,
    object_address: u16,
    object_low: u8,
    last_object_tile: ?i16,

    pub fn init() Ppu {
        return Ppu{
            .frame_buffer = [_][SCREEN_WIDTH]DmgColor{[_]DmgColor{.White} ** SCREEN_WIDTH} ** SCREEN_HEIGHT,
            .capture_pixels = true,
            .mode = .VBlank,
            .mode_cycles = 0,
            .mode3_duration = 172,
            .ly = 0x91,
            .window_line = 0,
            .enabled = false,
            .frame_ready = false,
            .lcd_startup = false,
            .line_start_delay = 0,
            .mode0_stat_delay = 0,
            .vblank_startup = false,
            .fetcher = .{},
            .pixel_x = 0,
            .startup_dots = 0,
            .discard_pixels = 0,
            .window_started = false,
            .window_drew_line = false,
            .object_fifo = .{},
            .line_sprites = [_]LineSprite{.{}} ** 10,
            .line_sprite_count = 0,
            .oam_scan_index = 0,
            .next_sprite = 0,
            .object_fetch_phase = .idle,
            .object_phase_dots = 0,
            .object_align_dots = 0,
            .object_sprite = .{},
            .object_address = 0,
            .object_low = 0,
            .last_object_tile = null,
        };
    }

    pub fn reset(self: *Ppu) void {
        const capture_pixels = self.capture_pixels;
        self.mode = .OamSearch;
        self.mode_cycles = 0;
        self.mode3_duration = 172;
        self.ly = 0;
        self.window_line = 0;
        self.enabled = false;
        self.frame_ready = true;
        self.lcd_startup = false;
        self.line_start_delay = 0;
        self.mode0_stat_delay = 0;
        self.vblank_startup = false;
        self.fetcher.reset(false);
        self.pixel_x = 0;
        self.startup_dots = 0;
        self.discard_pixels = 0;
        self.window_started = false;
        self.window_drew_line = false;
        self.object_fifo.clear();
        @memset(&self.line_sprites, .{});
        self.line_sprite_count = 0;
        self.oam_scan_index = 0;
        self.next_sprite = 0;
        self.resetObjectFetcher();
        @memset(&self.frame_buffer, [_]DmgColor{.White} ** SCREEN_WIDTH);
        self.capture_pixels = capture_pixels;
    }

    pub fn setPixelCapture(self: *Ppu, enabled: bool) void {
        self.capture_pixels = enabled;
    }

    pub fn isPixelCaptureEnabled(self: *const Ppu) bool {
        return self.capture_pixels;
    }

    fn setMode(self: *Ppu, mode: PpuMode, bus: anytype) void {
        self.mode = mode;
        bus.io.setPpuMode(@intFromEnum(mode));
        switch (mode) {
            .HBlank, .VBlank => bus.io.setPpuMemoryBlocked(false, false),
            .OamSearch => bus.io.setPpuMemoryBlocked(true, false),
            .PixelTransfer => bus.io.setPpuMemoryBlocked(true, true),
        }
    }

    pub fn syncIoState(self: *const Ppu, bus: anytype) void {
        bus.io.setPpuMode(@intFromEnum(self.mode));
        switch (self.mode) {
            .HBlank, .VBlank => if (self.line_start_delay > 0)
                bus.io.setPpuMemoryBlockedDetailed(true, false, false, false)
            else
                bus.io.setPpuMemoryBlocked(false, false),
            .OamSearch => bus.io.setPpuMemoryBlocked(true, false),
            .PixelTransfer => bus.io.setPpuMemoryBlocked(true, true),
        }
    }

    pub fn tick(self: *Ppu, cycles: u32, bus: anytype) void {
        if (!self.enabled) return;

        // Most blanking dots only advance an internal counter. Jump across a
        // batch when no LY, STAT, mode, or bus-arbitration edge can occur in
        // it; batches touching an edge retain the dot reference path below.
        if (self.canSkipBlankDots(cycles)) {
            self.mode_cycles += cycles;
            return;
        }
        for (0..cycles) |_| self.tickDot(bus);
    }

    fn canSkipBlankDots(self: *const Ppu, cycles: u32) bool {
        return switch (self.mode) {
            .VBlank => self.mode_cycles + cycles < 456,
            .HBlank => blk: {
                if (self.vblank_startup or self.mode0_stat_delay != 0 or
                    self.lcd_startup or self.line_start_delay != 0)
                {
                    break :blk false;
                }
                const duration = 372 - @min(@as(u32, self.mode3_duration), 372);
                break :blk self.mode_cycles + cycles < duration;
            },
            .OamSearch, .PixelTransfer => false,
        };
    }

    fn tickDot(self: *Ppu, bus: anytype) void {
        self.mode_cycles += 1;

        switch (self.mode) {
            .OamSearch => {
                // CPU bus accesses are committed after their four-dot hook.
                // Keep the OAM row on the word scanned during that M-cycle;
                // advancing at the first dot shifts every corruption pattern.
                const row: u8 = @intCast(@min((self.mode_cycles - 1) / 4, 19));
                bus.io.setOamScanRow(row);
                if ((self.mode_cycles & 1) == 0 and self.oam_scan_index < 40) {
                    self.scanOamEntry(bus, self.oam_scan_index);
                    self.oam_scan_index += 1;
                }
                // The DMG seizes VRAM reads near the end of OAM scan, before
                // STAT changes to mode 3. Writes remain possible until mode 3.
                if (self.mode_cycles == 76) {
                    bus.io.setPpuMemoryBlockedDetailed(true, false, true, false);
                }
                if (self.mode_cycles == 80) self.beginPixelTransfer(bus);
            },
            .PixelTransfer => self.tickPixelTransfer(bus),
            .HBlank => {
                // Line 144 begins with a short mode-0 tail: IF rises after the
                // CPU sampling point at the line boundary, LY follows after
                // two dots, and the public mode becomes 1 after four dots.
                if (self.vblank_startup) {
                    if (self.mode_cycles == 2) bus.io.setLy(self.ly);
                    if (self.mode_cycles >= 4) {
                        self.vblank_startup = false;
                        self.setMode(.VBlank, bus);
                        self.frame_ready = true;
                    }
                    return;
                }

                if (self.mode0_stat_delay > 0) {
                    self.mode0_stat_delay -= 1;
                    if (self.mode0_stat_delay == 0) bus.io.releaseMode0Stat();
                }

                // A DMG starts LCD line 0 in mode 0, skips its OAM scan, and
                // enters mode 3 after 82 dots. This startup phase is distinct
                // from the HBlank at the end of a rendered line.
                if (self.lcd_startup) {
                    if (self.mode_cycles >= 82) {
                        self.lcd_startup = false;
                        self.beginPixelTransfer(bus);
                    }
                    return;
                }

                // LY changes before STAT exposes mode 2. The four-dot mode-0
                // phase at the head of each visible line is observable by CPU
                // reads and is part of the fixed 456-dot line budget.
                if (self.line_start_delay > 0) {
                    self.line_start_delay -= 1;
                    // Unlike lines 1-143, line 0 does not expose the mode-2
                    // interrupt source one dot before its public mode bits.
                    if (self.line_start_delay == 1 and self.ly != 0) {
                        bus.io.preassertMode2Stat();
                    }
                    if (self.line_start_delay == 0) self.beginOamSearch(bus);
                    return;
                }

                const hblank_duration = 372 - @min(@as(u32, self.mode3_duration), 372);
                if (self.mode_cycles >= hblank_duration) {
                    self.mode_cycles = 0;
                    self.ly += 1;

                    if (self.ly == 144) {
                        bus.io.preassertMode2StatLate();
                        bus.io.requestInterruptLate(Interrupt.VBLANK);
                        self.vblank_startup = true;
                    } else {
                        bus.io.beginVisibleLine(self.ly);
                        bus.io.setPpuMemoryBlockedDetailed(true, false, false, false);
                        self.line_start_delay = 4;
                    }
                }
            },
            .VBlank => {
                if (self.mode_cycles == 456) {
                    self.mode_cycles = 0;
                    self.ly += 1;

                    if (self.ly > 153) {
                        self.ly = 0;
                        self.window_line = 0;
                        // A normal frame's line 0 has the same four-dot
                        // HBlank-to-OAM bus phase as later visible lines. It
                        // was previously skipped here, shortening every frame
                        // by one M-cycle while leaving per-line tests green.
                        self.setMode(.HBlank, bus);
                        bus.io.beginVisibleLine(self.ly);
                        bus.io.setPpuMemoryBlockedDetailed(true, false, false, false);
                        self.line_start_delay = 4;
                    } else {
                        bus.io.setLy(self.ly);
                    }
                }
            },
        }
    }

    fn beginOamSearch(self: *Ppu, bus: anytype) void {
        self.mode_cycles = 0;
        self.oam_scan_index = 0;
        self.line_sprite_count = 0;
        bus.io.latchLyCoincidence();
        self.setMode(.OamSearch, bus);
    }

    fn scanOamEntry(self: *Ppu, bus: anytype, index: u6) void {
        if (self.line_sprite_count == self.line_sprites.len) return;

        const base: u16 = 0xFE00 + @as(u16, index) * 4;
        const oam_y = bus.readOam(base);
        const sprite_height: i16 = if ((bus.io.getLcdc() & 0x04) != 0) 16 else 8;
        const line = @as(i16, self.ly) - (@as(i16, oam_y) - 16);
        if (line < 0 or line >= sprite_height) return;

        const destination: usize = self.line_sprite_count;
        self.line_sprites[destination] = .{
            .oam_x = bus.readOam(base + 1),
            .oam_y = oam_y,
            .tile = bus.readOam(base + 2),
            .attributes = bus.readOam(base + 3),
            .oam_index = @intCast(index),
        };
        self.line_sprite_count += 1;
    }

    fn beginPixelTransfer(self: *Ppu, bus: anytype) void {
        self.mode_cycles = 0;
        self.pixel_x = 0;
        self.startup_dots = 12;
        self.discard_pixels = bus.io.getScx() & 0x07;
        self.window_started = false;
        self.window_drew_line = false;
        self.fetcher.reset(false);
        self.object_fifo.clear();
        self.next_sprite = 0;
        self.resetObjectFetcher();
        self.sortLineSprites();

        if (self.isWindowVisible(bus) and bus.io.getWx() <= 7) {
            self.fetcher.reset(true);
            self.window_started = true;
            self.discard_pixels = 7 - bus.io.getWx();
        }

        self.mode3_duration = 172 + @as(u16, bus.io.getScx() & 0x07);
        if (self.isWindowVisible(bus)) self.mode3_duration += 6;
        self.setMode(.PixelTransfer, bus);
    }

    fn tickPixelTransfer(self: *Ppu, bus: anytype) void {
        const lcdc = bus.io.getLcdc();

        if (self.tickObjectFetcher(bus, lcdc)) return;

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

        const window_start: u16 = if (bus.io.getWx() <= 7) 0 else bus.io.getWx() - 7;
        const window_stalls_next_dot = !self.window_started and
            self.isWindowVisible(bus) and self.pixel_x == window_start;
        const next_dot_finishes_line = self.pixel_x == SCREEN_WIDTH - 1 and
            self.startup_dots == 0 and
            self.object_fetch_phase == .idle and
            self.next_sprite >= self.line_sprite_count and
            self.fetcher.fifo.len > 0 and
            !window_stalls_next_dot;
        // STAT is read through the CPU bus latch. When the final mode-3 dot
        // falls on phase 3, that latch exposes the upcoming mode 0 one dot
        // before the renderer and video-memory locks actually transition.
        bus.io.setStatReadEarlyHblank(next_dot_finishes_line and
            ((self.mode_cycles + 1) & 0x03) == 3);

        if (self.pixel_x >= SCREEN_WIDTH and self.mode_cycles >= self.mode3_duration) {
            // A FIFO underflow may extend mode 3; keep the scanline total fixed
            // by shortening HBlank by the same amount.
            self.mode3_duration = @intCast(@min(self.mode_cycles, 376));
            if (self.window_drew_line) self.window_line +%= 1;
            self.mode_cycles = 0;
            const fine_scroll = bus.io.getScx() & 0x07;
            self.mode0_stat_delay = switch (fine_scroll) {
                1, 5 => 2,
                2, 6 => 1,
                else => 0,
            };
            if (self.mode0_stat_delay > 0) bus.io.suppressMode0Stat();
            self.setMode(.HBlank, bus);
        }
    }

    fn tickObjectFetcher(self: *Ppu, bus: anytype, lcdc: u8) bool {
        if (self.object_fetch_phase != .idle) {
            if ((lcdc & 0x02) == 0) {
                // DMG LCDC.1 changes can abort any unfinished object fetch.
                // Tile bytes are committed to the object FIFO only after the
                // high-byte phase, so a canceled fetch cannot reappear if
                // objects are enabled again later on the scanline.
                self.cancelObjectFetch();
                return false;
            }
            return self.advanceObjectFetch(bus, lcdc);
        }

        while (self.next_sprite < self.line_sprite_count) {
            const sprite = &self.line_sprites[self.next_sprite];
            if (sprite.oam_x > 167) {
                self.next_sprite += 1;
                continue;
            }

            const trigger_x: u16 = if (sprite.oam_x <= 8) 0 else sprite.oam_x - 8;
            if (trigger_x > self.pixel_x) return false;

            // Disabling objects cancels a fetch that has reached the current
            // LCD position. Re-enabling later must not resurrect that sprite.
            if ((lcdc & 0x02) == 0) {
                self.next_sprite += 1;
                continue;
            }
            self.beginObjectFetch(bus, sprite.*);
            self.next_sprite += 1;
            return self.advanceObjectFetch(bus, lcdc);
        }
        return false;
    }

    fn beginObjectFetch(self: *Ppu, bus: anytype, sprite: LineSprite) void {
        self.object_sprite = sprite;
        self.object_phase_dots = 0;
        self.object_align_dots = self.objectFetchPenalty(bus, sprite) - 6;
        self.object_fetch_phase = if (self.object_align_dots == 0) .tile else .alignment;
    }

    fn advanceObjectFetch(self: *Ppu, bus: anytype, lcdc: u8) bool {
        switch (self.object_fetch_phase) {
            .idle => return false,
            .alignment => {
                // Object and background fetching share one fetcher. During
                // alignment the background state machine may finish its
                // current stage, but LCD output remains stalled.
                self.fetcher.tick(
                    bus,
                    lcdc,
                    self.ly,
                    bus.io.getScx(),
                    bus.io.getScy(),
                    self.window_line,
                );
                self.object_align_dots -= 1;
                if (self.object_align_dots == 0) {
                    self.object_fetch_phase = .tile;
                    self.object_phase_dots = 0;
                }
            },
            .tile => {
                self.object_phase_dots += 1;
                if (self.object_phase_dots == 2) {
                    self.object_address = self.objectTileAddress(bus, self.object_sprite);
                    self.object_fetch_phase = .low;
                    self.object_phase_dots = 0;
                }
            },
            .low => {
                self.object_phase_dots += 1;
                if (self.object_phase_dots == 2) {
                    self.object_low = bus.readVram(self.object_address);
                    self.object_fetch_phase = .high;
                    self.object_phase_dots = 0;
                }
            },
            .high => {
                self.object_phase_dots += 1;
                if (self.object_phase_dots == 2) {
                    const high = bus.readVram(self.object_address + 1);
                    self.commitObjectRow(self.object_sprite, self.object_low, high);
                    self.object_fetch_phase = .idle;
                    self.object_phase_dots = 0;
                }
            },
        }
        return true;
    }

    fn objectTileAddress(self: *const Ppu, bus: anytype, sprite: LineSprite) u16 {
        const sprite_height: u8 = if ((bus.io.getLcdc() & 0x04) != 0) 16 else 8;
        var line = @as(i16, self.ly) - (@as(i16, sprite.oam_y) - 16);
        if ((sprite.attributes & 0x40) != 0) line = @as(i16, sprite_height) - 1 - line;

        var tile = sprite.tile;
        if (sprite_height == 16) {
            tile &= 0xFE;
            if (line >= 8) {
                tile +%= 1;
                line -= 8;
            }
        }

        return 0x8000 + @as(u16, tile) * 16 + @as(u16, @intCast(line)) * 2;
    }

    fn commitObjectRow(self: *Ppu, sprite: LineSprite, low: u8, high: u8) void {
        const screen_x = @as(i16, sprite.oam_x) - 8;
        const offset: i8 = @intCast(screen_x - @as(i16, @intCast(self.pixel_x)));
        self.object_fifo.overlayRow(
            low,
            high,
            (sprite.attributes & 0x20) != 0,
            @intCast((sprite.attributes >> 4) & 1),
            (sprite.attributes & 0x80) != 0,
            offset,
        );
    }

    fn objectFetchPenalty(self: *Ppu, bus: anytype, sprite: LineSprite) u4 {
        var penalty: u4 = 6;
        const tile: i16 = if (sprite.oam_x == 0)
            -1
        else blk: {
            const screen_x = @as(i16, sprite.oam_x) - 8;
            const window_start = @as(i16, bus.io.getWx()) - 7;
            const using_window = self.isWindowVisible(bus) and screen_x >= window_start;
            const fetch_x = if (using_window)
                screen_x - window_start
            else
                screen_x + @as(i16, bus.io.getScx());
            if (self.last_object_tile == null or
                self.last_object_tile.? != @divFloor(fetch_x, 8) +
                    (if (using_window) @as(i16, 0x100) else 0))
            {
                const pixel_in_tile: u4 = @intCast(@mod(fetch_x, 8));
                if (pixel_in_tile < 5) penalty += 5 - pixel_in_tile;
            }
            break :blk @divFloor(fetch_x, 8) +
                (if (using_window) @as(i16, 0x100) else 0);
        };

        if (sprite.oam_x == 0 and
            (self.last_object_tile == null or self.last_object_tile.? != tile))
        {
            penalty = 11;
        }
        self.last_object_tile = tile;
        return penalty;
    }

    fn cancelObjectFetch(self: *Ppu) void {
        self.object_fetch_phase = .idle;
        self.object_phase_dots = 0;
        self.object_align_dots = 0;
        self.object_low = 0;
    }

    fn resetObjectFetcher(self: *Ppu) void {
        self.cancelObjectFetch();
        self.object_sprite = .{};
        self.object_address = 0;
        self.last_object_tile = null;
    }

    fn outputPixel(self: *Ppu, bus: anytype, fetched_color_id: u2) void {
        const x: usize = @intCast(self.pixel_x);
        const lcdc = bus.io.getLcdc();
        const bg_enabled = (lcdc & 0x01) != 0;
        const color_id: u2 = if (bg_enabled) fetched_color_id else 0;

        if (self.capture_pixels) {
            const shift: u3 = @as(u3, color_id) * 2;
            self.frame_buffer[self.ly][x] = @enumFromInt((bus.io.getBgp() >> shift) & 0x03);
        }

        const object = self.object_fifo.pop();
        if ((lcdc & 0x02) != 0 and object.color_id != 0 and
            !(object.behind_background and color_id != 0))
        {
            if (self.capture_pixels) {
                const palette = if (object.palette == 0) bus.io.getObp0() else bus.io.getObp1();
                const object_shift: u3 = @as(u3, object.color_id) * 2;
                self.frame_buffer[self.ly][x] = @enumFromInt((palette >> object_shift) & 0x03);
            }
        }

        self.pixel_x += 1;
        if (self.fetcher.using_window) self.window_drew_line = true;
    }

    fn sortLineSprites(self: *Ppu) void {
        // DMG priority is lower X first, with OAM order breaking ties. The
        // mode-2 list is already in OAM order, so a stable insertion sort is
        // sufficient.
        var i: usize = 1;
        while (i < self.line_sprite_count) : (i += 1) {
            const sprite = self.line_sprites[i];
            var j = i;
            while (j > 0 and (self.line_sprites[j - 1].oam_x > sprite.oam_x or
                (self.line_sprites[j - 1].oam_x == sprite.oam_x and
                    self.line_sprites[j - 1].oam_index > sprite.oam_index))) : (j -= 1)
            {
                self.line_sprites[j] = self.line_sprites[j - 1];
            }
            self.line_sprites[j] = sprite;
        }
    }

    fn isWindowVisible(self: *const Ppu, bus: anytype) bool {
        const lcdc = bus.io.getLcdc();
        return (lcdc & 0x21) == 0x21 and
            self.ly >= bus.io.getWy() and
            bus.io.getWx() <= 166;
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
            self.lcd_startup = false;
            self.line_start_delay = 0;
            self.mode0_stat_delay = 0;
            self.vblank_startup = false;
            self.line_sprite_count = 0;
            self.oam_scan_index = 0;
            self.object_fifo.clear();
            self.resetObjectFetcher();
        } else if (!was_enabled) {
            self.ly = 0;
            self.mode = .HBlank;
            self.mode_cycles = 0;
            self.mode3_duration = 172;
            self.window_line = 0;
            self.lcd_startup = true;
            self.line_start_delay = 0;
            self.mode0_stat_delay = 0;
            self.vblank_startup = false;
            self.line_sprite_count = 0;
            self.oam_scan_index = 0;
            self.object_fifo.clear();
            self.resetObjectFetcher();
        }
    }

    pub fn isEnabled(self: *const Ppu) bool {
        return self.enabled;
    }
};

test "disabling LCD leaves PPU in HBlank mode" {
    var ppu = Ppu.init();
    ppu.mode = .PixelTransfer;
    ppu.setEnabled(false);

    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    try std.testing.expectEqual(@as(u8, 0), ppu.ly);
}

test "blanking fast path matches dot advancement away from edges" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const TestBus = struct {
        io: IoRegisters,

        pub fn readVram(_: *const @This(), _: u16) u8 {
            return 0;
        }

        pub fn readOam(_: *const @This(), _: u16) u8 {
            return 0;
        }
    };

    var fast_bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer fast_bus.io.deinit();
    var reference_bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer reference_bus.io.deinit();

    var fast = Ppu.init();
    fast.enabled = true;
    fast.mode = .HBlank;
    fast.mode_cycles = 10;
    fast.mode3_duration = 172;
    var reference = fast;

    fast.tick(100, &fast_bus);
    for (0..100) |_| reference.tickDot(&reference_bus);
    try std.testing.expectEqualDeep(reference, fast);

    fast.mode = .VBlank;
    fast.mode_cycles = 100;
    reference = fast;
    fast.tick(255, &fast_bus);
    for (0..255) |_| reference.tickDot(&reference_bus);
    try std.testing.expectEqualDeep(reference, fast);
}

test "DMG LCD startup skips mode 2 and phases later scanlines" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const TestBus = struct {
        io: IoRegisters,

        pub fn readVram(_: *const @This(), _: u16) u8 {
            return 0;
        }

        pub fn readOam(_: *const @This(), _: u16) u8 {
            return 0;
        }
    };

    var bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    var ppu = Ppu.init();
    ppu.setEnabled(true);

    ppu.tick(81, &bus);
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    ppu.tick(1, &bus);
    try std.testing.expectEqual(PpuMode.PixelTransfer, ppu.mode);

    ppu.tick(172, &bus);
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    ppu.tick(200, &bus);
    try std.testing.expectEqual(@as(u8, 1), ppu.ly);
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    ppu.tick(4, &bus);
    try std.testing.expectEqual(PpuMode.OamSearch, ppu.mode);
}

test "DMG phases line 144 interrupt and public mode edges" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const IoReg = @import("../memory/io.zig").IoReg;
    const TestBus = struct {
        io: IoRegisters,

        pub fn readVram(_: *const @This(), _: u16) u8 {
            return 0;
        }

        pub fn readOam(_: *const @This(), _: u16) u8 {
            return 0;
        }
    };

    var bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    var ppu = Ppu.init();
    ppu.enabled = true;
    ppu.mode = .HBlank;
    ppu.ly = 143;
    ppu.mode3_duration = 172;
    ppu.mode_cycles = 199;
    bus.io.setLy(143);
    bus.io.setPpuMode(@intFromEnum(PpuMode.HBlank));
    bus.io.data[@intFromEnum(IoReg.STAT)] |= 0x20;

    ppu.tick(1, &bus); // End line 143 and raise the late-dot requests.
    try std.testing.expect(ppu.vblank_startup);
    try std.testing.expectEqual(@as(u8, 143), bus.io.getLy());
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    try std.testing.expectEqual(
        Interrupt.VBLANK | Interrupt.LCD_STAT,
        bus.io.late_interrupts & (Interrupt.VBLANK | Interrupt.LCD_STAT),
    );

    ppu.tick(2, &bus);
    try std.testing.expectEqual(@as(u8, 144), bus.io.getLy());

    ppu.tick(2, &bus);
    try std.testing.expectEqual(PpuMode.VBlank, ppu.mode);
    try std.testing.expect(!ppu.vblank_startup);
    try std.testing.expectEqual(
        Interrupt.VBLANK | Interrupt.LCD_STAT,
        bus.io.data[@intFromEnum(IoReg.IF)] & (Interrupt.VBLANK | Interrupt.LCD_STAT),
    );
}

test "normal frame line zero keeps its four-dot mode 0 head" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const TestBus = struct {
        io: IoRegisters,

        pub fn readVram(_: *const @This(), _: u16) u8 {
            return 0;
        }

        pub fn readOam(_: *const @This(), _: u16) u8 {
            return 0;
        }
    };

    var bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    var ppu = Ppu.init();
    ppu.enabled = true;
    ppu.mode = .VBlank;
    ppu.ly = 153;
    ppu.mode_cycles = 455;
    bus.io.setLy(153);
    bus.io.setPpuMode(@intFromEnum(PpuMode.VBlank));

    ppu.tick(1, &bus);
    try std.testing.expectEqual(@as(u8, 0), ppu.ly);
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    try std.testing.expectEqual(@as(u8, 4), ppu.line_start_delay);

    ppu.tick(3, &bus);
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    ppu.tick(1, &bus);
    try std.testing.expectEqual(PpuMode.OamSearch, ppu.mode);
}

test "OAM scan row advances after each four-dot bus window" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const TestBus = struct {
        io: IoRegisters,

        pub fn readVram(_: *const @This(), _: u16) u8 {
            return 0;
        }

        pub fn readOam(_: *const @This(), _: u16) u8 {
            return 0;
        }
    };

    var bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    var ppu = Ppu.init();
    ppu.enabled = true;
    ppu.mode = .OamSearch;
    ppu.mode_cycles = 0;

    ppu.tick(4, &bus);
    try std.testing.expectEqual(@as(u8, 0), bus.io.getOamScanRow());
    ppu.tick(4, &bus);
    try std.testing.expectEqual(@as(u8, 1), bus.io.getOamScanRow());
}

test "mode 3 timing includes fine scroll, window, and visible objects" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const IoReg = @import("../memory/io.zig").IoReg;
    const TestBus = struct {
        io: IoRegisters,
    };

    var bus = TestBus{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    var ppu = Ppu.init();
    ppu.ly = 0;

    bus.io.data[@intFromEnum(IoReg.SCX)] = 5;
    ppu.beginPixelTransfer(&bus);
    try std.testing.expectEqual(@as(u16, 177), ppu.mode3_duration);

    bus.io.data[@intFromEnum(IoReg.LCDC)] |= 0x20;
    bus.io.data[@intFromEnum(IoReg.WY)] = 0;
    bus.io.data[@intFromEnum(IoReg.WX)] = 7;
    ppu.line_sprite_count = 0;
    ppu.beginPixelTransfer(&bus);
    try std.testing.expectEqual(@as(u16, 183), ppu.mode3_duration);

    bus.io.data[@intFromEnum(IoReg.LCDC)] |= 0x02;
    ppu.line_sprites[0] = .{ .oam_y = 16, .oam_x = 8 };
    ppu.line_sprite_count = 1;
    ppu.beginPixelTransfer(&bus);
    try std.testing.expectEqual(@as(u16, 183), ppu.mode3_duration);
}

test "mode 3 emits background pixels through the dot FIFO" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const TestBus = struct {
        io: IoRegisters,
        vram: [0x2000]u8 = [_]u8{0} ** 0x2000,
        oam: [0xA0]u8 = [_]u8{0} ** 0xA0,

        pub fn readVram(self: *const @This(), address: u16) u8 {
            return self.vram[address - 0x8000];
        }

        pub fn readOam(self: *const @This(), address: u16) u8 {
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
    ppu.tick(82, &bus);
    try std.testing.expectEqual(PpuMode.PixelTransfer, ppu.mode);

    ppu.tick(172, &bus);
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    try std.testing.expectEqual(@as(u16, SCREEN_WIDTH), ppu.pixel_x);
    try std.testing.expectEqual(DmgColor.Black, ppu.frame_buffer[0][0]);
}

test "mode 2 selects sprites and the object FIFO mixes them at output" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const IoReg = @import("../memory/io.zig").IoReg;
    const TestBus = struct {
        io: IoRegisters,
        vram: [0x2000]u8 = [_]u8{0} ** 0x2000,
        oam: [0xA0]u8 = [_]u8{0} ** 0xA0,

        pub fn readVram(self: *const @This(), address: u16) u8 {
            return self.vram[address - 0x8000];
        }

        pub fn readOam(self: *const @This(), address: u16) u8 {
            return self.oam[address - 0xFE00];
        }
    };

    var bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    bus.io.data[@intFromEnum(IoReg.LCDC)] |= 0x02;
    bus.io.data[@intFromEnum(IoReg.OBP0)] = 0xE4;
    bus.oam[0] = 16;
    bus.oam[1] = 8;
    bus.oam[2] = 1;
    bus.vram[16] = 0xFF;

    var ppu = Ppu.init();
    ppu.enabled = true;
    ppu.ly = 0;
    ppu.beginOamSearch(&bus);
    ppu.tick(80, &bus);
    try std.testing.expectEqual(@as(u4, 1), ppu.line_sprite_count);
    try std.testing.expectEqual(@as(u6, 40), ppu.oam_scan_index);
    try std.testing.expectEqual(PpuMode.PixelTransfer, ppu.mode);

    ppu.tick(183, &bus);
    try std.testing.expectEqual(PpuMode.HBlank, ppu.mode);
    try std.testing.expectEqual(DmgColor.LightGray, ppu.frame_buffer[0][0]);
}

test "object fetch reads tile bytes in phases and commits only when complete" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const IoReg = @import("../memory/io.zig").IoReg;
    const TestBus = struct {
        io: IoRegisters,
        vram: [0x2000]u8 = [_]u8{0} ** 0x2000,
        object_low_reads: u8 = 0,
        object_high_reads: u8 = 0,

        pub fn readVram(self: *@This(), address: u16) u8 {
            if (address == 0x8010) self.object_low_reads += 1;
            if (address == 0x8011) self.object_high_reads += 1;
            return self.vram[address - 0x8000];
        }
    };

    var bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    bus.io.data[@intFromEnum(IoReg.LCDC)] |= 0x02;
    bus.vram[0x10] = 0xFF;

    var ppu = Ppu.init();
    ppu.enabled = true;
    ppu.ly = 0;
    ppu.line_sprites[0] = .{ .oam_y = 16, .oam_x = 8, .tile = 1 };
    ppu.line_sprite_count = 1;
    ppu.beginPixelTransfer(&bus);

    // Five alignment dots may advance the BG fetcher, followed by the
    // two-dot tile phase. Neither object tile byte has been sampled yet.
    for (0..7) |_| try std.testing.expect(ppu.tickObjectFetcher(&bus, bus.io.getLcdc()));
    try std.testing.expectEqual(@as(u8, 0), bus.object_low_reads);
    try std.testing.expectEqual(@as(u8, 0), bus.object_high_reads);
    try std.testing.expectEqual(@as(u2, 0), ppu.object_fifo.pixels[0].color_id);

    for (0..2) |_| try std.testing.expect(ppu.tickObjectFetcher(&bus, bus.io.getLcdc()));
    try std.testing.expectEqual(@as(u8, 1), bus.object_low_reads);
    try std.testing.expectEqual(@as(u8, 0), bus.object_high_reads);
    try std.testing.expectEqual(@as(u2, 0), ppu.object_fifo.pixels[0].color_id);

    for (0..2) |_| try std.testing.expect(ppu.tickObjectFetcher(&bus, bus.io.getLcdc()));
    try std.testing.expectEqual(@as(u8, 1), bus.object_high_reads);
    try std.testing.expectEqual(@as(u2, 1), ppu.object_fifo.pixels[0].color_id);
    try std.testing.expectEqual(ObjectFetchPhase.idle, ppu.object_fetch_phase);
}

test "disabling objects cancels an in-flight fetch without stale pixels" {
    const IoRegisters = @import("../memory/io.zig").IoRegisters;
    const IoReg = @import("../memory/io.zig").IoReg;
    const TestBus = struct {
        io: IoRegisters,
        vram: [0x2000]u8 = [_]u8{0} ** 0x2000,

        pub fn readVram(self: *const @This(), address: u16) u8 {
            return self.vram[address - 0x8000];
        }
    };

    var bus: TestBus = .{ .io = IoRegisters.init(std.testing.allocator) };
    defer bus.io.deinit();
    bus.io.data[@intFromEnum(IoReg.LCDC)] |= 0x02;
    bus.vram[0x10] = 0xFF;

    var ppu = Ppu.init();
    ppu.enabled = true;
    ppu.ly = 0;
    ppu.line_sprites[0] = .{ .oam_y = 16, .oam_x = 8, .tile = 1 };
    ppu.line_sprite_count = 1;
    ppu.beginPixelTransfer(&bus);

    for (0..9) |_| try std.testing.expect(ppu.tickObjectFetcher(&bus, bus.io.getLcdc()));
    bus.io.data[@intFromEnum(IoReg.LCDC)] &= ~@as(u8, 0x02);
    try std.testing.expect(!ppu.tickObjectFetcher(&bus, bus.io.getLcdc()));
    try std.testing.expectEqual(ObjectFetchPhase.idle, ppu.object_fetch_phase);

    bus.io.data[@intFromEnum(IoReg.LCDC)] |= 0x02;
    try std.testing.expect(!ppu.tickObjectFetcher(&bus, bus.io.getLcdc()));
    for (ppu.object_fifo.pixels) |pixel| {
        try std.testing.expectEqual(@as(u2, 0), pixel.color_id);
    }
}
