const std = @import("std");
const IoRegisters = @import("io.zig").IoRegisters;
const IoReg = @import("io.zig").IoReg;
const Cartridge = @import("../cartridge/cartridge.zig").Cartridge;
const Timer = @import("../timer.zig").Timer;
const Serial = @import("../serial.zig").Serial;
const Apu = @import("../apu.zig").Apu;

/// OAM DMA progresses one byte per M-cycle. On DMG, the CPU can only reach
/// HRAM while a transfer is active; DMA's own source reads bypass that lockout.
pub const Dma = struct {
    active: bool = false,
    source: u16 = 0,
    next_byte: u8 = 0,
    cycles_until_copy: u3 = 0,
    pending_source: u16 = 0,
    start_delay: u4 = 0,

    fn start(self: *Dma, source_high: u8) void {
        // The write occupies M0, M1 leaves the previous DMA state intact, and
        // the new transfer becomes active for accesses in M2. A restart must
        // therefore retain the old active transfer during this delay.
        self.pending_source = @as(u16, source_high) << 8;
        self.start_delay = 8;
    }
};

/// Memory Bus - handles all memory reads and writes
pub const Bus = struct {
    pub const CycleHook = struct {
        context: *anyopaque,
        tickFn: *const fn (*anyopaque, u8) void,
    };

    // Memory regions
    wram: [0x2000]u8, // Work RAM (0xC000-0xDFFF)
    hram: [0x7F]u8, // High RAM (0xFF80-0xFFFE)
    oam: [0xA0]u8, // OAM (0xFE00-0xFE9F)
    vram: [0x2000]u8, // Video RAM (0x8000-0x9FFF)

    // I/O and interrupts
    io: IoRegisters,
    ie_register: u8, // Interrupt Enable (0xFFFF)
    timer: Timer,
    serial: Serial,
    apu: Apu,
    dma: Dma,

    // Cartridge (owns ROM + RAM + MBC)
    cartridge: Cartridge,

    // Optional hook invoked on each CPU memory access (one M-cycle = 4 T-cycles)
    cycle_hook: ?CycleHook,

    pub fn init(allocator: std.mem.Allocator, cartridge: Cartridge) Bus {
        return Bus{
            .wram = [_]u8{0} ** 0x2000,
            .hram = [_]u8{0} ** 0x7F,
            .oam = [_]u8{0} ** 0xA0,
            .vram = [_]u8{0} ** 0x2000,
            .io = IoRegisters.init(allocator),
            .ie_register = 0,
            .timer = Timer.init(),
            .serial = .{},
            .apu = Apu.init(),
            .dma = .{},
            .cartridge = cartridge,
            .cycle_hook = null,
        };
    }

    pub fn deinit(self: *Bus) void {
        self.io.deinit();
        self.cartridge.deinit();
    }

    /// Reset bus state (keeps cartridge loaded)
    pub fn reset(self: *Bus) void {
        @memset(&self.wram, 0);
        @memset(&self.hram, 0);
        @memset(&self.oam, 0);
        @memset(&self.vram, 0);
        self.io.reset();
        self.ie_register = 0;
        self.timer.reset(&self.io);
        self.serial.reset();
        self.apu.reset();
        self.dma = .{};
        self.cartridge.mbc.reset();
    }

    pub fn setCycleHook(self: *Bus, hook: ?CycleHook) void {
        self.cycle_hook = hook;
    }

    pub fn tickInternal(self: *const Bus, cycles: u8) void {
        if (cycles == 0) return;
        if (self.cycle_hook) |hook| {
            hook.tickFn(hook.context, cycles);
        }
    }

    pub fn tickTimer(self: *Bus, cycles: u8) void {
        self.timer.tick(cycles, &self.io);
    }

    pub fn tickApu(self: *Bus, cycles: u8, divider_start: u16, capture_samples: bool) void {
        self.apu.tickWithSampleCapture(cycles, divider_start, capture_samples);
    }

    pub fn tickDma(self: *Bus, cycles: u8) void {
        if (!self.dma.active and self.dma.start_delay == 0) return;

        var remaining = cycles;
        while (remaining > 0) : (remaining -= 1) {
            if (self.dma.start_delay != 0) {
                self.dma.start_delay -= 1;
                if (self.dma.start_delay == 0) {
                    self.dma.active = true;
                    self.dma.source = self.dma.pending_source;
                    self.dma.next_byte = 0;
                    // The bus becomes unavailable in M2; byte zero completes
                    // one M-cycle later, preserving the full 160-M-cycle
                    // transfer lifetime after activation.
                    self.dma.cycles_until_copy = 4;
                    continue;
                }
            }

            if (!self.dma.active) continue;
            self.dma.cycles_until_copy -= 1;
            if (self.dma.cycles_until_copy != 0) continue;

            self.copyDmaByte();
        }
    }

    fn copyDmaByte(self: *Bus) void {
        const index = self.dma.next_byte;
        const raw_source = self.dma.source + @as(u16, index);
        // The DMG DMA address decoder mirrors E000-FFFF onto C000-DFFF.
        // This includes nominal OAM/IO source pages FE and FF; treating them as
        // normal CPU addresses incorrectly feeds OAM or register values back
        // into the transfer.
        const source = if (raw_source >= 0xE000) raw_source - 0x2000 else raw_source;
        self.oam[index] = self.readNoTick(source);

        if (index == 0x9F) {
            self.dma.active = false;
        } else {
            self.dma.next_byte += 1;
            self.dma.cycles_until_copy = 4;
        }
    }

    pub fn tickSerial(self: *Bus, cycles: u8) void {
        self.serial.tick(cycles, &self.io);
    }

    /// STOP asserts the same divider-reset signal as a DIV write without
    /// performing an additional CPU bus access.
    pub fn enterStopMode(self: *Bus) void {
        self.apu.dividerReset(self.timer.system_counter);
        self.timer.writeRegister(&self.io, @intFromEnum(IoReg.DIV), 0);
    }

    pub fn triggerOamBugWriteIdu(self: *Bus, addr: u16) void {
        if (!isOamAddress(addr) or !self.isPpuInMode2()) return;
        const row = self.io.getOamScanRow();
        if (row >= 19) return;
        self.applyOamWriteCorruption(nextOamScanRow(row));
    }

    inline fn tickAccess(self: *const Bus) void {
        if (self.cycle_hook) |hook| {
            hook.tickFn(hook.context, 4);
        }
    }

    inline fn isPpuInMode2(self: *const Bus) bool {
        return self.io.getPpuMode() == 2;
    }

    inline fn isPpuOamReadBlocked(self: *const Bus) bool {
        return self.io.ppu_oam_read_blocked;
    }

    inline fn isPpuOamWriteBlocked(self: *const Bus) bool {
        return self.io.ppu_oam_write_blocked;
    }

    inline fn isPpuVramReadBlocked(self: *const Bus) bool {
        return self.io.ppu_vram_read_blocked;
    }

    inline fn isPpuVramWriteBlocked(self: *const Bus) bool {
        return self.io.ppu_vram_write_blocked;
    }

    inline fn isOamAddress(addr: u16) bool {
        return addr >= 0xFE00 and addr <= 0xFEFF;
    }

    inline fn nextOamScanRow(current: u8) u8 {
        return @min(current + 1, 19);
    }

    inline fn getOamWord(self: *const Bus, row: u8, word: u8) u16 {
        const index = @as(usize, row) * 8 + @as(usize, word) * 2;
        const lo = self.oam[index];
        const hi = self.oam[index + 1];
        return (@as(u16, hi) << 8) | lo;
    }

    inline fn setOamWord(self: *Bus, row: u8, word: u8, value: u16) void {
        const index = @as(usize, row) * 8 + @as(usize, word) * 2;
        self.oam[index] = @truncate(value);
        self.oam[index + 1] = @truncate(value >> 8);
    }

    fn copyOamTailFromPrevRow(self: *Bus, row: u8) void {
        if (row == 0 or row >= 20) return;
        var word: u8 = 1;
        while (word < 4) : (word += 1) {
            const prev = self.getOamWord(row - 1, word);
            self.setOamWord(row, word, prev);
        }
    }

    fn copyOamRow(self: *Bus, dst_row: u8, src_row: u8) void {
        if (dst_row >= 20 or src_row >= 20) return;
        var word: u8 = 0;
        while (word < 4) : (word += 1) {
            self.setOamWord(dst_row, word, self.getOamWord(src_row, word));
        }
    }

    // Pattern for write-related corruption (INC/DEC rp, writes in mode 2).
    fn applyOamWriteCorruption(self: *Bus, row: u8) void {
        if (row == 0 or row >= 20) return;

        const a = self.getOamWord(row, 0);
        const b = self.getOamWord(row - 1, 0);
        const c = self.getOamWord(row - 1, 2);

        self.setOamWord(row, 0, ((a ^ c) & (b ^ c)) ^ c);
        self.copyOamTailFromPrevRow(row);
    }

    // OAM reads feed the scan row through several overlapping latches. Which
    // older rows are overwritten depends on the low five bits of the PPU's
    // byte-row address, so reducing this to the normal b | (a & c) formula
    // misses the distinctive POP and HL+/- corruption patterns.
    fn applyOamReadCorruption(self: *Bus, row: u8) void {
        if (row == 0 or row >= 20) return;

        const byte_row = row * 8;
        switch (byte_row & 0x18) {
            0x10 => {
                // Secondary corruption also copies the preceding row two
                // rows back before the normal final row copy below.
                if (row < 19) {
                    const a = self.getOamWord(row - 2, 0);
                    const b = self.getOamWord(row - 1, 0);
                    const c = self.getOamWord(row, 0);
                    const d = self.getOamWord(row - 1, 2);
                    self.setOamWord(row - 1, 0, (b & (a | c | d)) | (a & c & d));
                    self.copyOamRow(row - 2, row - 1);
                }
            },
            0x00 => {
                // These rows reach four rows back. The 0x40 byte row has an
                // additional DMG-specific latch equation; 0x20 and 0x60 use
                // their measured tertiary variants.
                if (row >= 4 and row < 19) {
                    const a = self.getOamWord(row, 0);
                    const b = self.getOamWord(row - 1, 2);
                    const c = self.getOamWord(row - 1, 0);
                    const d = self.getOamWord(row - 2, 0);
                    const e = self.getOamWord(row - 4, 0);
                    const merged = if (byte_row == 0x40) blk: {
                        const f = self.getOamWord(row - 2, 1);
                        const g = self.getOamWord(row - 2, 0);
                        const h = self.getOamWord(row - 4, 0);
                        break :blk (e & (h | g | (~d & f) | c | b)) | (c & g & h);
                    } else if (byte_row == 0x20)
                        (c & (a | b | d | e)) | (a & b & d & e)
                    else if (byte_row == 0x60)
                        (c & (a | b | d | e)) | (b & d & e)
                    else
                        c | (a & b & d & e);

                    self.setOamWord(row - 1, 0, merged);
                    self.copyOamRow(row - 2, row - 1);
                    self.copyOamRow(row - 4, row - 1);
                }
            },
            else => {
                const a = self.getOamWord(row, 0);
                const b = self.getOamWord(row - 1, 0);
                const c = self.getOamWord(row - 1, 2);
                const merged = b | (a & c);
                self.setOamWord(row - 1, 0, merged);
                self.setOamWord(row, 0, merged);
            },
        }

        self.copyOamRow(row, row - 1);
        // On the tested DMG revision, byte row 0x80 is also fed back into the
        // first OAM row.
        if (byte_row == 0x80) self.copyOamRow(0, row);
    }

    /// Get serial output for test ROMs
    pub fn getSerialOutput(self: *const Bus) []const u8 {
        return self.io.getSerialOutput();
    }

    /// Read from VRAM (for PPU)
    pub fn readVram(self: *const Bus, addr: u16) u8 {
        if (addr >= 0x8000 and addr <= 0x9FFF) {
            return self.vram[addr - 0x8000];
        }
        return 0xFF;
    }

    /// Read from OAM directly (for PPU sprite rendering)
    pub fn readOam(self: *const Bus, addr: u16) u8 {
        if (addr >= 0xFE00 and addr <= 0xFE9F) {
            return self.oam[addr - 0xFE00];
        }
        return 0xFF;
    }

    pub fn read(self: *const Bus, addr: u16) u8 {
        return self.readInternal(addr, true);
    }

    /// Read without consuming a CPU bus cycle. Intended for debugger and
    /// automation observations; unlike a CPU access it cannot trigger OAM
    /// corruption or be rejected by CPU-only DMA/PPU arbitration.
    pub fn peek(self: *const Bus, addr: u16) u8 {
        return self.readInternal(addr, false);
    }

    fn readNoTick(self: *const Bus, addr: u16) u8 {
        return self.readInternal(addr, false);
    }

    fn readInternal(self: *const Bus, addr: u16, count_cycle: bool) u8 {
        if (count_cycle) self.tickAccess();

        // FF46 is wired to its own latch and remains readable while DMA owns
        // the main bus. A VRAM-source transfer occupies the video bus, so the
        // CPU can still reach WRAM and its echo; OAM remains unavailable as the
        // DMA destination.
        const is_dma_register = addr == 0xFF46;
        if (count_cycle and self.dma.active and dmaBlocksCpuAccess(self.dma.source, addr) and !is_dma_register) {
            return 0xFF;
        }

        if (count_cycle and addr >= 0x8000 and addr <= 0x9FFF and self.isPpuVramReadBlocked()) {
            return 0xFF;
        }

        if (count_cycle and isOamAddress(addr) and self.isPpuOamReadBlocked()) {
            if (self.isPpuInMode2()) {
                // Accessing OAM while mode 2 is active triggers DMG corruption.
                const row = self.io.getOamScanRow();
                if (row < 19) {
                    // The PPU row counter names the M-cycle that just
                    // completed; the OAM latch exposed to the CPU is the next
                    // eight-byte row.
                    @constCast(self).applyOamReadCorruption(nextOamScanRow(row));
                }
            }
            return 0xFF;
        }

        return switch (addr) {
            // ROM Bank 0 + Switchable ROM Bank
            0x0000...0x7FFF => self.cartridge.mbc.readRom(addr),

            // Video RAM
            0x8000...0x9FFF => self.vram[addr - 0x8000],

            // External RAM (cartridge)
            0xA000...0xBFFF => self.cartridge.mbc.readRam(addr),

            // Work RAM
            0xC000...0xDFFF => self.wram[addr - 0xC000],

            // Echo RAM (mirror of C000-DDFF)
            0xE000...0xFDFF => self.wram[addr - 0xE000],

            // OAM
            0xFE00...0xFE9F => self.oam[addr - 0xFE00],

            // Unusable
            0xFEA0...0xFEFF => 0xFF,

            // I/O Registers
            0xFF00...0xFF7F => blk: {
                const io_addr: u8 = @truncate(addr - 0xFF00);
                break :blk if (Apu.isRegister(io_addr))
                    self.apu.read(io_addr)
                else
                    self.io.read(io_addr);
            },

            // High RAM
            0xFF80...0xFFFE => self.hram[addr - 0xFF80],

            // Interrupt Enable Register
            0xFFFF => self.ie_register,
        };
    }

    pub fn write(self: *Bus, addr: u16, val: u8) void {
        self.writeInternal(addr, val, true);
    }

    fn writeInternal(self: *Bus, addr: u16, val: u8, count_cycle: bool) void {
        const delay_tick_for_lcdc = count_cycle and addr == 0xFF40;
        if (count_cycle and !delay_tick_for_lcdc) self.tickAccess();

        // FF46 remains writable so an HRAM routine can restart an active DMA.
        const is_dma_restart = addr == 0xFF46;
        if (count_cycle and self.dma.active and dmaBlocksCpuAccess(self.dma.source, addr) and !is_dma_restart) {
            return;
        }

        if (count_cycle and addr >= 0x8000 and addr <= 0x9FFF and self.isPpuVramWriteBlocked()) {
            return;
        }

        if (isOamAddress(addr) and self.isPpuOamWriteBlocked()) {
            if (self.isPpuInMode2()) {
                // Accessing OAM while mode 2 is active triggers DMG corruption.
                const row = self.io.getOamScanRow();
                if (row < 19) self.applyOamWriteCorruption(nextOamScanRow(row));
            }
            return;
        }

        switch (addr) {
            // ROM Bank 0 + Switchable ROM Bank (MBC registers)
            0x0000...0x7FFF => self.cartridge.mbc.writeRom(addr, val),

            // Video RAM
            0x8000...0x9FFF => self.vram[addr - 0x8000] = val,

            // External RAM (cartridge)
            0xA000...0xBFFF => self.cartridge.mbc.writeRam(addr, val),

            // Work RAM
            0xC000...0xDFFF => self.wram[addr - 0xC000] = val,

            // Echo RAM (mirror of C000-DDFF)
            0xE000...0xFDFF => self.wram[addr - 0xE000] = val,

            // OAM
            0xFE00...0xFE9F => self.oam[addr - 0xFE00] = val,

            // Unusable
            0xFEA0...0xFEFF => {},

            // I/O Registers
            0xFF00...0xFF7F => {
                const io_addr: u8 = @truncate(addr - 0xFF00);
                if (Apu.isRegister(io_addr)) {
                    self.apu.write(io_addr, val);
                } else if (io_addr == @intFromEnum(IoReg.SC)) {
                    self.serial.writeControl(&self.io, val);
                } else if (Timer.isRegister(io_addr)) {
                    if (io_addr == @intFromEnum(IoReg.DIV)) {
                        self.apu.dividerReset(self.timer.system_counter);
                    }
                    self.timer.writeRegister(&self.io, io_addr, val);
                } else {
                    self.io.write(io_addr, val);
                }

                if (io_addr == @intFromEnum(IoReg.DMA)) {
                    self.dma.start(val);
                }
            },

            // High RAM
            0xFF80...0xFFFE => self.hram[addr - 0xFF80] = val,

            // Interrupt Enable Register
            0xFFFF => self.ie_register = val,
        }

        if (delay_tick_for_lcdc) self.tickAccess();
    }

    inline fn isHramAddress(addr: u16) bool {
        return addr >= 0xFF80 and addr <= 0xFFFE;
    }

    fn dmaBlocksCpuAccess(source: u16, addr: u16) bool {
        if (isHramAddress(addr)) return false;
        if (source >= 0x8000 and source <= 0x9FFF) {
            return (addr >= 0x8000 and addr <= 0x9FFF) or isOamAddress(addr);
        }
        return true;
    }

    /// Read 16-bit value (little endian)
    pub fn read16(self: *const Bus, addr: u16) u16 {
        const lo = self.read(addr);
        const hi = self.read(addr +% 1);
        return (@as(u16, hi) << 8) | lo;
    }

    /// Write 16-bit value (little endian)
    pub fn write16(self: *Bus, addr: u16, val: u16) void {
        self.write(addr, @truncate(val));
        self.write(addr +% 1, @truncate(val >> 8));
    }
};

test "OAM DMA starts after one accessible M-cycle and then locks the CPU bus" {
    var bus = Bus.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
    );
    defer bus.deinit();

    for (0..0xA0) |index| bus.wram[index] = @intCast(index);
    bus.oam[0] = 0xEE;
    bus.hram[0] = 0x12;
    bus.write(0xFF46, 0xC0);

    try std.testing.expect(!bus.dma.active);
    try std.testing.expectEqual(@as(u8, 0), bus.read(0xC000));
    bus.tickDma(4);
    try std.testing.expect(!bus.dma.active);
    try std.testing.expectEqual(@as(u8, 0), bus.read(0xC000));

    bus.tickDma(4);
    try std.testing.expect(bus.dma.active);
    try std.testing.expectEqual(@as(u8, 0xEE), bus.oam[0]);

    bus.tickDma(4);
    try std.testing.expectEqual(@as(u8, 0), bus.oam[0]);
    try std.testing.expectEqual(@as(u8, 0xFF), bus.read(0xC000));
    try std.testing.expectEqual(@as(u8, 0x12), bus.read(0xFF80));
    bus.write(0xC000, 0xEE);
    bus.write(0xFF80, 0x34);
    try std.testing.expectEqual(@as(u8, 0), bus.wram[0]);
    try std.testing.expectEqual(@as(u8, 0x34), bus.hram[0]);

    bus.tickDma(255);
    bus.tickDma(255);
    bus.tickDma(126);
    try std.testing.expect(!bus.dma.active);
    for (0..0xA0) |index| {
        try std.testing.expectEqual(@as(u8, @intCast(index)), bus.oam[index]);
    }
}

test "OAM DMA restart remains writable and delays the new source" {
    var bus = Bus.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
    );
    defer bus.deinit();

    bus.wram[0] = 0x11;
    bus.wram[1] = 0x12;
    bus.wram[0x1000] = 0xA1;
    bus.write(0xFF46, 0xC0);
    bus.tickDma(8);
    bus.tickDma(4);
    try std.testing.expectEqual(@as(u8, 0x11), bus.oam[0]);

    bus.write(0xFF46, 0xD0);
    try std.testing.expectEqual(@as(u8, 8), bus.dma.start_delay);
    try std.testing.expectEqual(@as(u8, 0xD0), bus.read(0xFF46));
    bus.tickDma(4);
    try std.testing.expectEqual(@as(u8, 0x12), bus.oam[1]);
    try std.testing.expectEqual(@as(u16, 0xC000), bus.dma.source);

    bus.tickDma(4);
    try std.testing.expectEqual(@as(u16, 0xD000), bus.dma.source);
    try std.testing.expectEqual(@as(u8, 0x11), bus.oam[0]);
    bus.tickDma(4);
    try std.testing.expectEqual(@as(u8, 0xA1), bus.oam[0]);
}

test "DMG DMA source pages E0 through FF mirror C0 through DF" {
    var bus = Bus.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
    );
    defer bus.deinit();

    bus.wram[0] = 0xC0;
    bus.wram[0x1E00] = 0xDE;
    bus.wram[0x1F00] = 0xDF;

    for ([_]struct { page: u8, expected: u8 }{
        .{ .page = 0xE0, .expected = 0xC0 },
        .{ .page = 0xFE, .expected = 0xDE },
        .{ .page = 0xFF, .expected = 0xDF },
    }) |case| {
        bus.write(0xFF46, case.page);
        bus.tickDma(12);
        try std.testing.expectEqual(case.expected, bus.oam[0]);
        bus.dma = .{};
    }
}

test "VRAM-source DMA leaves cartridge and WRAM buses accessible" {
    var bus = Bus.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
    );
    defer bus.deinit();

    bus.wram[0] = 0x42;
    bus.vram[0] = 0x99;
    bus.oam[0] = 0x24;
    bus.write(0xFF46, 0x80);
    bus.tickDma(8);

    try std.testing.expectEqual(@as(u8, 0), bus.read(0x0000));
    try std.testing.expectEqual(@as(u8, 0x42), bus.read(0xC000));
    try std.testing.expectEqual(@as(u8, 0x42), bus.read(0xE000));
    try std.testing.expectEqual(@as(u8, 0xFF), bus.read(0x8000));
    try std.testing.expectEqual(@as(u8, 0xFF), bus.read(0xFE00));
}

test "CPU VRAM access is blocked during pixel transfer" {
    var bus = Bus.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
    );
    defer bus.deinit();

    bus.vram[0] = 0x12;
    bus.io.setPpuMode(3);
    bus.io.setPpuMemoryBlocked(false, true);
    try std.testing.expectEqual(@as(u8, 0xFF), bus.read(0x8000));
    bus.write(0x8000, 0x34);

    bus.io.setPpuMode(0);
    bus.io.setPpuMemoryBlocked(false, false);
    try std.testing.expectEqual(@as(u8, 0x12), bus.read(0x8000));
}

test "consecutive DMG OAM reads reproduce the POP row-latch pattern" {
    var bus = Bus.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
    );
    defer bus.deinit();

    for (&bus.oam, 0..) |*byte, index| byte.* = @intCast(index);

    // POP performs two reads on consecutive M-cycles. At this phase those
    // reads expose byte rows 0x30 and 0x38 to the CPU.
    bus.applyOamReadCorruption(6);
    bus.applyOamReadCorruption(7);

    // The first access combines row 6 with row 5 and copies that result
    // backward; the second advances the same row-5 latch into row 7.
    const latched_row = [_]u8{ 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F };
    for (4..8) |row| {
        try std.testing.expectEqualSlices(u8, &latched_row, bus.oam[row * 8 ..][0..8]);
    }
}

test "consecutive DMG OAM writes propagate the PUSH row-latch pattern" {
    var bus = Bus.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
    );
    defer bus.deinit();

    for (&bus.oam, 0..) |*byte, index| byte.* = @intCast(index);

    // PUSH exposes SP during its internal cycle, followed by both stack-write
    // addresses. Each M-cycle advances the same latched row one step.
    bus.applyOamWriteCorruption(6);
    bus.applyOamWriteCorruption(7);
    bus.applyOamWriteCorruption(8);

    const latched_row = [_]u8{ 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F };
    for (6..9) |row| {
        try std.testing.expectEqualSlices(u8, &latched_row, bus.oam[row * 8 ..][0..8]);
    }
}
