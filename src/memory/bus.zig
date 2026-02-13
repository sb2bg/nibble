const std = @import("std");
const Mbc = @import("mbc.zig").Mbc;
const MbcType = @import("mbc.zig").MbcType;
const IoRegisters = @import("io.zig").IoRegisters;
const IoReg = @import("io.zig").IoReg;
const Cartridge = @import("../cartridge/cartridge.zig").Cartridge;

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
        self.cartridge.mbc.reset();
    }

    pub fn setCycleHook(self: *Bus, hook: ?CycleHook) void {
        self.cycle_hook = hook;
    }

    inline fn tickAccess(self: *const Bus) void {
        if (self.cycle_hook) |hook| {
            hook.tickFn(hook.context, 4);
        }
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

    pub fn read(self: *const Bus, addr: u16) u8 {
        return self.readInternal(addr, true);
    }

    fn readNoTick(self: *const Bus, addr: u16) u8 {
        return self.readInternal(addr, false);
    }

    fn readInternal(self: *const Bus, addr: u16, count_cycle: bool) u8 {
        if (count_cycle) self.tickAccess();

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
            0xFF00...0xFF7F => self.io.read(@truncate(addr - 0xFF00)),

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
        if (count_cycle) self.tickAccess();

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
                self.io.write(io_addr, val);

                // Execute OAM DMA immediately for functional correctness.
                if (io_addr == @intFromEnum(IoReg.DMA)) {
                    const source_base: u16 = @as(u16, val) << 8;
                    var i: usize = 0;
                    while (i < self.oam.len) : (i += 1) {
                        self.oam[i] = self.readNoTick(source_base + @as(u16, @intCast(i)));
                    }
                }
            },

            // High RAM
            0xFF80...0xFFFE => self.hram[addr - 0xFF80] = val,

            // Interrupt Enable Register
            0xFFFF => self.ie_register = val,
        }
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
