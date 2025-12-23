const std = @import("std");

/// Memory Bank Controller type
pub const MbcType = enum {
    none, // ROM only
    mbc1,
    mbc2,
    mbc3,
    mbc5,

    pub fn fromCartridgeType(cart_type: u8) MbcType {
        return switch (cart_type) {
            0x00, 0x08, 0x09 => .none,
            0x01, 0x02, 0x03 => .mbc1,
            0x05, 0x06 => .mbc2,
            0x0F, 0x10, 0x11, 0x12, 0x13 => .mbc3,
            0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E => .mbc5,
            else => .none,
        };
    }
};

/// Memory Bank Controller - handles ROM/RAM banking
pub const Mbc = struct {
    mbc_type: MbcType,

    // Banking state
    rom_bank: u16, // Current ROM bank (can be 9-bit for MBC5)
    ram_bank: u8, // Current RAM bank
    ram_enabled: bool,
    banking_mode: u1, // MBC1: 0 = ROM banking, 1 = RAM banking

    // ROM and RAM data (owned by Cartridge, referenced here)
    rom_data: []const u8,
    ram_data: ?[]u8,

    pub fn init(mbc_type: MbcType, rom_data: []const u8, ram_data: ?[]u8) Mbc {
        return Mbc{
            .mbc_type = mbc_type,
            .rom_bank = 1,
            .ram_bank = 0,
            .ram_enabled = false,
            .banking_mode = 0,
            .rom_data = rom_data,
            .ram_data = ram_data,
        };
    }

    /// Reset MBC state (keeps ROM/RAM data)
    pub fn reset(self: *Mbc) void {
        self.rom_bank = 1;
        self.ram_bank = 0;
        self.ram_enabled = false;
        self.banking_mode = 0;
    }

    /// Read from ROM address space (0x0000-0x7FFF)
    pub fn readRom(self: *const Mbc, addr: u16) u8 {
        return switch (addr) {
            // Bank 0 (always fixed at 0x0000-0x3FFF)
            0x0000...0x3FFF => blk: {
                if (addr < self.rom_data.len) {
                    break :blk self.rom_data[addr];
                }
                break :blk 0xFF;
            },

            // Switchable bank (0x4000-0x7FFF)
            0x4000...0x7FFF => blk: {
                const bank_offset: usize = @as(usize, self.rom_bank) * 0x4000;
                const offset = bank_offset + (addr - 0x4000);
                if (offset < self.rom_data.len) {
                    break :blk self.rom_data[offset];
                }
                break :blk 0xFF;
            },

            else => 0xFF,
        };
    }

    /// Write to ROM address space (MBC register writes)
    pub fn writeRom(self: *Mbc, addr: u16, val: u8) void {
        switch (self.mbc_type) {
            .none => {}, // ROM only - writes ignored
            .mbc1 => self.writeMbc1(addr, val),
            .mbc2 => self.writeMbc2(addr, val),
            .mbc3 => self.writeMbc3(addr, val),
            .mbc5 => self.writeMbc5(addr, val),
        }
    }

    /// Read from external RAM (0xA000-0xBFFF)
    pub fn readRam(self: *const Mbc, addr: u16) u8 {
        if (!self.ram_enabled) return 0xFF;
        if (self.ram_data) |ram| {
            const offset = (@as(usize, self.ram_bank) * 0x2000) + (addr - 0xA000);
            if (offset < ram.len) {
                return ram[offset];
            }
        }
        return 0xFF;
    }

    /// Write to external RAM (0xA000-0xBFFF)
    pub fn writeRam(self: *Mbc, addr: u16, val: u8) void {
        if (!self.ram_enabled) return;
        if (self.ram_data) |ram| {
            const offset = (@as(usize, self.ram_bank) * 0x2000) + (addr - 0xA000);
            if (offset < ram.len) {
                ram[offset] = val;
            }
        }
    }

    // =========================================================================
    // MBC1
    // =========================================================================

    fn writeMbc1(self: *Mbc, addr: u16, val: u8) void {
        switch (addr) {
            0x0000...0x1FFF => {
                // RAM enable
                self.ram_enabled = (val & 0x0F) == 0x0A;
            },
            0x2000...0x3FFF => {
                // ROM bank number (lower 5 bits)
                var bank: u8 = val & 0x1F;
                if (bank == 0) bank = 1; // Bank 0 maps to bank 1
                self.rom_bank = (self.rom_bank & 0x60) | bank;
            },
            0x4000...0x5FFF => {
                // RAM bank / upper ROM bank bits
                if (self.banking_mode == 0) {
                    // ROM banking mode - upper 2 bits of ROM bank
                    self.rom_bank = (self.rom_bank & 0x1F) | (@as(u16, val & 0x03) << 5);
                } else {
                    // RAM banking mode
                    self.ram_bank = val & 0x03;
                }
            },
            0x6000...0x7FFF => {
                // Banking mode select
                self.banking_mode = @intCast(val & 0x01);
            },
            else => {},
        }
    }

    // =========================================================================
    // MBC2
    // =========================================================================

    fn writeMbc2(self: *Mbc, addr: u16, val: u8) void {
        switch (addr) {
            0x0000...0x3FFF => {
                if ((addr & 0x0100) == 0) {
                    // RAM enable (bit 8 must be 0)
                    self.ram_enabled = (val & 0x0F) == 0x0A;
                } else {
                    // ROM bank (bit 8 must be 1)
                    var bank = val & 0x0F;
                    if (bank == 0) bank = 1;
                    self.rom_bank = bank;
                }
            },
            else => {},
        }
    }

    // =========================================================================
    // MBC3
    // =========================================================================

    fn writeMbc3(self: *Mbc, addr: u16, val: u8) void {
        switch (addr) {
            0x0000...0x1FFF => {
                self.ram_enabled = (val & 0x0F) == 0x0A;
            },
            0x2000...0x3FFF => {
                var bank: u8 = val & 0x7F;
                if (bank == 0) bank = 1;
                self.rom_bank = bank;
            },
            0x4000...0x5FFF => {
                // RAM bank or RTC register select
                self.ram_bank = val & 0x0F;
            },
            0x6000...0x7FFF => {
                // RTC latch - not implemented yet
            },
            else => {},
        }
    }

    // =========================================================================
    // MBC5
    // =========================================================================

    fn writeMbc5(self: *Mbc, addr: u16, val: u8) void {
        switch (addr) {
            0x0000...0x1FFF => {
                self.ram_enabled = (val & 0x0F) == 0x0A;
            },
            0x2000...0x2FFF => {
                // Lower 8 bits of ROM bank
                self.rom_bank = (self.rom_bank & 0x100) | val;
            },
            0x3000...0x3FFF => {
                // Upper 1 bit of ROM bank (9th bit)
                self.rom_bank = (@as(u16, val & 0x01) << 8) | (self.rom_bank & 0xFF);
            },
            0x4000...0x5FFF => {
                self.ram_bank = val & 0x0F;
            },
            else => {},
        }
    }
};
