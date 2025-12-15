const std = @import("std");
const Rom = @import("rom.zig").Rom;

pub const Bus = struct {
    rom: *Rom,

    // Internal memory regions
    wram: [0x2000]u8, // Work RAM (0xC000-0xDFFF)
    hram: [0x7F]u8, // High RAM (0xFF80-0xFFFE)
    oam: [0xA0]u8, // OAM (0xFE00-0xFE9F)
    io: [0x80]u8, // I/O registers (0xFF00-0xFF7F)
    vram: [0x2000]u8, // Video RAM (0x8000-0x9FFF)
    ie_register: u8, // Interrupt Enable (0xFFFF)

    pub fn init(rom: *Rom) Bus {
        return Bus{
            .rom = rom,
            .wram = [_]u8{0} ** 0x2000,
            .hram = [_]u8{0} ** 0x7F,
            .oam = [_]u8{0} ** 0xA0,
            .io = [_]u8{0} ** 0x80,
            .vram = [_]u8{0} ** 0x2000,
            .ie_register = 0,
        };
    }

    pub fn read(self: *Bus, addr: u16) u8 {
        return switch (addr) {
            // ROM Bank 0 + Switchable ROM Bank
            0x0000...0x7FFF => self.rom.readRom(addr),

            // Video RAM
            0x8000...0x9FFF => self.vram[addr - 0x8000],

            // External RAM (cartridge)
            0xA000...0xBFFF => self.rom.readRam(addr),

            // Work RAM
            0xC000...0xDFFF => self.wram[addr - 0xC000],

            // Echo RAM (mirror of C000-DDFF)
            0xE000...0xFDFF => self.wram[addr - 0xE000],

            // OAM
            0xFE00...0xFE9F => self.oam[addr - 0xFE00],

            // Unusable
            0xFEA0...0xFEFF => 0xFF,

            // I/O Registers
            0xFF00...0xFF7F => self.io[addr - 0xFF00],

            // High RAM
            0xFF80...0xFFFE => self.hram[addr - 0xFF80],

            // Interrupt Enable Register
            0xFFFF => self.ie_register,
        };
    }

    pub fn write(self: *Bus, addr: u16, val: u8) void {
        switch (addr) {
            // ROM Bank 0 + Switchable ROM Bank (MBC registers)
            0x0000...0x7FFF => self.rom.writeRom(addr, val),

            // Video RAM
            0x8000...0x9FFF => self.vram[addr - 0x8000] = val,

            // External RAM (cartridge)
            0xA000...0xBFFF => self.rom.writeRam(addr, val),

            // Work RAM
            0xC000...0xDFFF => self.wram[addr - 0xC000] = val,

            // Echo RAM (mirror of C000-DDFF)
            0xE000...0xFDFF => self.wram[addr - 0xE000] = val,

            // OAM
            0xFE00...0xFE9F => self.oam[addr - 0xFE00] = val,

            // Unusable
            0xFEA0...0xFEFF => {},

            // I/O Registers
            0xFF00...0xFF7F => self.io[addr - 0xFF00] = val,

            // High RAM
            0xFF80...0xFFFE => self.hram[addr - 0xFF80] = val,

            // Interrupt Enable Register
            0xFFFF => self.ie_register = val,
        }
    }
};
