const std = @import("std");

// CPU Flags Register (F)
const Flags = packed struct(u8) {
    _unused: u4 = 0, // Bits 0-3 (always zero)
    c: u1, // Bit 4: Carry
    h: u1, // Bit 5: Half Carry
    n: u1, // Bit 6: Subtract
    z: u1, // Bit 7: Zero
};

// A Helper Union to access registers as 8-bit or 16-bit
const RegisterPair = packed union {
    word: u16,
    // Little-endian order
    bytes: packed struct {
        low: u8,
        high: u8,
    },
};

pub const Cpu = struct {
    a: u8, // Accumulator Register
    f: Flags, // Flags Register

    // General Purpose Registers
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,

    sp: u16, // Stack Pointer
    pc: u16, // Program Counter

    // Initialize CPU state (Power-on state is known)
    pub fn init() Cpu {
        return Cpu{
            .a = 0x01, // Accumulator starts at 0x01
            .f = @bitCast(@as(u8, 0xB0)), // 10110000: Z=1, N=0, H=1, C=0

            // Typical power-on values
            .b = 0x00,
            .c = 0x13,
            .d = 0x00,
            .e = 0xD8,
            .h = 0x01,
            .l = 0x4D,

            .sp = 0xFFFE, // Stack Pointer starts at top of memory
            .pc = 0x0100, // 0x100 is the entry point for cartridges
        };
    }
};
