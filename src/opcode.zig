/// 8-bit register identifiers for the LR35902
pub const Reg8 = enum(u3) {
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    HL_INDIRECT = 6, // (HL) - memory at address HL
    A = 7,
};

/// 16-bit register identifiers
pub const Reg16 = enum(u2) {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
};

/// Operand for ALU operations - can be register, memory indirect, or immediate
pub const AluOperand = union(enum) {
    reg: Reg8,
    immediate: u8,
};

/// Decoded instruction for the LR35902
pub const Instruction = union(enum) {
    nop,
    // ADD A, operand (without carry)
    add: AluOperand,
    // ADC A, operand (with carry)
    adc: AluOperand,
    // Placeholder for unimplemented opcodes
    illegal: u8,

    const Self = @This();

    /// Byte reader interface for fetching instruction bytes
    pub const ByteReader = struct {
        context: *anyopaque,
        readFn: *const fn (*anyopaque) u8,

        pub fn read(self: ByteReader) u8 {
            return self.readFn(self.context);
        }
    };

    /// Decode a single opcode byte into an Instruction
    pub fn decode(reader: ByteReader) Self {
        const byte = reader.read();

        return switch (byte) {
            // NOP: 0x00
            0x00 => .nop,

            // ADD A, r8: 0x80-0x87
            // Pattern: 10 000 rrr (where rrr is the source register)
            0x80...0x87 => .{ .add = .{ .reg = @enumFromInt(byte & 0x07) } },

            // ADC A, r8: 0x88-0x8F
            // Pattern: 10 001 rrr
            0x88...0x8F => .{ .adc = .{ .reg = @enumFromInt(byte & 0x07) } },

            // ADD A, n8: 0xC6
            0xC6 => .{ .add = .{ .immediate = reader.read() } },

            // ADC A, n8: 0xCE
            0xCE => .{ .adc = .{ .immediate = reader.read() } },

            else => .{ .illegal = byte },
        };
    }
};
