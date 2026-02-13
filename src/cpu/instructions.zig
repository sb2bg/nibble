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
pub const Reg16 = enum(u3) {
    BC = 0,
    DE = 1,
    HL = 2,
    SP = 3,
    AF = 4, // Only for PUSH/POP
};

/// Operand for ALU operations - can be register or immediate
pub const AluOperand = union(enum) {
    reg: Reg8,
    immediate: u8,
};

/// Condition codes for conditional jumps/calls/returns
pub const Condition = enum(u3) {
    NZ = 0, // Not Zero
    Z = 1, // Zero
    NC = 2, // Not Carry
    C = 3, // Carry
    Always = 4, // Unconditional (not a real condition code)
};

/// Decoded instruction for the LR35902
pub const Instruction = union(enum) {
    // Misc/Control
    nop,
    halt,
    stop,
    di,
    ei,

    // 8-bit Loads
    ld_r_r: struct { dst: Reg8, src: Reg8 }, // LD r, r'
    ld_r_n: struct { dst: Reg8, value: u8 }, // LD r, n
    ld_a_bc, // LD A, (BC)
    ld_a_de, // LD A, (DE)
    ld_bc_a, // LD (BC), A
    ld_de_a, // LD (DE), A
    ld_a_mem: u16, // LD A, (nn)
    ld_mem_a: u16, // LD (nn), A
    ldh_a_n: u8, // LDH A, (n) - LD A, (0xFF00+n)
    ldh_n_a: u8, // LDH (n), A - LD (0xFF00+n), A
    ldh_a_c, // LD A, (0xFF00+C)
    ldh_c_a, // LD (0xFF00+C), A
    ld_a_hli, // LD A, (HL+)
    ld_a_hld, // LD A, (HL-)
    ld_hli_a, // LD (HL+), A
    ld_hld_a, // LD (HL-), A

    // 16-bit Loads
    ld_rr_nn: struct { dst: Reg16, value: u16 }, // LD rr, nn
    ld_sp_hl, // LD SP, HL
    ld_nn_sp: u16, // LD (nn), SP
    push_rr: Reg16, // PUSH rr
    pop_rr: Reg16, // POP rr

    // 8-bit ALU
    add: AluOperand, // ADD A, operand
    adc: AluOperand, // ADC A, operand
    sub: AluOperand, // SUB A, operand
    sbc: AluOperand, // SBC A, operand
    and_: AluOperand, // AND A, operand
    or_: AluOperand, // OR A, operand
    xor: AluOperand, // XOR A, operand
    cp: AluOperand, // CP A, operand
    inc_r: Reg8, // INC r
    dec_r: Reg8, // DEC r
    daa, // DAA
    cpl, // CPL
    scf, // SCF
    ccf, // CCF

    // 16-bit Arithmetic
    add_hl_rr: Reg16, // ADD HL, rr
    add_sp_e: u8, // ADD SP, e (signed)
    ld_hl_sp_e: u8, // LD HL, SP+e (signed)
    inc_rr: Reg16, // INC rr
    dec_rr: Reg16, // DEC rr

    // Rotates (non-CB)
    rlca, // RLCA
    rrca, // RRCA
    rla, // RLA
    rra, // RRA

    // CB-prefixed rotates/shifts
    rlc: Reg8, // RLC r
    rrc: Reg8, // RRC r
    rl: Reg8, // RL r
    rr: Reg8, // RR r
    sla: Reg8, // SLA r
    sra: Reg8, // SRA r
    swap: Reg8, // SWAP r
    srl: Reg8, // SRL r

    // Bit operations
    bit: struct { bit: u3, reg: Reg8 }, // BIT b, r
    res: struct { bit: u3, reg: Reg8 }, // RES b, r
    set: struct { bit: u3, reg: Reg8 }, // SET b, r

    // Jumps
    jp: u16, // JP nn
    jp_cc: struct { cond: Condition, addr: u16 }, // JP cc, nn
    jp_hl, // JP HL
    jr: u8, // JR e (signed offset, stored as u8)
    jr_cc: struct { cond: Condition, offset: u8 }, // JR cc, e

    // Calls
    call: u16, // CALL nn
    call_cc: struct { cond: Condition, addr: u16 }, // CALL cc, nn

    // Returns
    ret, // RET
    ret_cc: Condition, // RET cc
    reti, // RETI

    // RST (restart)
    rst: u3, // RST vec (0-7, multiplied by 8)

    // Illegal opcode
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

    /// Decode a single instruction
    pub fn decode(reader: ByteReader) Self {
        const byte = reader.read();

        return switch (byte) {
            // =====================================================================
            // Misc/Control
            // =====================================================================
            0x00 => .nop,
            0x10 => blk: {
                _ = reader.read(); // STOP has a second byte
                break :blk .stop;
            },
            0x76 => .halt,
            0xF3 => .di,
            0xFB => .ei,

            // =====================================================================
            // 8-bit Loads: LD r, r'
            // =====================================================================
            // 0x40-0x7F except 0x76 (HALT)
            0x40...0x75, 0x77...0x7F => blk: {
                const dst: Reg8 = @enumFromInt((byte >> 3) & 0x07);
                const src: Reg8 = @enumFromInt(byte & 0x07);
                break :blk .{ .ld_r_r = .{ .dst = dst, .src = src } };
            },

            // LD r, n (immediate)
            0x06 => .{ .ld_r_n = .{ .dst = .B, .value = reader.read() } },
            0x0E => .{ .ld_r_n = .{ .dst = .C, .value = reader.read() } },
            0x16 => .{ .ld_r_n = .{ .dst = .D, .value = reader.read() } },
            0x1E => .{ .ld_r_n = .{ .dst = .E, .value = reader.read() } },
            0x26 => .{ .ld_r_n = .{ .dst = .H, .value = reader.read() } },
            0x2E => .{ .ld_r_n = .{ .dst = .L, .value = reader.read() } },
            0x36 => .{ .ld_r_n = .{ .dst = .HL_INDIRECT, .value = reader.read() } },
            0x3E => .{ .ld_r_n = .{ .dst = .A, .value = reader.read() } },

            // LD A, (rr)
            0x0A => .ld_a_bc,
            0x1A => .ld_a_de,

            // LD (rr), A
            0x02 => .ld_bc_a,
            0x12 => .ld_de_a,

            // LD A, (nn) / LD (nn), A
            0xFA => .{ .ld_a_mem = readU16(reader) },
            0xEA => .{ .ld_mem_a = readU16(reader) },

            // LDH instructions
            0xF0 => .{ .ldh_a_n = reader.read() },
            0xE0 => .{ .ldh_n_a = reader.read() },
            0xF2 => .ldh_a_c,
            0xE2 => .ldh_c_a,

            // LD A, (HL+/-) and LD (HL+/-), A
            0x2A => .ld_a_hli,
            0x3A => .ld_a_hld,
            0x22 => .ld_hli_a,
            0x32 => .ld_hld_a,

            // =====================================================================
            // 16-bit Loads
            // =====================================================================
            0x01 => .{ .ld_rr_nn = .{ .dst = .BC, .value = readU16(reader) } },
            0x11 => .{ .ld_rr_nn = .{ .dst = .DE, .value = readU16(reader) } },
            0x21 => .{ .ld_rr_nn = .{ .dst = .HL, .value = readU16(reader) } },
            0x31 => .{ .ld_rr_nn = .{ .dst = .SP, .value = readU16(reader) } },
            0xF9 => .ld_sp_hl,
            0x08 => .{ .ld_nn_sp = readU16(reader) },
            0xF8 => .{ .ld_hl_sp_e = reader.read() },

            // PUSH/POP
            0xC5 => .{ .push_rr = .BC },
            0xD5 => .{ .push_rr = .DE },
            0xE5 => .{ .push_rr = .HL },
            0xF5 => .{ .push_rr = .AF },
            0xC1 => .{ .pop_rr = .BC },
            0xD1 => .{ .pop_rr = .DE },
            0xE1 => .{ .pop_rr = .HL },
            0xF1 => .{ .pop_rr = .AF },

            // =====================================================================
            // 8-bit ALU
            // =====================================================================
            // ADD A, r
            0x80...0x87 => .{ .add = .{ .reg = @enumFromInt(byte & 0x07) } },
            0xC6 => .{ .add = .{ .immediate = reader.read() } },

            // ADC A, r
            0x88...0x8F => .{ .adc = .{ .reg = @enumFromInt(byte & 0x07) } },
            0xCE => .{ .adc = .{ .immediate = reader.read() } },

            // SUB A, r
            0x90...0x97 => .{ .sub = .{ .reg = @enumFromInt(byte & 0x07) } },
            0xD6 => .{ .sub = .{ .immediate = reader.read() } },

            // SBC A, r
            0x98...0x9F => .{ .sbc = .{ .reg = @enumFromInt(byte & 0x07) } },
            0xDE => .{ .sbc = .{ .immediate = reader.read() } },

            // AND A, r
            0xA0...0xA7 => .{ .and_ = .{ .reg = @enumFromInt(byte & 0x07) } },
            0xE6 => .{ .and_ = .{ .immediate = reader.read() } },

            // XOR A, r
            0xA8...0xAF => .{ .xor = .{ .reg = @enumFromInt(byte & 0x07) } },
            0xEE => .{ .xor = .{ .immediate = reader.read() } },

            // OR A, r
            0xB0...0xB7 => .{ .or_ = .{ .reg = @enumFromInt(byte & 0x07) } },
            0xF6 => .{ .or_ = .{ .immediate = reader.read() } },

            // CP A, r
            0xB8...0xBF => .{ .cp = .{ .reg = @enumFromInt(byte & 0x07) } },
            0xFE => .{ .cp = .{ .immediate = reader.read() } },

            // INC r
            0x04 => .{ .inc_r = .B },
            0x0C => .{ .inc_r = .C },
            0x14 => .{ .inc_r = .D },
            0x1C => .{ .inc_r = .E },
            0x24 => .{ .inc_r = .H },
            0x2C => .{ .inc_r = .L },
            0x34 => .{ .inc_r = .HL_INDIRECT },
            0x3C => .{ .inc_r = .A },

            // DEC r
            0x05 => .{ .dec_r = .B },
            0x0D => .{ .dec_r = .C },
            0x15 => .{ .dec_r = .D },
            0x1D => .{ .dec_r = .E },
            0x25 => .{ .dec_r = .H },
            0x2D => .{ .dec_r = .L },
            0x35 => .{ .dec_r = .HL_INDIRECT },
            0x3D => .{ .dec_r = .A },

            // Misc ALU
            0x27 => .daa,
            0x2F => .cpl,
            0x37 => .scf,
            0x3F => .ccf,

            // =====================================================================
            // 16-bit Arithmetic
            // =====================================================================
            // ADD HL, rr
            0x09 => .{ .add_hl_rr = .BC },
            0x19 => .{ .add_hl_rr = .DE },
            0x29 => .{ .add_hl_rr = .HL },
            0x39 => .{ .add_hl_rr = .SP },

            // ADD SP, e
            0xE8 => .{ .add_sp_e = reader.read() },

            // INC rr
            0x03 => .{ .inc_rr = .BC },
            0x13 => .{ .inc_rr = .DE },
            0x23 => .{ .inc_rr = .HL },
            0x33 => .{ .inc_rr = .SP },

            // DEC rr
            0x0B => .{ .dec_rr = .BC },
            0x1B => .{ .dec_rr = .DE },
            0x2B => .{ .dec_rr = .HL },
            0x3B => .{ .dec_rr = .SP },

            // =====================================================================
            // Rotates (non-CB)
            // =====================================================================
            0x07 => .rlca,
            0x0F => .rrca,
            0x17 => .rla,
            0x1F => .rra,

            // =====================================================================
            // Jumps
            // =====================================================================
            0xC3 => .{ .jp = readU16(reader) },
            0xE9 => .jp_hl,
            0xC2 => .{ .jp_cc = .{ .cond = .NZ, .addr = readU16(reader) } },
            0xCA => .{ .jp_cc = .{ .cond = .Z, .addr = readU16(reader) } },
            0xD2 => .{ .jp_cc = .{ .cond = .NC, .addr = readU16(reader) } },
            0xDA => .{ .jp_cc = .{ .cond = .C, .addr = readU16(reader) } },

            0x18 => .{ .jr = reader.read() },
            0x20 => .{ .jr_cc = .{ .cond = .NZ, .offset = reader.read() } },
            0x28 => .{ .jr_cc = .{ .cond = .Z, .offset = reader.read() } },
            0x30 => .{ .jr_cc = .{ .cond = .NC, .offset = reader.read() } },
            0x38 => .{ .jr_cc = .{ .cond = .C, .offset = reader.read() } },

            // =====================================================================
            // Calls
            // =====================================================================
            0xCD => .{ .call = readU16(reader) },
            0xC4 => .{ .call_cc = .{ .cond = .NZ, .addr = readU16(reader) } },
            0xCC => .{ .call_cc = .{ .cond = .Z, .addr = readU16(reader) } },
            0xD4 => .{ .call_cc = .{ .cond = .NC, .addr = readU16(reader) } },
            0xDC => .{ .call_cc = .{ .cond = .C, .addr = readU16(reader) } },

            // =====================================================================
            // Returns
            // =====================================================================
            0xC9 => .ret,
            0xC0 => .{ .ret_cc = .NZ },
            0xC8 => .{ .ret_cc = .Z },
            0xD0 => .{ .ret_cc = .NC },
            0xD8 => .{ .ret_cc = .C },
            0xD9 => .reti,

            // =====================================================================
            // RST (restart vectors)
            // =====================================================================
            0xC7 => .{ .rst = 0 },
            0xCF => .{ .rst = 1 },
            0xD7 => .{ .rst = 2 },
            0xDF => .{ .rst = 3 },
            0xE7 => .{ .rst = 4 },
            0xEF => .{ .rst = 5 },
            0xF7 => .{ .rst = 6 },
            0xFF => .{ .rst = 7 },

            // =====================================================================
            // CB-prefixed instructions
            // =====================================================================
            0xCB => decodeCB(reader),

            else => .{ .illegal = byte },
        };
    }

    fn decodeCB(reader: ByteReader) Self {
        const cb_byte = reader.read();
        const reg: Reg8 = @enumFromInt(cb_byte & 0x07);
        const bit: u3 = @truncate((cb_byte >> 3) & 0x07);
        const op: u2 = @truncate(cb_byte >> 6);

        return switch (op) {
            0b00 => switch (bit) {
                0 => .{ .rlc = reg },
                1 => .{ .rrc = reg },
                2 => .{ .rl = reg },
                3 => .{ .rr = reg },
                4 => .{ .sla = reg },
                5 => .{ .sra = reg },
                6 => .{ .swap = reg },
                7 => .{ .srl = reg },
            },
            0b01 => .{ .bit = .{ .bit = bit, .reg = reg } },
            0b10 => .{ .res = .{ .bit = bit, .reg = reg } },
            0b11 => .{ .set = .{ .bit = bit, .reg = reg } },
        };
    }

    fn readU16(reader: ByteReader) u16 {
        const lo = reader.read();
        const hi = reader.read();
        return (@as(u16, hi) << 8) | lo;
    }
};
