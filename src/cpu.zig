const std = @import("std");
const Bus = @import("bus.zig").Bus;
const opcode = @import("opcode.zig");
const Instruction = opcode.Instruction;
const Reg8 = opcode.Reg8;
const AluOperand = opcode.AluOperand;

// CPU Flags Register (F)
const Flags = packed struct(u8) {
    _unused: u4 = 0, // Bits 0-3 (always zero)
    c: u1, // Bit 4: Carry
    h: u1, // Bit 5: Half Carry
    n: u1, // Bit 6: Subtract
    z: u1, // Bit 7: Zero

    pub fn toU8(self: Flags) u8 {
        return @bitCast(self);
    }
};

pub const Cpu = struct {
    // Accumulator & Flags
    af: u16,
    // General Purpose
    bc: u16,
    de: u16,
    hl: u16,
    // Stack Pointer
    sp: u16,
    // Program Counter
    pc: u16,
    // Internal context for byte reader (used during decode)
    reader_ctx: ReaderContext = undefined,

    const ReaderContext = struct {
        cpu: *Cpu,
        bus: *Bus,
    };

    pub fn init() Cpu {
        return Cpu{
            .af = 0x01B0, // A=0x01, F=0xB0
            .bc = 0x0013, // B=0x00, C=0x13
            .de = 0x00D8, // D=0x00, E=0xD8
            .hl = 0x014D, // H=0x01, L=0x4D
            .sp = 0xFFFE, // Initial Stack Pointer (top of stack)
            .pc = 0x0100, // Initial Program Counter (after boot ROM)
        };
    }

    // Accumulator register
    // A is the High byte of AF
    pub inline fn a(self: *const Cpu) u8 {
        return @truncate(self.af >> 8);
    }

    pub inline fn setA(self: *Cpu, val: u8) void {
        self.af = (self.af & 0x00FF) | (@as(u16, val) << 8);
    }

    // Flags register
    // F is the Low byte of AF
    pub inline fn f(self: *const Cpu) Flags {
        return @bitCast(@as(u8, @truncate(self.af)));
    }

    pub inline fn setF(self: *Cpu, flags: Flags) void {
        const val: u8 = @bitCast(flags);
        self.af = (self.af & 0xFF00) | val;
    }

    // General Purpose Register B
    // B is the High byte of BC
    pub inline fn b(self: *const Cpu) u8 {
        return @truncate(self.bc >> 8);
    }

    pub inline fn setB(self: *Cpu, val: u8) void {
        self.bc = (self.bc & 0x00FF) | (@as(u16, val) << 8);
    }

    // General Purpose Register C
    // C is the Low byte of BC
    pub inline fn c(self: *const Cpu) u8 {
        return @truncate(self.bc);
    }

    pub inline fn setC(self: *Cpu, val: u8) void {
        self.bc = (self.bc & 0xFF00) | @as(u16, val);
    }

    // General Purpose Register D
    // D is the High byte of DE
    pub inline fn d(self: *const Cpu) u8 {
        return @truncate(self.de >> 8);
    }

    pub inline fn setD(self: *Cpu, val: u8) void {
        self.de = (self.de & 0x00FF) | (@as(u16, val) << 8);
    }

    // General Purpose Register E
    // E is the Low byte of DE
    pub inline fn e(self: *const Cpu) u8 {
        return @truncate(self.de);
    }

    pub inline fn setE(self: *Cpu, val: u8) void {
        self.de = (self.de & 0xFF00) | @as(u16, val);
    }

    // General Purpose Register H
    // H is the High byte of HL
    pub inline fn h(self: *const Cpu) u8 {
        return @truncate(self.hl >> 8);
    }

    pub inline fn setH(self: *Cpu, val: u8) void {
        self.hl = (self.hl & 0x00FF) | (@as(u16, val) << 8);
    }

    // General Purpose Register L
    // L is the Low byte of HL
    pub inline fn l(self: *const Cpu) u8 {
        return @truncate(self.hl);
    }

    pub inline fn setL(self: *Cpu, val: u8) void {
        self.hl = (self.hl & 0xFF00) | @as(u16, val);
    }

    pub fn fetch(self: *Cpu, bus: *Bus) u8 {
        const val = bus.read(self.pc);
        self.pc +%= 1;
        return val;
    }

    pub fn fetch16(self: *Cpu, bus: *Bus) u16 {
        const lo = self.fetch(bus);
        const hi = self.fetch(bus);
        return (@as(u16, hi) << 8) | lo;
    }

    // =========================================================================
    // Register Access by Enum
    // =========================================================================

    /// Read an 8-bit register value (or memory at HL for HL_INDIRECT)
    pub fn readReg8(self: *const Cpu, reg: Reg8, bus: *Bus) u8 {
        return switch (reg) {
            .A => self.a(),
            .B => self.b(),
            .C => self.c(),
            .D => self.d(),
            .E => self.e(),
            .H => self.h(),
            .L => self.l(),
            .HL_INDIRECT => bus.read(self.hl),
        };
    }

    /// Write an 8-bit register value (or memory at HL for HL_INDIRECT)
    pub fn writeReg8(self: *Cpu, reg: Reg8, val: u8, bus: *Bus) void {
        switch (reg) {
            .A => self.setA(val),
            .B => self.setB(val),
            .C => self.setC(val),
            .D => self.setD(val),
            .E => self.setE(val),
            .H => self.setH(val),
            .L => self.setL(val),
            .HL_INDIRECT => bus.write(self.hl, val),
        }
    }

    // =========================================================================
    // Decode
    // =========================================================================

    /// Create a ByteReader that fetches from this CPU/Bus
    fn byteReader(self: *Cpu, bus: *Bus) Instruction.ByteReader {
        const Context = struct {
            cpu: *Cpu,
            bus: *Bus,

            fn read(ptr: *anyopaque) u8 {
                const ctx: *@This() = @ptrCast(@alignCast(ptr));
                return ctx.cpu.fetch(ctx.bus);
            }
        };

        // Store context in CPU's scratch space for the duration of decode
        self.reader_ctx = .{ .cpu = self, .bus = bus };
        return .{
            .context = @ptrCast(&self.reader_ctx),
            .readFn = Context.read,
        };
    }

    /// Decode the next instruction from memory
    pub fn decode(self: *Cpu, bus: *Bus) Instruction {
        return Instruction.decode(self.byteReader(bus));
    }

    // =========================================================================
    // Execute
    // =========================================================================

    /// Execute a decoded instruction
    pub fn execute(self: *Cpu, inst: Instruction, bus: *Bus) void {
        switch (inst) {
            .nop => {},
            .add => |operand| self.execAdd(operand, false, bus),
            .adc => |operand| self.execAdd(operand, true, bus),
            .illegal => |byte| {
                std.debug.print("Illegal opcode: 0x{X:0>2}\n", .{byte});
            },
        }
    }

    // =========================================================================
    // ALU Operations
    // =========================================================================

    /// Execute ADD or ADC: A = A + operand [+ carry]
    fn execAdd(self: *Cpu, operand: AluOperand, with_carry: bool, bus: *Bus) void {
        const a_val = self.a();
        const operand_val: u8 = switch (operand) {
            .reg => |r| self.readReg8(r, bus),
            .immediate => |imm| imm,
        };
        const carry_in: u1 = if (with_carry) self.f().c else 0;

        const result = add8(a_val, operand_val, carry_in);

        self.setA(result.value);
        self.setF(.{
            .z = if (result.value == 0) 1 else 0,
            .n = 0, // ADD clears subtract flag
            .h = result.half_carry,
            .c = result.carry,
        });
    }

    // =========================================================================
    // Fetch-Decode-Execute Cycle
    // =========================================================================

    /// Execute one CPU cycle: fetch, decode, and execute a single instruction
    pub fn step(self: *Cpu, bus: *Bus) void {
        const inst = self.decode(bus);
        self.execute(inst, bus);
    }
};

// =============================================================================
// ALU Helper Functions
// =============================================================================

/// Result of an 8-bit arithmetic operation
const AluResult8 = struct {
    value: u8,
    carry: u1,
    half_carry: u1,
};

/// 8-bit addition with carry input, returns result and flags
fn add8(a: u8, b: u8, carry_in: u1) AluResult8 {
    // Full result for carry detection
    const full: u16 = @as(u16, a) + @as(u16, b) + carry_in;

    // Half-carry: carry from bit 3 to bit 4
    const half = (a & 0x0F) + (b & 0x0F) + carry_in;

    return .{
        .value = @truncate(full),
        .carry = if (full > 0xFF) 1 else 0,
        .half_carry = if (half > 0x0F) 1 else 0,
    };
}
