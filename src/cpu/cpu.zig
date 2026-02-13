const std = @import("std");
const Bus = @import("../memory/bus.zig").Bus;
const instructions = @import("instructions.zig");
const Instruction = instructions.Instruction;
const Reg8 = instructions.Reg8;
const Reg16 = instructions.Reg16;
const AluOperand = instructions.AluOperand;
const Condition = instructions.Condition;
const IoReg = @import("../memory/io.zig").IoReg;
const Interrupt = @import("../memory/io.zig").Interrupt;

// CPU Flags Register (F)
pub const Flags = packed struct(u8) {
    _unused: u4 = 0, // Bits 0-3 (always zero)
    c: u1, // Bit 4: Carry
    h: u1, // Bit 5: Half Carry
    n: u1, // Bit 6: Subtract
    z: u1, // Bit 7: Zero

    pub fn toU8(self: Flags) u8 {
        return @bitCast(self);
    }

    pub fn fromU8(val: u8) Flags {
        return @bitCast(val & 0xF0); // Lower 4 bits always 0
    }
};

pub const Cpu = struct {
    // Registers
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,

    // Interrupt state
    ime: bool, // Interrupt Master Enable
    ime_enable_delay: u2, // EI enables IME after the next instruction
    halted: bool, // CPU is halted (waiting for interrupt)
    halt_bug: bool, // HALT bug: next opcode fetch does not increment PC

    // Cycle tracking
    cycles: u64 = 0, // Total cycles executed

    // Internal context for byte reader (used during decode)
    reader_ctx: ReaderContext = undefined,

    const ReaderContext = struct {
        cpu: *Cpu,
        bus: *const Bus,
    };

    /// Initialize CPU to post-boot ROM state (DMG)
    pub fn init() Cpu {
        return Cpu{
            .af = 0x01B0, // A=0x01, F=0xB0
            .bc = 0x0013, // B=0x00, C=0x13
            .de = 0x00D8, // D=0x00, E=0xD8
            .hl = 0x014D, // H=0x01, L=0x4D
            .sp = 0xFFFE,
            .pc = 0x0100,
            .ime = false,
            .ime_enable_delay = 0,
            .halted = false,
            .halt_bug = false,
        };
    }

    pub fn reset(self: *Cpu) void {
        self.* = Cpu.init();
    }

    // =========================================================================
    // Register Access
    // =========================================================================

    pub inline fn a(self: *const Cpu) u8 {
        return @truncate(self.af >> 8);
    }

    pub inline fn setA(self: *Cpu, val: u8) void {
        self.af = (self.af & 0x00FF) | (@as(u16, val) << 8);
    }

    pub inline fn f(self: *const Cpu) Flags {
        return @bitCast(@as(u8, @truncate(self.af)));
    }

    pub inline fn setF(self: *Cpu, flags: Flags) void {
        const val: u8 = @bitCast(flags);
        self.af = (self.af & 0xFF00) | val;
    }

    pub inline fn b(self: *const Cpu) u8 {
        return @truncate(self.bc >> 8);
    }

    pub inline fn setB(self: *Cpu, val: u8) void {
        self.bc = (self.bc & 0x00FF) | (@as(u16, val) << 8);
    }

    pub inline fn c(self: *const Cpu) u8 {
        return @truncate(self.bc);
    }

    pub inline fn setC(self: *Cpu, val: u8) void {
        self.bc = (self.bc & 0xFF00) | @as(u16, val);
    }

    pub inline fn d(self: *const Cpu) u8 {
        return @truncate(self.de >> 8);
    }

    pub inline fn setD(self: *Cpu, val: u8) void {
        self.de = (self.de & 0x00FF) | (@as(u16, val) << 8);
    }

    pub inline fn e(self: *const Cpu) u8 {
        return @truncate(self.de);
    }

    pub inline fn setE(self: *Cpu, val: u8) void {
        self.de = (self.de & 0xFF00) | @as(u16, val);
    }

    pub inline fn h(self: *const Cpu) u8 {
        return @truncate(self.hl >> 8);
    }

    pub inline fn setH(self: *Cpu, val: u8) void {
        self.hl = (self.hl & 0x00FF) | (@as(u16, val) << 8);
    }

    pub inline fn l(self: *const Cpu) u8 {
        return @truncate(self.hl);
    }

    pub inline fn setL(self: *Cpu, val: u8) void {
        self.hl = (self.hl & 0xFF00) | @as(u16, val);
    }

    // =========================================================================
    // Memory Access
    // =========================================================================

    pub fn fetch(self: *Cpu, bus: *const Bus) u8 {
        const val = bus.read(self.pc);
        if (self.halt_bug) {
            self.halt_bug = false;
        } else {
            self.pc +%= 1;
        }
        return val;
    }

    pub fn fetch16(self: *Cpu, bus: *const Bus) u16 {
        const lo = self.fetch(bus);
        const hi = self.fetch(bus);
        return (@as(u16, hi) << 8) | lo;
    }

    /// Read an 8-bit register value (or memory at HL for HL_INDIRECT)
    pub fn readReg8(self: *const Cpu, reg: Reg8, bus: *const Bus) u8 {
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

    /// Read a 16-bit register pair
    pub fn readReg16(self: *const Cpu, reg: Reg16) u16 {
        return switch (reg) {
            .BC => self.bc,
            .DE => self.de,
            .HL => self.hl,
            .SP => self.sp,
            .AF => self.af,
        };
    }

    /// Write a 16-bit register pair
    pub fn writeReg16(self: *Cpu, reg: Reg16, val: u16) void {
        switch (reg) {
            .BC => self.bc = val,
            .DE => self.de = val,
            .HL => self.hl = val,
            .SP => self.sp = val,
            .AF => self.af = val & 0xFFF0, // Lower 4 bits of F always 0
        }
    }

    // =========================================================================
    // Stack Operations
    // =========================================================================

    pub fn push(self: *Cpu, val: u16, bus: *Bus) void {
        self.sp -%= 1;
        bus.write(self.sp, @truncate(val >> 8));
        self.sp -%= 1;
        bus.write(self.sp, @truncate(val));
    }

    pub fn pop(self: *Cpu, bus: *const Bus) u16 {
        const lo = bus.read(self.sp);
        self.sp +%= 1;
        const hi = bus.read(self.sp);
        self.sp +%= 1;
        return (@as(u16, hi) << 8) | lo;
    }

    // =========================================================================
    // Condition Checking
    // =========================================================================

    pub fn checkCondition(self: *const Cpu, cond: Condition) bool {
        const flags = self.f();
        return switch (cond) {
            .NZ => flags.z == 0,
            .Z => flags.z == 1,
            .NC => flags.c == 0,
            .C => flags.c == 1,
            .Always => true,
        };
    }

    // =========================================================================
    // Decode
    // =========================================================================

    fn byteReader(self: *Cpu, bus: *const Bus) Instruction.ByteReader {
        const Context = struct {
            cpu: *Cpu,
            bus: *const Bus,

            fn read(ptr: *anyopaque) u8 {
                const ctx: *@This() = @ptrCast(@alignCast(ptr));
                return ctx.cpu.fetch(ctx.bus);
            }
        };

        self.reader_ctx = .{ .cpu = self, .bus = bus };
        return .{
            .context = @ptrCast(&self.reader_ctx),
            .readFn = Context.read,
        };
    }

    pub fn decode(self: *Cpu, bus: *const Bus) Instruction {
        return Instruction.decode(self.byteReader(bus));
    }

    // =========================================================================
    // Interrupts
    // =========================================================================

    /// Handle pending interrupts. Returns cycles used if interrupt was handled.
    fn handleInterrupts(self: *Cpu, bus: *Bus) ?u8 {
        const ie = bus.ie_register;
        const if_reg = bus.io.data[@intFromEnum(IoReg.IF)];
        const pending = ie & if_reg & 0x1F;

        if (pending == 0) return null;

        // Wake from HALT even if IME is disabled
        self.halted = false;

        if (!self.ime) return null;

        // Handle highest priority interrupt
        const interrupt_vectors = [_]struct { bit: u8, addr: u16 }{
            .{ .bit = Interrupt.VBLANK, .addr = 0x0040 },
            .{ .bit = Interrupt.LCD_STAT, .addr = 0x0048 },
            .{ .bit = Interrupt.TIMER, .addr = 0x0050 },
            .{ .bit = Interrupt.SERIAL, .addr = 0x0058 },
            .{ .bit = Interrupt.JOYPAD, .addr = 0x0060 },
        };

        for (interrupt_vectors) |iv| {
            if (pending & iv.bit != 0) {
                self.ime = false;
                bus.io.clearInterrupt(iv.bit);
                self.push(self.pc, bus);
                self.pc = iv.addr;
                return 20; // Interrupt dispatch takes 20 cycles
            }
        }

        return null;
    }

    fn pendingInterrupts(bus: *const Bus) u8 {
        const ie = bus.ie_register;
        const if_reg = bus.io.data[@intFromEnum(IoReg.IF)];
        return ie & if_reg & 0x1F;
    }

    fn tickImeEnableDelay(self: *Cpu) void {
        if (self.ime_enable_delay == 0) return;
        self.ime_enable_delay -= 1;
        if (self.ime_enable_delay == 0) {
            self.ime = true;
        }
    }

    // =========================================================================
    // Execute
    // =========================================================================

    /// Execute a decoded instruction. Returns cycles used.
    pub fn execute(self: *Cpu, inst: Instruction, bus: *Bus) u8 {
        return switch (inst) {
            .nop => 4,
            .halt => blk: {
                if (!self.ime and pendingInterrupts(bus) != 0) {
                    // HALT bug: CPU continues, but the next opcode fetch won't
                    // increment PC.
                    self.halted = false;
                    self.halt_bug = true;
                } else {
                    self.halted = true;
                }
                break :blk 4;
            },
            .stop => 4, // TODO: implement properly
            .di => blk: {
                self.ime = false;
                self.ime_enable_delay = 0;
                break :blk 4;
            },
            .ei => blk: {
                self.ime = true;
                self.ime_enable_delay = 0;
                break :blk 4;
            },

            // 8-bit loads
            .ld_r_r => |args| blk: {
                const val = self.readReg8(args.src, bus);
                self.writeReg8(args.dst, val, bus);
                // (HL) access adds 4 cycles
                const extra: u8 = if (args.src == .HL_INDIRECT or args.dst == .HL_INDIRECT) 4 else 0;
                break :blk 4 + extra;
            },
            .ld_r_n => |args| blk: {
                self.writeReg8(args.dst, args.value, bus);
                break :blk if (args.dst == .HL_INDIRECT) 12 else 8;
            },
            .ld_a_bc => blk: {
                self.setA(bus.read(self.bc));
                break :blk 8;
            },
            .ld_a_de => blk: {
                self.setA(bus.read(self.de));
                break :blk 8;
            },
            .ld_bc_a => blk: {
                bus.write(self.bc, self.a());
                break :blk 8;
            },
            .ld_de_a => blk: {
                bus.write(self.de, self.a());
                break :blk 8;
            },
            .ld_a_mem => |addr| blk: {
                self.setA(bus.read(addr));
                break :blk 16;
            },
            .ld_mem_a => |addr| blk: {
                bus.write(addr, self.a());
                break :blk 16;
            },
            .ldh_a_n => |offset| blk: {
                self.setA(bus.read(0xFF00 | @as(u16, offset)));
                break :blk 12;
            },
            .ldh_n_a => |offset| blk: {
                bus.write(0xFF00 | @as(u16, offset), self.a());
                break :blk 12;
            },
            .ldh_a_c => blk: {
                self.setA(bus.read(0xFF00 | @as(u16, self.c())));
                break :blk 8;
            },
            .ldh_c_a => blk: {
                bus.write(0xFF00 | @as(u16, self.c()), self.a());
                break :blk 8;
            },
            .ld_a_hli => blk: {
                self.setA(bus.read(self.hl));
                self.hl +%= 1;
                break :blk 8;
            },
            .ld_a_hld => blk: {
                self.setA(bus.read(self.hl));
                self.hl -%= 1;
                break :blk 8;
            },
            .ld_hli_a => blk: {
                bus.write(self.hl, self.a());
                self.hl +%= 1;
                break :blk 8;
            },
            .ld_hld_a => blk: {
                bus.write(self.hl, self.a());
                self.hl -%= 1;
                break :blk 8;
            },

            // 16-bit loads
            .ld_rr_nn => |args| blk: {
                self.writeReg16(args.dst, args.value);
                break :blk 12;
            },
            .ld_sp_hl => blk: {
                self.sp = self.hl;
                break :blk 8;
            },
            .ld_nn_sp => |addr| blk: {
                bus.write(addr, @truncate(self.sp));
                bus.write(addr +% 1, @truncate(self.sp >> 8));
                break :blk 20;
            },
            .push_rr => |reg| blk: {
                self.push(self.readReg16(reg), bus);
                break :blk 16;
            },
            .pop_rr => |reg| blk: {
                const val = self.pop(bus);
                self.writeReg16(reg, val);
                break :blk 12;
            },

            // ALU 8-bit
            .add => |operand| self.execAdd(operand, false, bus),
            .adc => |operand| self.execAdd(operand, true, bus),
            .sub => |operand| self.execSub(operand, false, bus),
            .sbc => |operand| self.execSub(operand, true, bus),
            .and_ => |operand| self.execAnd(operand, bus),
            .or_ => |operand| self.execOr(operand, bus),
            .xor => |operand| self.execXor(operand, bus),
            .cp => |operand| self.execCp(operand, bus),
            .inc_r => |reg| blk: {
                const val = self.readReg8(reg, bus);
                const result = val +% 1;
                self.writeReg8(reg, result, bus);
                const flags = self.f();
                self.setF(.{
                    .z = if (result == 0) 1 else 0,
                    .n = 0,
                    .h = if ((val & 0x0F) == 0x0F) 1 else 0,
                    .c = flags.c,
                });
                break :blk if (reg == .HL_INDIRECT) 12 else 4;
            },
            .dec_r => |reg| blk: {
                const val = self.readReg8(reg, bus);
                const result = val -% 1;
                self.writeReg8(reg, result, bus);
                const flags = self.f();
                self.setF(.{
                    .z = if (result == 0) 1 else 0,
                    .n = 1,
                    .h = if ((val & 0x0F) == 0) 1 else 0,
                    .c = flags.c,
                });
                break :blk if (reg == .HL_INDIRECT) 12 else 4;
            },
            .daa => self.execDaa(),
            .cpl => blk: {
                self.setA(~self.a());
                const flags = self.f();
                self.setF(.{
                    .z = flags.z,
                    .n = 1,
                    .h = 1,
                    .c = flags.c,
                });
                break :blk 4;
            },
            .scf => blk: {
                const flags = self.f();
                self.setF(.{
                    .z = flags.z,
                    .n = 0,
                    .h = 0,
                    .c = 1,
                });
                break :blk 4;
            },
            .ccf => blk: {
                const flags = self.f();
                self.setF(.{
                    .z = flags.z,
                    .n = 0,
                    .h = 0,
                    .c = ~flags.c,
                });
                break :blk 4;
            },

            // 16-bit arithmetic
            .add_hl_rr => |reg| blk: {
                const hl = self.hl;
                const val = self.readReg16(reg);
                const result = @as(u32, hl) + @as(u32, val);
                self.hl = @truncate(result);
                const flags = self.f();
                self.setF(.{
                    .z = flags.z,
                    .n = 0,
                    .h = if ((hl & 0x0FFF) + (val & 0x0FFF) > 0x0FFF) 1 else 0,
                    .c = if (result > 0xFFFF) 1 else 0,
                });
                break :blk 8;
            },
            .add_sp_e => |offset| blk: {
                const sp = self.sp;
                const e_val: u16 = @bitCast(@as(i16, @as(i8, @bitCast(offset))));
                const result = sp +% e_val;
                self.sp = result;
                // Flags are set based on unsigned addition of lower byte
                const lo_result = (sp & 0xFF) +% (e_val & 0xFF);
                self.setF(.{
                    .z = 0,
                    .n = 0,
                    .h = if ((sp & 0x0F) + (e_val & 0x0F) > 0x0F) 1 else 0,
                    .c = if (lo_result > 0xFF) 1 else 0,
                });
                break :blk 16;
            },
            .ld_hl_sp_e => |offset| blk: {
                const sp = self.sp;
                const e_val: u16 = @bitCast(@as(i16, @as(i8, @bitCast(offset))));
                const result = sp +% e_val;
                self.hl = result;
                const lo_result = (sp & 0xFF) +% (e_val & 0xFF);
                self.setF(.{
                    .z = 0,
                    .n = 0,
                    .h = if ((sp & 0x0F) + (e_val & 0x0F) > 0x0F) 1 else 0,
                    .c = if (lo_result > 0xFF) 1 else 0,
                });
                break :blk 12;
            },
            .inc_rr => |reg| blk: {
                self.writeReg16(reg, self.readReg16(reg) +% 1);
                break :blk 8;
            },
            .dec_rr => |reg| blk: {
                self.writeReg16(reg, self.readReg16(reg) -% 1);
                break :blk 8;
            },

            // Rotates and shifts
            .rlca => blk: {
                const val = self.a();
                const carry = val >> 7;
                self.setA((val << 1) | carry);
                self.setF(.{ .z = 0, .n = 0, .h = 0, .c = @truncate(carry) });
                break :blk 4;
            },
            .rrca => blk: {
                const val = self.a();
                const carry = val & 1;
                self.setA((val >> 1) | (carry << 7));
                self.setF(.{ .z = 0, .n = 0, .h = 0, .c = @truncate(carry) });
                break :blk 4;
            },
            .rla => blk: {
                const val = self.a();
                const old_carry = self.f().c;
                const new_carry = val >> 7;
                self.setA((val << 1) | old_carry);
                self.setF(.{ .z = 0, .n = 0, .h = 0, .c = @truncate(new_carry) });
                break :blk 4;
            },
            .rra => blk: {
                const val = self.a();
                const old_carry = self.f().c;
                const new_carry = val & 1;
                self.setA((val >> 1) | (@as(u8, old_carry) << 7));
                self.setF(.{ .z = 0, .n = 0, .h = 0, .c = @truncate(new_carry) });
                break :blk 4;
            },

            // CB-prefixed instructions
            .rlc => |reg| self.execRlc(reg, bus),
            .rrc => |reg| self.execRrc(reg, bus),
            .rl => |reg| self.execRl(reg, bus),
            .rr => |reg| self.execRr(reg, bus),
            .sla => |reg| self.execSla(reg, bus),
            .sra => |reg| self.execSra(reg, bus),
            .swap => |reg| self.execSwap(reg, bus),
            .srl => |reg| self.execSrl(reg, bus),
            .bit => |args| blk: {
                const val = self.readReg8(args.reg, bus);
                const bit = (val >> args.bit) & 1;
                const flags = self.f();
                self.setF(.{
                    .z = if (bit == 0) 1 else 0,
                    .n = 0,
                    .h = 1,
                    .c = flags.c,
                });
                break :blk if (args.reg == .HL_INDIRECT) 12 else 8;
            },
            .res => |args| blk: {
                const val = self.readReg8(args.reg, bus);
                self.writeReg8(args.reg, val & ~(@as(u8, 1) << args.bit), bus);
                break :blk if (args.reg == .HL_INDIRECT) 16 else 8;
            },
            .set => |args| blk: {
                const val = self.readReg8(args.reg, bus);
                self.writeReg8(args.reg, val | (@as(u8, 1) << args.bit), bus);
                break :blk if (args.reg == .HL_INDIRECT) 16 else 8;
            },

            // Jumps
            .jp => |addr| blk: {
                self.pc = addr;
                break :blk 16;
            },
            .jp_cc => |args| blk: {
                if (self.checkCondition(args.cond)) {
                    self.pc = args.addr;
                    break :blk 16;
                }
                break :blk 12;
            },
            .jp_hl => blk: {
                self.pc = self.hl;
                break :blk 4;
            },
            .jr => |offset| blk: {
                self.pc = @bitCast(@as(i16, @bitCast(self.pc)) +% @as(i16, @as(i8, @bitCast(offset))));
                break :blk 12;
            },
            .jr_cc => |args| blk: {
                if (self.checkCondition(args.cond)) {
                    self.pc = @bitCast(@as(i16, @bitCast(self.pc)) +% @as(i16, @as(i8, @bitCast(args.offset))));
                    break :blk 12;
                }
                break :blk 8;
            },

            // Calls and returns
            .call => |addr| blk: {
                self.push(self.pc, bus);
                self.pc = addr;
                break :blk 24;
            },
            .call_cc => |args| blk: {
                if (self.checkCondition(args.cond)) {
                    self.push(self.pc, bus);
                    self.pc = args.addr;
                    break :blk 24;
                }
                break :blk 12;
            },
            .ret => blk: {
                self.pc = self.pop(bus);
                break :blk 16;
            },
            .ret_cc => |cond| blk: {
                if (self.checkCondition(cond)) {
                    self.pc = self.pop(bus);
                    break :blk 20;
                }
                break :blk 8;
            },
            .reti => blk: {
                self.pc = self.pop(bus);
                self.ime = true;
                self.ime_enable_delay = 0;
                break :blk 16;
            },
            .rst => |vec| blk: {
                self.push(self.pc, bus);
                self.pc = @as(u16, vec) * 8;
                break :blk 16;
            },

            .illegal => |byte| blk: {
                std.debug.print("Illegal opcode: 0x{X:0>2} at PC=0x{X:0>4}\n", .{ byte, self.pc -% 1 });
                break :blk 4;
            },
        };
    }

    // =========================================================================
    // ALU Operations
    // =========================================================================

    fn execAdd(self: *Cpu, operand: AluOperand, with_carry: bool, bus: *const Bus) u8 {
        const a_val = self.a();
        const operand_val: u8 = switch (operand) {
            .reg => |r| self.readReg8(r, bus),
            .immediate => |imm| imm,
        };
        const carry_in: u8 = if (with_carry) self.f().c else 0;

        const result = @as(u16, a_val) + @as(u16, operand_val) + carry_in;
        const half = (a_val & 0x0F) + (operand_val & 0x0F) + carry_in;

        self.setA(@truncate(result));
        self.setF(.{
            .z = if (@as(u8, @truncate(result)) == 0) 1 else 0,
            .n = 0,
            .h = if (half > 0x0F) 1 else 0,
            .c = if (result > 0xFF) 1 else 0,
        });

        return switch (operand) {
            .reg => |r| if (r == .HL_INDIRECT) 8 else 4,
            .immediate => 8,
        };
    }

    fn execSub(self: *Cpu, operand: AluOperand, with_carry: bool, bus: *const Bus) u8 {
        const a_val = self.a();
        const operand_val: u8 = switch (operand) {
            .reg => |r| self.readReg8(r, bus),
            .immediate => |imm| imm,
        };
        const carry_in: u8 = if (with_carry) self.f().c else 0;

        const result = @as(i16, a_val) - @as(i16, operand_val) - carry_in;
        const half = @as(i8, @intCast(a_val & 0x0F)) - @as(i8, @intCast(operand_val & 0x0F)) - @as(i8, @intCast(carry_in));

        self.setA(@bitCast(@as(i8, @truncate(result))));
        self.setF(.{
            .z = if (@as(u8, @bitCast(@as(i8, @truncate(result)))) == 0) 1 else 0,
            .n = 1,
            .h = if (half < 0) 1 else 0,
            .c = if (result < 0) 1 else 0,
        });

        return switch (operand) {
            .reg => |r| if (r == .HL_INDIRECT) 8 else 4,
            .immediate => 8,
        };
    }

    fn execAnd(self: *Cpu, operand: AluOperand, bus: *const Bus) u8 {
        const a_val = self.a();
        const operand_val: u8 = switch (operand) {
            .reg => |r| self.readReg8(r, bus),
            .immediate => |imm| imm,
        };

        const result = a_val & operand_val;
        self.setA(result);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 1, .c = 0 });

        return switch (operand) {
            .reg => |r| if (r == .HL_INDIRECT) 8 else 4,
            .immediate => 8,
        };
    }

    fn execOr(self: *Cpu, operand: AluOperand, bus: *const Bus) u8 {
        const a_val = self.a();
        const operand_val: u8 = switch (operand) {
            .reg => |r| self.readReg8(r, bus),
            .immediate => |imm| imm,
        };

        const result = a_val | operand_val;
        self.setA(result);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = 0 });

        return switch (operand) {
            .reg => |r| if (r == .HL_INDIRECT) 8 else 4,
            .immediate => 8,
        };
    }

    fn execXor(self: *Cpu, operand: AluOperand, bus: *const Bus) u8 {
        const a_val = self.a();
        const operand_val: u8 = switch (operand) {
            .reg => |r| self.readReg8(r, bus),
            .immediate => |imm| imm,
        };

        const result = a_val ^ operand_val;
        self.setA(result);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = 0 });

        return switch (operand) {
            .reg => |r| if (r == .HL_INDIRECT) 8 else 4,
            .immediate => 8,
        };
    }

    fn execCp(self: *Cpu, operand: AluOperand, bus: *const Bus) u8 {
        const a_val = self.a();
        const operand_val: u8 = switch (operand) {
            .reg => |r| self.readReg8(r, bus),
            .immediate => |imm| imm,
        };

        const result = @as(i16, a_val) - @as(i16, operand_val);
        const half = @as(i8, @intCast(a_val & 0x0F)) - @as(i8, @intCast(operand_val & 0x0F));

        self.setF(.{
            .z = if (a_val == operand_val) 1 else 0,
            .n = 1,
            .h = if (half < 0) 1 else 0,
            .c = if (result < 0) 1 else 0,
        });

        return switch (operand) {
            .reg => |r| if (r == .HL_INDIRECT) 8 else 4,
            .immediate => 8,
        };
    }

    fn execDaa(self: *Cpu) u8 {
        var a_val: u8 = self.a();
        var flags = self.f();
        var correction: u8 = 0;

        if (flags.h == 1 or (flags.n == 0 and (a_val & 0x0F) > 9)) {
            correction |= 0x06;
        }

        if (flags.c == 1 or (flags.n == 0 and a_val > 0x99)) {
            correction |= 0x60;
            flags.c = 1;
        }

        if (flags.n == 1) {
            a_val -%= correction;
        } else {
            a_val +%= correction;
        }

        self.setA(a_val);
        self.setF(.{
            .z = if (a_val == 0) 1 else 0,
            .n = flags.n,
            .h = 0,
            .c = flags.c,
        });

        return 4;
    }

    // =========================================================================
    // CB-prefixed operations
    // =========================================================================

    fn execRlc(self: *Cpu, reg: Reg8, bus: *Bus) u8 {
        const val = self.readReg8(reg, bus);
        const carry = val >> 7;
        const result = (val << 1) | carry;
        self.writeReg8(reg, result, bus);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = @truncate(carry) });
        return if (reg == .HL_INDIRECT) 16 else 8;
    }

    fn execRrc(self: *Cpu, reg: Reg8, bus: *Bus) u8 {
        const val = self.readReg8(reg, bus);
        const carry = val & 1;
        const result = (val >> 1) | (carry << 7);
        self.writeReg8(reg, result, bus);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = @truncate(carry) });
        return if (reg == .HL_INDIRECT) 16 else 8;
    }

    fn execRl(self: *Cpu, reg: Reg8, bus: *Bus) u8 {
        const val = self.readReg8(reg, bus);
        const old_carry = self.f().c;
        const new_carry = val >> 7;
        const result = (val << 1) | old_carry;
        self.writeReg8(reg, result, bus);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = @truncate(new_carry) });
        return if (reg == .HL_INDIRECT) 16 else 8;
    }

    fn execRr(self: *Cpu, reg: Reg8, bus: *Bus) u8 {
        const val = self.readReg8(reg, bus);
        const old_carry = self.f().c;
        const new_carry = val & 1;
        const result = (val >> 1) | (@as(u8, old_carry) << 7);
        self.writeReg8(reg, result, bus);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = @truncate(new_carry) });
        return if (reg == .HL_INDIRECT) 16 else 8;
    }

    fn execSla(self: *Cpu, reg: Reg8, bus: *Bus) u8 {
        const val = self.readReg8(reg, bus);
        const carry = val >> 7;
        const result = val << 1;
        self.writeReg8(reg, result, bus);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = @truncate(carry) });
        return if (reg == .HL_INDIRECT) 16 else 8;
    }

    fn execSra(self: *Cpu, reg: Reg8, bus: *Bus) u8 {
        const val = self.readReg8(reg, bus);
        const carry = val & 1;
        const result = (val >> 1) | (val & 0x80); // Preserve bit 7
        self.writeReg8(reg, result, bus);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = @truncate(carry) });
        return if (reg == .HL_INDIRECT) 16 else 8;
    }

    fn execSwap(self: *Cpu, reg: Reg8, bus: *Bus) u8 {
        const val = self.readReg8(reg, bus);
        const result = (val >> 4) | (val << 4);
        self.writeReg8(reg, result, bus);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = 0 });
        return if (reg == .HL_INDIRECT) 16 else 8;
    }

    fn execSrl(self: *Cpu, reg: Reg8, bus: *Bus) u8 {
        const val = self.readReg8(reg, bus);
        const carry = val & 1;
        const result = val >> 1;
        self.writeReg8(reg, result, bus);
        self.setF(.{ .z = if (result == 0) 1 else 0, .n = 0, .h = 0, .c = @truncate(carry) });
        return if (reg == .HL_INDIRECT) 16 else 8;
    }

    // =========================================================================
    // Main Step Function
    // =========================================================================

    /// Execute one CPU step: handle interrupts, then fetch-decode-execute
    /// Returns the number of cycles used
    pub fn step(self: *Cpu, bus: *Bus) u8 {
        // Check for interrupts
        if (self.handleInterrupts(bus)) |int_cycles| {
            self.cycles += int_cycles;
            return int_cycles;
        }

        // If halted, just wait
        if (self.halted) {
            self.cycles += 4;
            return 4;
        }

        // Normal instruction execution
        const inst = self.decode(bus);
        const cycles = self.execute(inst, bus);
        self.tickImeEnableDelay();
        self.cycles += cycles;
        return cycles;
    }
};
