const std = @import("std");
const IoRegisters = @import("memory/io.zig").IoRegisters;
const IoReg = @import("memory/io.zig").IoReg;
const Interrupt = @import("memory/io.zig").Interrupt;

/// DMG system counter and programmable timer.
///
/// The timer owns the hidden 16-bit system counter. Keeping that counter out
/// of `IoRegisters` gives DIV writes, TAC writes, and normal ticking one source
/// of truth. The bus routes timer-register writes through `writeRegister`.
pub const Timer = struct {
    const DMG_POST_BOOT_COUNTER: u16 = 0xABCC;

    /// DIV exposes the upper eight bits of this counter.
    system_counter: u16 = DMG_POST_BOOT_COUNTER,

    /// An overflowing TIMA remains zero for four T-cycles before TMA is loaded
    /// and the timer interrupt is requested.
    reload_delay: u3 = 0,

    pub fn init() Timer {
        return .{};
    }

    pub fn reset(self: *Timer, io: *IoRegisters) void {
        self.* = .{};
        io.data[@intFromEnum(IoReg.DIV)] = @truncate(self.system_counter >> 8);
    }

    pub fn isRegister(addr: u8) bool {
        return addr >= @intFromEnum(IoReg.DIV) and addr <= @intFromEnum(IoReg.TAC);
    }

    /// Apply memory-mapped timer writes, including the falling-edge glitches
    /// caused by resetting DIV or changing TAC's input selection.
    pub fn writeRegister(self: *Timer, io: *IoRegisters, addr: u8, value: u8) void {
        const reg: IoReg = @enumFromInt(addr);
        switch (reg) {
            .DIV => {
                const old_signal = self.timerSignal(io.data[@intFromEnum(IoReg.TAC)]);
                self.system_counter = 0;
                io.data[@intFromEnum(IoReg.DIV)] = 0;
                if (old_signal) self.incrementTima(io);
            },
            .TIMA => {
                // A TIMA write during the overflow wait cancels the pending
                // reload and interrupt.
                self.reload_delay = 0;
                io.data[addr] = value;
            },
            .TMA => io.data[addr] = value,
            .TAC => {
                const old_signal = self.timerSignal(io.data[addr]);
                io.data[addr] = value | 0xF8;
                const new_signal = self.timerSignal(io.data[addr]);
                if (old_signal and !new_signal) self.incrementTima(io);
            },
            else => unreachable,
        }
    }

    /// Advance by T-cycles. The emulator may call this in small batches; all
    /// edge-sensitive behavior is still evaluated one T-cycle at a time.
    pub fn tick(self: *Timer, cycles: u8, io: *IoRegisters) void {
        var remaining = cycles;
        while (remaining > 0) : (remaining -= 1) {
            self.tickReload(io);

            const tac = io.data[@intFromEnum(IoReg.TAC)];
            const old_signal = self.timerSignal(tac);
            self.system_counter +%= 1;
            io.data[@intFromEnum(IoReg.DIV)] = @truncate(self.system_counter >> 8);
            const new_signal = self.timerSignal(tac);

            if (old_signal and !new_signal) self.incrementTima(io);
        }
    }

    fn tickReload(self: *Timer, io: *IoRegisters) void {
        if (self.reload_delay == 0) return;

        self.reload_delay -= 1;
        if (self.reload_delay == 0) {
            io.data[@intFromEnum(IoReg.TIMA)] = io.data[@intFromEnum(IoReg.TMA)];
            io.requestInterrupt(Interrupt.TIMER);
        }
    }

    fn incrementTima(self: *Timer, io: *IoRegisters) void {
        // Further input edges do not change TIMA while an overflow reload is
        // pending.
        if (self.reload_delay != 0) return;

        const index = @intFromEnum(IoReg.TIMA);
        if (io.data[index] == 0xFF) {
            io.data[index] = 0;
            self.reload_delay = 4;
        } else {
            io.data[index] +%= 1;
        }
    }

    fn timerSignal(self: *const Timer, tac: u8) bool {
        if ((tac & 0x04) == 0) return false;
        const bit = timerBit(tac);
        return ((self.system_counter >> bit) & 1) != 0;
    }

    fn timerBit(tac: u8) u4 {
        return switch (tac & 0x03) {
            0b00 => 9,
            0b01 => 3,
            0b10 => 5,
            0b11 => 7,
            else => unreachable,
        };
    }
};

test "DIV reset can clock TIMA on a falling edge" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();
    var timer = Timer.init();
    timer.system_counter = 0;
    io.data[@intFromEnum(IoReg.DIV)] = 0;

    timer.writeRegister(&io, @intFromEnum(IoReg.TAC), 0b101);
    timer.tick(8, &io); // Selected counter bit 3 is now high.
    timer.writeRegister(&io, @intFromEnum(IoReg.DIV), 0x99);

    try std.testing.expectEqual(@as(u16, 0), timer.system_counter);
    try std.testing.expectEqual(@as(u8, 1), io.data[@intFromEnum(IoReg.TIMA)]);
}

test "TIMA reload and interrupt occur four T-cycles after overflow" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();
    var timer = Timer.init();
    timer.system_counter = 0;
    io.data[@intFromEnum(IoReg.DIV)] = 0;

    timer.writeRegister(&io, @intFromEnum(IoReg.TAC), 0b101);
    timer.writeRegister(&io, @intFromEnum(IoReg.TMA), 0xA7);
    timer.writeRegister(&io, @intFromEnum(IoReg.TIMA), 0xFF);
    timer.tick(16, &io);

    try std.testing.expectEqual(@as(u8, 0), io.data[@intFromEnum(IoReg.TIMA)]);
    try std.testing.expectEqual(@as(u8, 4), timer.reload_delay);
    try std.testing.expectEqual(@as(u8, 0), io.data[@intFromEnum(IoReg.IF)] & Interrupt.TIMER);

    timer.tick(3, &io);
    try std.testing.expectEqual(@as(u8, 0), io.data[@intFromEnum(IoReg.TIMA)]);
    timer.tick(1, &io);
    try std.testing.expectEqual(@as(u8, 0xA7), io.data[@intFromEnum(IoReg.TIMA)]);
    try std.testing.expect((io.data[@intFromEnum(IoReg.IF)] & Interrupt.TIMER) != 0);
}

test "writing TIMA cancels a pending reload" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();
    var timer = Timer.init();
    timer.system_counter = 0;
    io.data[@intFromEnum(IoReg.DIV)] = 0;

    timer.writeRegister(&io, @intFromEnum(IoReg.TAC), 0b101);
    timer.writeRegister(&io, @intFromEnum(IoReg.TIMA), 0xFF);
    timer.tick(16, &io);
    timer.writeRegister(&io, @intFromEnum(IoReg.TIMA), 0x42);
    timer.tick(4, &io);

    try std.testing.expectEqual(@as(u8, 0x42), io.data[@intFromEnum(IoReg.TIMA)]);
    try std.testing.expectEqual(@as(u8, 0), io.data[@intFromEnum(IoReg.IF)] & Interrupt.TIMER);
}
