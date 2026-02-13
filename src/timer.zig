const std = @import("std");
const IoRegisters = @import("memory/io.zig").IoRegisters;
const IoReg = @import("memory/io.zig").IoReg;
const Interrupt = @import("memory/io.zig").Interrupt;

/// Timer and Divider - handles DIV, TIMA, TMA, TAC registers
pub const Timer = struct {
    /// Internal 16-bit counter (DIV is upper 8 bits)
    div_counter: u16,

    /// TIMA overflow counter (for tracking when to increment TIMA)
    tima_counter: u16,

    /// Previous AND result for falling edge detection
    prev_and_result: bool,

    pub fn init() Timer {
        return Timer{
            .div_counter = 0,
            .tima_counter = 0,
            .prev_and_result = false,
        };
    }

    pub fn reset(self: *Timer) void {
        self.div_counter = 0;
        self.tima_counter = 0;
        self.prev_and_result = false;
    }

    /// Tick the timer by the given number of T-cycles (usually 4 per M-cycle)
    pub fn tick(self: *Timer, cycles: u8, io: *IoRegisters) void {
        const tac = io.data[@intFromEnum(IoReg.TAC)];
        const timer_enabled = (tac & 0x04) != 0;

        var i: u8 = 0;
        while (i < cycles) : (i += 1) {
            // Increment DIV counter every T-cycle
            const old_div = self.div_counter;
            self.div_counter +%= 1;

            // Update IO DIV register (upper 8 bits of counter)
            io.div_counter = self.div_counter;

            if (timer_enabled) {
                // Get the bit position for the selected frequency
                const bit_pos = getTimerBitPos(tac);
                const old_bit = (old_div >> bit_pos) & 1;
                const new_bit = (self.div_counter >> bit_pos) & 1;

                // Falling edge detection: bit went from 1 to 0
                if (old_bit == 1 and new_bit == 0) {
                    self.incrementTima(io);
                }
            }
        }
    }

    /// Increment TIMA and handle overflow
    fn incrementTima(self: *Timer, io: *IoRegisters) void {
        _ = self;
        const tima = io.data[@intFromEnum(IoReg.TIMA)];
        if (tima == 0xFF) {
            // Overflow: reset to TMA and request interrupt
            io.data[@intFromEnum(IoReg.TIMA)] = io.data[@intFromEnum(IoReg.TMA)];
            io.requestInterrupt(Interrupt.TIMER);
        } else {
            io.data[@intFromEnum(IoReg.TIMA)] = tima + 1;
        }
    }

    /// Get the bit position in DIV counter for timer frequency
    fn getTimerBitPos(tac: u8) u4 {
        return switch (tac & 0x03) {
            0b00 => 9, // 4096 Hz (DIV bit 9)
            0b01 => 3, // 262144 Hz (DIV bit 3)
            0b10 => 5, // 65536 Hz (DIV bit 5)
            0b11 => 7, // 16384 Hz (DIV bit 7)
            else => unreachable,
        };
    }
};
