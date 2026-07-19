const std = @import("std");
const io_mod = @import("memory/io.zig");
const IoRegisters = io_mod.IoRegisters;
const IoReg = io_mod.IoReg;
const Interrupt = io_mod.Interrupt;

const DOTS_PER_BIT = 512;

/// DMG serial transfer engine. A disconnected internal-clock transfer shifts
/// in high bits, so SB becomes $FF after eight 8,192 Hz clock pulses.
pub const Serial = struct {
    active: bool = false,
    outgoing: u8 = 0,
    bits_remaining: u4 = 0,
    dots_until_shift: u10 = 0,

    pub fn reset(self: *Serial) void {
        self.* = .{};
    }

    pub fn writeControl(self: *Serial, io: *IoRegisters, value: u8) void {
        const sc = value | 0x7E;
        io.data[@intFromEnum(IoReg.SC)] = sc;

        if ((value & 0x80) == 0) {
            self.reset();
            return;
        }

        if ((value & 0x01) == 0) {
            // An externally-clocked transfer remains pending until a link
            // partner supplies pulses; there is no synthetic timeout.
            self.reset();
            return;
        }

        self.active = true;
        self.outgoing = io.data[@intFromEnum(IoReg.SB)];
        self.bits_remaining = 8;
        self.dots_until_shift = DOTS_PER_BIT;
    }

    pub fn tick(self: *Serial, cycles: u8, io: *IoRegisters) void {
        for (0..cycles) |_| {
            if (!self.active) return;

            self.dots_until_shift -= 1;
            if (self.dots_until_shift != 0) continue;

            const sb_index = @intFromEnum(IoReg.SB);
            io.data[sb_index] = (io.data[sb_index] << 1) | 1;
            self.bits_remaining -= 1;

            if (self.bits_remaining == 0) {
                self.active = false;
                io.data[@intFromEnum(IoReg.SC)] &= ~@as(u8, 0x80);
                io.captureSerialOutput(self.outgoing);
                io.requestInterrupt(Interrupt.SERIAL);
            } else {
                self.dots_until_shift = DOTS_PER_BIT;
            }
        }
    }
};

test "internal clock transfer completes after 4096 dots" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();
    io.clearInterrupt(Interrupt.SERIAL);
    io.data[@intFromEnum(IoReg.SB)] = 0xA5;

    var serial: Serial = .{};
    serial.writeControl(&io, 0x81);
    var remaining: usize = 4095;
    while (remaining > 0) {
        const chunk: u8 = @intCast(@min(remaining, 255));
        serial.tick(chunk, &io);
        remaining -= chunk;
    }

    try std.testing.expect(serial.active);
    try std.testing.expect((io.read(@intFromEnum(IoReg.SC)) & 0x80) != 0);
    try std.testing.expectEqual(@as(usize, 0), io.getSerialOutput().len);

    serial.tick(1, &io);
    try std.testing.expect(!serial.active);
    try std.testing.expectEqual(@as(u8, 0xFF), io.read(@intFromEnum(IoReg.SB)));
    try std.testing.expectEqual(@as(u8, 0), io.read(@intFromEnum(IoReg.SC)) & 0x80);
    try std.testing.expectEqualSlices(u8, &.{0xA5}, io.getSerialOutput());
    try std.testing.expect((io.read(@intFromEnum(IoReg.IF)) & Interrupt.SERIAL) != 0);
}

test "external clock transfer waits indefinitely" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();

    var serial: Serial = .{};
    serial.writeControl(&io, 0x80);
    for (0..20) |_| serial.tick(255, &io);

    try std.testing.expect(!serial.active);
    try std.testing.expect((io.read(@intFromEnum(IoReg.SC)) & 0x80) != 0);
    try std.testing.expectEqual(@as(usize, 0), io.getSerialOutput().len);
}
