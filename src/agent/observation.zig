const std = @import("std");
const machine_mod = @import("../machine.zig");
const ppu_mod = @import("../ppu/ppu.zig");

pub const width = ppu_mod.SCREEN_WIDTH;
pub const height = ppu_mod.SCREEN_HEIGHT;
pub const pixel_count = width * height;
pub const raw_frame_bytes = pixel_count;
pub const packed_frame_bytes = pixel_count / 4;

comptime {
    if (pixel_count % 4 != 0) @compileError("2bpp frames require a multiple of four pixels");
    if (@sizeOf(ppu_mod.DmgColor) != 1) @compileError("raw observations require byte-sized palette indices");
    if (@sizeOf([height][width]ppu_mod.DmgColor) != raw_frame_bytes) {
        @compileError("the framebuffer contains unexpected padding");
    }
}

/// Stable visual formats intended for model input rather than presentation.
pub const FrameEncoding = enum {
    /// One byte per pixel with values 0 through 3.
    palette_u8,
    /// Four palette indices per byte, ordered from bits 7..6 to bits 1..0.
    packed_2bpp,

    pub fn bytesPerFrame(self: FrameEncoding) usize {
        return switch (self) {
            .palette_u8 => raw_frame_bytes,
            .packed_2bpp => packed_frame_bytes,
        };
    }
};

pub const Error = error{
    BufferTooSmall,
    ObservationCountOverflow,
};

/// A read-only view suitable for one batched model invocation.
pub const Batch = struct {
    encoding: FrameEncoding,
    frames: []const u8,
    count: usize,

    pub fn frame(self: Batch, index: usize) ?[]const u8 {
        if (index >= self.count) return null;
        const stride = self.encoding.bytesPerFrame();
        const offset = index * stride;
        return self.frames[offset..][0..stride];
    }
};

pub fn requiredBytes(encoding: FrameEncoding, count: usize) Error!usize {
    return std.math.mul(usize, encoding.bytesPerFrame(), count) catch
        error.ObservationCountOverflow;
}

/// Encode a framebuffer directly into caller-owned storage without allocating.
/// Extra destination capacity is left untouched.
pub fn writeFrame(
    frame: *const [height][width]ppu_mod.DmgColor,
    encoding: FrameEncoding,
    destination: []u8,
) Error![]u8 {
    const required = encoding.bytesPerFrame();
    if (destination.len < required) return error.BufferTooSmall;
    const output = destination[0..required];
    const palette = std.mem.asBytes(frame);

    switch (encoding) {
        .palette_u8 => @memcpy(output, palette),
        .packed_2bpp => {
            for (output, 0..) |*byte, index| {
                const pixels = palette[index * 4 ..][0..4];
                byte.* = (@as(u8, pixels[0]) << 6) |
                    (@as(u8, pixels[1]) << 4) |
                    (@as(u8, pixels[2]) << 2) |
                    pixels[3];
            }
        },
    }
    return output;
}

pub fn writeMachineFrame(
    machine: *const machine_mod.Machine,
    encoding: FrameEncoding,
    destination: []u8,
) Error![]u8 {
    return writeFrame(machine.observe().frame_buffer, encoding, destination);
}

/// Encode a contiguous machine slice into one model-ready batch buffer.
pub fn writeMachines(
    machines: []const machine_mod.Machine,
    encoding: FrameEncoding,
    destination: []u8,
) Error!Batch {
    const required = try requiredBytes(encoding, machines.len);
    if (destination.len < required) return error.BufferTooSmall;

    const stride = encoding.bytesPerFrame();
    for (machines, 0..) |*machine, index| {
        _ = try writeMachineFrame(machine, encoding, destination[index * stride ..]);
    }
    return .{
        .encoding = encoding,
        .frames = destination[0..required],
        .count = machines.len,
    };
}

test "raw and packed observations preserve palette order" {
    var frame = [_][width]ppu_mod.DmgColor{
        [_]ppu_mod.DmgColor{.White} ** width,
    } ** height;
    frame[0][0] = .White;
    frame[0][1] = .LightGray;
    frame[0][2] = .DarkGray;
    frame[0][3] = .Black;

    var raw: [raw_frame_bytes]u8 = undefined;
    const raw_view = try writeFrame(&frame, .palette_u8, &raw);
    try std.testing.expectEqualSlices(u8, &.{ 0, 1, 2, 3 }, raw_view[0..4]);

    var packed_output: [packed_frame_bytes]u8 = undefined;
    const packed_view = try writeFrame(&frame, .packed_2bpp, &packed_output);
    try std.testing.expectEqual(@as(u8, 0b00_01_10_11), packed_view[0]);
    try std.testing.expectEqual(@as(u8, 0), packed_view[1]);
}

test "observation writers validate capacity before writing" {
    var frame = [_][width]ppu_mod.DmgColor{
        [_]ppu_mod.DmgColor{.White} ** width,
    } ** height;
    var short = [_]u8{0xA5} ** (packed_frame_bytes - 1);

    try std.testing.expectError(
        error.BufferTooSmall,
        writeFrame(&frame, .packed_2bpp, &short),
    );
    try std.testing.expect(std.mem.allEqual(u8, &short, 0xA5));
}

test "batch frame views are contiguous and ordered" {
    var seed = machine_mod.Machine.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();
    var machines = [_]machine_mod.Machine{
        try seed.fork(std.testing.allocator),
        try seed.fork(std.testing.allocator),
    };
    defer for (&machines) |*machine| machine.deinit();
    machines[0].ppu.frame_buffer[0][0] = .LightGray;
    machines[1].ppu.frame_buffer[0][0] = .Black;

    var frames: [packed_frame_bytes * machines.len]u8 = undefined;
    const batch = try writeMachines(&machines, .packed_2bpp, &frames);
    try std.testing.expectEqual(@as(usize, 2), batch.count);
    try std.testing.expectEqual(@as(u8, 0b01_00_00_00), batch.frame(0).?[0]);
    try std.testing.expectEqual(@as(u8, 0b11_00_00_00), batch.frame(1).?[0]);
    try std.testing.expectEqual(@as(?[]const u8, null), batch.frame(2));
}
