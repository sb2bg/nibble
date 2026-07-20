const std = @import("std");
const machine_mod = @import("../machine.zig");

const Machine = machine_mod.Machine;

/// A decision-level input with explicit temporal semantics.
///
/// Buttons are pressed for `hold_frames`, then released for
/// `release_frames`. The machine is always left with every button released,
/// including when frame stepping times out.
pub const Action = struct {
    buttons: machine_mod.Buttons = .{},
    hold_frames: u16 = 1,
    release_frames: u16 = 0,

    pub fn frameCount(self: Action) usize {
        return @as(usize, self.hold_frames) + @as(usize, self.release_frames);
    }
};

pub const ActionResult = machine_mod.FrameStepResult;

/// Execute one temporal action without allocating.
///
/// `.final_frame` captures only the final frame across both phases. In
/// contrast, `.every_frame` retains its literal meaning for both phases.
pub fn step(
    machine: *Machine,
    action: Action,
    options: machine_mod.FrameStepOptions,
) error{EmptyAction}!ActionResult {
    if (action.frameCount() == 0) return error.EmptyAction;

    var aggregate: ActionResult = .{
        .frames_completed = 0,
        .instructions = 0,
        .cycles = 0,
        .timed_out = false,
    };

    machine.setButtons(action.buttons);
    defer machine.setButtons(.{});

    if (action.hold_frames != 0) {
        var hold_options = options;
        if (action.release_frames != 0 and options.video == .final_frame) {
            hold_options.video = .none;
        }
        mergeResult(&aggregate, machine.stepFrames(action.hold_frames, hold_options));
        if (aggregate.timed_out) return aggregate;
    }

    machine.setButtons(.{});
    if (action.release_frames != 0) {
        mergeResult(&aggregate, machine.stepFrames(action.release_frames, options));
    }
    return aggregate;
}

fn mergeResult(aggregate: *ActionResult, phase: ActionResult) void {
    aggregate.frames_completed += phase.frames_completed;
    aggregate.instructions += phase.instructions;
    aggregate.cycles += phase.cycles;
    aggregate.timed_out = phase.timed_out;
}

test "temporal action holds, releases, and advances exact frames" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    const result = try step(&machine, .{
        .buttons = .{ .right = true, .a = true },
        .hold_frames = 2,
        .release_frames = 1,
    }, .{
        .video = .none,
        .max_instructions_per_frame = 100_000,
    });

    try std.testing.expect(!result.timed_out);
    try std.testing.expectEqual(@as(usize, 3), result.frames_completed);
    try std.testing.expectEqual(@as(usize, 3), machine.frames);
    try std.testing.expectEqual(@as(u8, 0xFF), machine.bus.io.joypad_buttons);
}

test "temporal action rejects no-op before mutating machine" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    const before = machine.observableDigest();
    try std.testing.expectError(error.EmptyAction, step(&machine, .{
        .buttons = .{ .start = true },
        .hold_frames = 0,
        .release_frames = 0,
    }, .{}));
    try std.testing.expectEqual(before, machine.observableDigest());
}

test "temporal action releases input after a timeout" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    const result = try step(&machine, .{
        .buttons = .{ .b = true },
        .hold_frames = 1,
    }, .{
        .video = .none,
        .max_instructions_per_frame = 0,
    });
    try std.testing.expect(result.timed_out);
    try std.testing.expectEqual(@as(u8, 0xFF), machine.bus.io.joypad_buttons);
}
