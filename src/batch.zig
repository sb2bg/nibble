const std = @import("std");
const machine_mod = @import("machine.zig");
const Machine = machine_mod.Machine;
const Buttons = machine_mod.Buttons;
const FrameStepOptions = machine_mod.FrameStepOptions;
const FrameStepResult = machine_mod.FrameStepResult;
const ResetOptions = machine_mod.ResetOptions;

pub const MachineBatch = struct {
    allocator: std.mem.Allocator,
    machines: []Machine,

    pub fn initForked(
        allocator: std.mem.Allocator,
        seed: *const Machine,
        count: usize,
    ) !MachineBatch {
        const machines = try allocator.alloc(Machine, count);
        errdefer allocator.free(machines);

        var initialized: usize = 0;
        errdefer for (machines[0..initialized]) |*machine| machine.deinit();
        while (initialized < machines.len) : (initialized += 1) {
            machines[initialized] = try seed.fork(allocator);
        }

        return .{ .allocator = allocator, .machines = machines };
    }

    pub fn deinit(self: *MachineBatch) void {
        for (self.machines) |*machine| machine.deinit();
        self.allocator.free(self.machines);
        self.* = undefined;
    }

    pub fn setButtons(self: *MachineBatch, inputs: []const Buttons) error{InputCountMismatch}!void {
        if (inputs.len != self.machines.len) return error.InputCountMismatch;
        for (self.machines, inputs) |*machine, buttons| machine.setButtons(buttons);
    }

    pub fn resetDeterministic(self: *MachineBatch, options: ResetOptions) void {
        for (self.machines) |*machine| machine.resetDeterministic(options);
    }

    /// Advance every machine by the same instruction count. Work is divided
    /// into at most one chunk per available CPU rather than spawning one task
    /// per environment. An IO backend without concurrency support falls back
    /// to completing the unsubmitted chunks on the calling thread.
    pub fn runInstructionsParallel(
        self: *MachineBatch,
        io: std.Io,
        instruction_count: usize,
    ) std.Io.Cancelable!void {
        if (self.machines.len == 0 or instruction_count == 0) return;

        const worker_count = @min(
            self.machines.len,
            std.Thread.getCpuCount() catch 1,
        );
        const machines_per_worker = std.math.divCeil(usize, self.machines.len, worker_count) catch unreachable;

        var group: std.Io.Group = .init;
        var start: usize = 0;
        while (start < self.machines.len) {
            const end = @min(start + machines_per_worker, self.machines.len);
            group.concurrent(io, runInstructionChunk, .{
                self.machines[start..end],
                instruction_count,
            }) catch {
                try group.await(io);
                runInstructionChunk(self.machines[start..], instruction_count) catch unreachable;
                return;
            };
            start = end;
        }
        try group.await(io);
    }

    pub fn runUntilFrameParallel(
        self: *MachineBatch,
        io: std.Io,
        max_instructions: usize,
        results: []?usize,
    ) (std.Io.Cancelable || error{ResultCountMismatch})!void {
        if (results.len != self.machines.len) return error.ResultCountMismatch;
        if (self.machines.len == 0) return;

        const worker_count = @min(
            self.machines.len,
            std.Thread.getCpuCount() catch 1,
        );
        const machines_per_worker = std.math.divCeil(usize, self.machines.len, worker_count) catch unreachable;

        var group: std.Io.Group = .init;
        var start: usize = 0;
        while (start < self.machines.len) {
            const end = @min(start + machines_per_worker, self.machines.len);
            group.concurrent(io, runFrameChunk, .{
                self.machines[start..end],
                max_instructions,
                results[start..end],
            }) catch {
                try group.await(io);
                runFrameChunk(
                    self.machines[start..],
                    max_instructions,
                    results[start..],
                ) catch unreachable;
                return;
            };
            start = end;
        }
        try group.await(io);
    }

    /// Advance every environment by the same number of frames with an explicit
    /// observation policy. Results are written in machine order without
    /// allocation; std.Io supplies the reusable host worker pool.
    pub fn stepFramesParallel(
        self: *MachineBatch,
        io: std.Io,
        frame_count: usize,
        options: FrameStepOptions,
        results: []FrameStepResult,
    ) (std.Io.Cancelable || error{ResultCountMismatch})!void {
        if (results.len != self.machines.len) return error.ResultCountMismatch;
        if (self.machines.len == 0) return;

        const worker_count = @min(
            self.machines.len,
            std.Thread.getCpuCount() catch 1,
        );
        const machines_per_worker = std.math.divCeil(usize, self.machines.len, worker_count) catch unreachable;

        var group: std.Io.Group = .init;
        var start: usize = 0;
        while (start < self.machines.len) {
            const end = @min(start + machines_per_worker, self.machines.len);
            group.concurrent(io, runFrameCountChunk, .{
                self.machines[start..end],
                frame_count,
                options,
                results[start..end],
            }) catch {
                try group.await(io);
                runFrameCountChunk(
                    self.machines[start..],
                    frame_count,
                    options,
                    results[start..],
                ) catch unreachable;
                return;
            };
            start = end;
        }
        try group.await(io);
    }

    /// Apply one action per environment, then advance all environments by the
    /// same action-repeat interval. Inputs and results are machine ordered;
    /// validation happens before any environment is mutated.
    pub fn stepFramesWithButtonsParallel(
        self: *MachineBatch,
        io: std.Io,
        inputs: []const Buttons,
        frame_count: usize,
        options: FrameStepOptions,
        results: []FrameStepResult,
    ) (std.Io.Cancelable || error{ InputCountMismatch, ResultCountMismatch })!void {
        if (inputs.len != self.machines.len) return error.InputCountMismatch;
        if (results.len != self.machines.len) return error.ResultCountMismatch;
        if (self.machines.len == 0) return;

        const worker_count = @min(
            self.machines.len,
            std.Thread.getCpuCount() catch 1,
        );
        const machines_per_worker = std.math.divCeil(usize, self.machines.len, worker_count) catch unreachable;

        var group: std.Io.Group = .init;
        var start: usize = 0;
        while (start < self.machines.len) {
            const end = @min(start + machines_per_worker, self.machines.len);
            group.concurrent(io, runFrameInputChunk, .{
                self.machines[start..end],
                inputs[start..end],
                frame_count,
                options,
                results[start..end],
            }) catch {
                try group.await(io);
                runFrameInputChunk(
                    self.machines[start..],
                    inputs[start..],
                    frame_count,
                    options,
                    results[start..],
                ) catch unreachable;
                return;
            };
            start = end;
        }
        try group.await(io);
    }
};

fn runInstructionChunk(
    machines: []Machine,
    instruction_count: usize,
) std.Io.Cancelable!void {
    for (machines) |*machine| machine.runInstructions(instruction_count);
}

fn runFrameChunk(
    machines: []Machine,
    max_instructions: usize,
    results: []?usize,
) std.Io.Cancelable!void {
    for (machines, results) |*machine, *result| {
        result.* = machine.runUntilFrame(max_instructions);
    }
}

fn runFrameCountChunk(
    machines: []Machine,
    frame_count: usize,
    options: FrameStepOptions,
    results: []FrameStepResult,
) std.Io.Cancelable!void {
    for (machines, results) |*machine, *result| {
        result.* = machine.stepFrames(frame_count, options);
    }
}

fn runFrameInputChunk(
    machines: []Machine,
    inputs: []const Buttons,
    frame_count: usize,
    options: FrameStepOptions,
    results: []FrameStepResult,
) std.Io.Cancelable!void {
    for (machines, inputs, results) |*machine, buttons, *result| {
        machine.setButtons(buttons);
        result.* = machine.stepFrames(frame_count, options);
    }
}

test "machine batch forks identical isolated environments" {
    var seed = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();

    var batch = try MachineBatch.initForked(std.testing.allocator, &seed, 4);
    defer batch.deinit();
    const inputs = [_]Buttons{.{ .a = true }} ** 4;
    try batch.setButtons(&inputs);

    for (batch.machines) |*machine| machine.runInstructions(100);
    const expected = batch.machines[0].observableDigest();
    for (batch.machines[1..]) |*machine| {
        try std.testing.expectEqual(expected, machine.observableDigest());
    }

    batch.machines[0].bus.wram[0] = 0x77;
    try std.testing.expect(batch.machines[0].observableDigest() != batch.machines[1].observableDigest());
}

test "machine batch frame stepping preserves result order" {
    var seed = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();

    var batch = try MachineBatch.initForked(std.testing.allocator, &seed, 3);
    defer batch.deinit();
    var results: [3]FrameStepResult = undefined;

    var threaded = std.Io.Threaded.init(std.testing.allocator, .{ .environ = .empty });
    defer threaded.deinit();
    try batch.stepFramesParallel(threaded.io(), 2, .{
        .video = .none,
        .max_instructions_per_frame = 100_000,
    }, &results);

    for (results, batch.machines) |result, machine| {
        try std.testing.expect(!result.timed_out);
        try std.testing.expectEqual(@as(usize, 2), result.frames_completed);
        try std.testing.expectEqual(machine.frames, result.frames_completed);
    }
}

test "machine batch applies heterogeneous actions before action repeat" {
    var seed = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();

    var batch = try MachineBatch.initForked(std.testing.allocator, &seed, 3);
    defer batch.deinit();
    const inputs = [_]Buttons{
        .{ .left = true },
        .{ .a = true },
        .{ .start = true },
    };
    var results: [3]FrameStepResult = undefined;

    var threaded = std.Io.Threaded.init(std.testing.allocator, .{ .environ = .empty });
    defer threaded.deinit();
    try batch.stepFramesWithButtonsParallel(threaded.io(), &inputs, 1, .{
        .video = .none,
        .max_instructions_per_frame = 100_000,
    }, &results);

    try std.testing.expectEqual(@as(u8, 0xFD), batch.machines[0].bus.io.joypad_buttons);
    try std.testing.expectEqual(@as(u8, 0xEF), batch.machines[1].bus.io.joypad_buttons);
    try std.testing.expectEqual(@as(u8, 0x7F), batch.machines[2].bus.io.joypad_buttons);
    for (results) |result| {
        try std.testing.expectEqual(@as(usize, 1), result.frames_completed);
        try std.testing.expect(!result.timed_out);
    }
}

test "machine batch rejects mismatched actions before mutation" {
    var seed = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();
    var batch = try MachineBatch.initForked(std.testing.allocator, &seed, 2);
    defer batch.deinit();

    const before = batch.machines[0].observableDigest();
    var results: [2]FrameStepResult = undefined;
    var threaded = std.Io.Threaded.init(std.testing.allocator, .{ .environ = .empty });
    defer threaded.deinit();
    try std.testing.expectError(
        error.InputCountMismatch,
        batch.stepFramesWithButtonsParallel(threaded.io(), &.{.{}}, 1, .{}, &results),
    );
    try std.testing.expectEqual(before, batch.machines[0].observableDigest());
}
