const std = @import("std");
const action_mod = @import("action.zig");
const machine_mod = @import("../machine.zig");
const observation_mod = @import("observation.zig");
const pool_mod = @import("pool.zig");

const Allocator = std.mem.Allocator;
const Machine = machine_mod.Machine;

pub const InitOptions = struct {
    /// Null resolves the host CPU count once during initialization. Supplying a
    /// value makes scheduling explicit and reproducible for benchmarks.
    worker_count: ?usize = null,
};

pub const StepOptions = struct {
    /// Null performs timing-only simulation and writes no visual bytes.
    frame_encoding: ?observation_mod.FrameEncoding = .packed_2bpp,
    capture_audio: bool = false,
    max_instructions_per_frame: usize = 1_000_000,
    /// Digests are useful replay identities but hash most mutable memory, so
    /// high-throughput training should leave them disabled outside audits.
    include_digest: bool = false,
};

pub const StateObservation = struct {
    cpu: machine_mod.CpuObservation,
    instructions: usize,
    frames: usize,
    digest: ?u64,

    fn capture(machine: *const Machine, include_digest: bool) StateObservation {
        const observed = machine.observe();
        return .{
            .cpu = observed.cpu,
            .instructions = observed.instructions,
            .frames = observed.frames,
            .digest = if (include_digest) machine.observableDigest() else null,
        };
    }
};

/// Caller-owned storage for one step. Keeping buffers outside the runtime lets
/// a model backend pin, map, or otherwise manage their memory directly.
pub const StepBuffers = struct {
    results: []action_mod.ActionResult,
    states: []StateObservation,
    frames: []u8,
};

/// Borrowed results remain valid until the caller reuses `StepBuffers`.
pub const StepBatch = struct {
    results: []const action_mod.ActionResult,
    states: []const StateObservation,
    visual: ?observation_mod.Batch,
};

pub const Error = pool_mod.Error || error{
    InvalidWorkerCount,
    InputCountMismatch,
    ResultCountMismatch,
    StateCountMismatch,
    DuplicateBranch,
    EmptyAction,
    BufferTooSmall,
    ObservationCountOverflow,
};

/// Host-side agent composition root. The deterministic `Machine` remains free
/// of host IO; this layer accepts `std.Io` and uses its persistent worker pool
/// to step independent branches in parallel.
pub const AgentRuntime = struct {
    allocator: Allocator,
    io: std.Io,
    pool: pool_mod.MachinePool,
    worker_count: usize,
    validation_marks: []u32,
    validation_epoch: u32 = 0,

    pub fn init(
        allocator: Allocator,
        io: std.Io,
        seed: *const Machine,
        slot_count: usize,
        options: InitOptions,
    ) (Allocator.Error || Error)!AgentRuntime {
        const requested_workers = options.worker_count orelse
            (std.Thread.getCpuCount() catch 1);
        if (requested_workers == 0) return error.InvalidWorkerCount;

        var pool = try pool_mod.MachinePool.initForked(allocator, seed, slot_count);
        errdefer pool.deinit();
        const validation_marks = try allocator.alloc(u32, slot_count);
        @memset(validation_marks, 0);

        return .{
            .allocator = allocator,
            .io = io,
            .pool = pool,
            .worker_count = requested_workers,
            .validation_marks = validation_marks,
        };
    }

    pub fn deinit(self: *AgentRuntime) void {
        self.allocator.free(self.validation_marks);
        self.pool.deinit();
        self.* = undefined;
    }

    pub fn requiredFrameBytes(
        _: *const AgentRuntime,
        branch_count: usize,
        encoding: ?observation_mod.FrameEncoding,
    ) error{ObservationCountOverflow}!usize {
        const concrete = encoding orelse return 0;
        return observation_mod.requiredBytes(concrete, branch_count) catch
            error.ObservationCountOverflow;
    }

    /// Execute heterogeneous temporal actions and write one contiguous model
    /// batch. All sizes, IDs, duplicates, and empty actions are validated
    /// before any machine is advanced.
    pub fn step(
        self: *AgentRuntime,
        branches: []const pool_mod.BranchId,
        actions: []const action_mod.Action,
        options: StepOptions,
        buffers: StepBuffers,
    ) (std.Io.Cancelable || Error)!StepBatch {
        try self.validateStep(branches, actions, options, buffers);

        const required_frames = try self.requiredFrameBytes(branches.len, options.frame_encoding);
        const frame_storage = buffers.frames[0..required_frames];
        if (branches.len != 0) {
            const active_workers = @min(self.worker_count, branches.len);
            const per_worker = std.math.divCeil(usize, branches.len, active_workers) catch unreachable;

            var group: std.Io.Group = .init;
            var start: usize = 0;
            while (start < branches.len) {
                const end = @min(start + per_worker, branches.len);
                const frame_start = if (options.frame_encoding) |encoding|
                    start * encoding.bytesPerFrame()
                else
                    0;
                const frame_end = if (options.frame_encoding) |encoding|
                    end * encoding.bytesPerFrame()
                else
                    0;
                group.concurrent(self.io, stepChunk, .{
                    &self.pool,
                    branches[start..end],
                    actions[start..end],
                    options,
                    buffers.results[start..end],
                    buffers.states[start..end],
                    frame_storage[frame_start..frame_end],
                }) catch {
                    try group.await(self.io);
                    try stepChunk(
                        &self.pool,
                        branches[start..],
                        actions[start..],
                        options,
                        buffers.results[start..],
                        buffers.states[start..],
                        frame_storage[frame_start..],
                    );
                    return makeBatch(branches.len, options, buffers, required_frames);
                };
                start = end;
            }
            try group.await(self.io);
        }

        return makeBatch(branches.len, options, buffers, required_frames);
    }

    fn validateStep(
        self: *AgentRuntime,
        branches: []const pool_mod.BranchId,
        actions: []const action_mod.Action,
        options: StepOptions,
        buffers: StepBuffers,
    ) Error!void {
        if (actions.len != branches.len) return error.InputCountMismatch;
        if (buffers.results.len != branches.len) return error.ResultCountMismatch;
        if (buffers.states.len != branches.len) return error.StateCountMismatch;
        const required_frames = try self.requiredFrameBytes(branches.len, options.frame_encoding);
        if (buffers.frames.len < required_frames) return error.BufferTooSmall;
        for (actions) |action| {
            if (action.frameCount() == 0) return error.EmptyAction;
        }

        self.validation_epoch +%= 1;
        if (self.validation_epoch == 0) {
            @memset(self.validation_marks, 0);
            self.validation_epoch = 1;
        }
        for (branches) |branch| {
            if (!self.pool.isValid(branch)) return error.InvalidBranch;
            const slot: usize = branch.slot;
            if (self.validation_marks[slot] == self.validation_epoch) {
                return error.DuplicateBranch;
            }
            self.validation_marks[slot] = self.validation_epoch;
        }
    }
};

fn stepChunk(
    pool: *pool_mod.MachinePool,
    branches: []const pool_mod.BranchId,
    actions: []const action_mod.Action,
    options: StepOptions,
    results: []action_mod.ActionResult,
    states: []StateObservation,
    frames: []u8,
) std.Io.Cancelable!void {
    const frame_options: machine_mod.FrameStepOptions = .{
        .video = if (options.frame_encoding == null) .none else .final_frame,
        .capture_audio = options.capture_audio,
        .max_instructions_per_frame = options.max_instructions_per_frame,
    };
    const stride = if (options.frame_encoding) |encoding| encoding.bytesPerFrame() else 0;

    for (branches, actions, results, states, 0..) |branch, action, *result, *state, index| {
        // IDs and action lengths were validated on the caller thread, and no
        // branch appears in more than one worker chunk.
        const branch_machine = &pool.machines[branch.slot];
        result.* = action_mod.step(branch_machine, action, frame_options) catch unreachable;
        state.* = StateObservation.capture(branch_machine, options.include_digest);
        if (options.frame_encoding) |encoding| {
            _ = observation_mod.writeMachineFrame(
                branch_machine,
                encoding,
                frames[index * stride ..],
            ) catch unreachable;
        }
    }
}

fn makeBatch(
    count: usize,
    options: StepOptions,
    buffers: StepBuffers,
    required_frames: usize,
) StepBatch {
    return .{
        .results = buffers.results[0..count],
        .states = buffers.states[0..count],
        .visual = if (options.frame_encoding) |encoding| .{
            .encoding = encoding,
            .frames = buffers.frames[0..required_frames],
            .count = count,
        } else null,
    };
}

test "agent runtime steps branches into one deterministic visual batch" {
    var seed = Machine.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();
    var checkpoint = try seed.captureOwned(std.testing.allocator);
    defer checkpoint.deinit();

    var threaded = std.Io.Threaded.init(std.testing.allocator, .{ .environ = .empty });
    defer threaded.deinit();
    var runtime = try AgentRuntime.init(
        std.testing.allocator,
        threaded.io(),
        &seed,
        3,
        .{ .worker_count = 2 },
    );
    defer runtime.deinit();

    var branches: [3]pool_mod.BranchId = undefined;
    try runtime.pool.acquireMany(&checkpoint, &branches);
    const actions = [_]action_mod.Action{
        .{ .buttons = .{ .right = true }, .hold_frames = 1 },
        .{ .buttons = .{ .a = true }, .hold_frames = 2, .release_frames = 1 },
        .{ .buttons = .{ .start = true }, .hold_frames = 4 },
    };
    var results: [3]action_mod.ActionResult = undefined;
    var states: [3]StateObservation = undefined;
    var frames: [observation_mod.packed_frame_bytes * branches.len]u8 = undefined;

    const batch = try runtime.step(&branches, &actions, .{
        .include_digest = true,
        .max_instructions_per_frame = 100_000,
    }, .{ .results = &results, .states = &states, .frames = &frames });
    try std.testing.expectEqual(@as(usize, 3), batch.visual.?.count);
    try std.testing.expectEqual(@as(usize, 1), batch.results[0].frames_completed);
    try std.testing.expectEqual(@as(usize, 3), batch.results[1].frames_completed);
    try std.testing.expectEqual(@as(usize, 4), batch.results[2].frames_completed);
    for (branches, batch.results, batch.states) |branch, result, state| {
        try std.testing.expect(!result.timed_out);
        try std.testing.expectEqual(result.frames_completed, state.frames);
        try std.testing.expect(state.digest != null);
        try std.testing.expectEqual(@as(u8, 0xFF), (try runtime.pool.machine(branch)).bus.io.joypad_buttons);
    }

    const first_frames = frames;
    for (branches) |branch| try runtime.pool.restore(branch, &checkpoint);
    _ = try runtime.step(&branches, &actions, .{
        .include_digest = true,
        .max_instructions_per_frame = 100_000,
    }, .{ .results = &results, .states = &states, .frames = &frames });
    try std.testing.expectEqualSlices(u8, &first_frames, &frames);
}

test "agent runtime rejects duplicate branches before stepping" {
    var seed = Machine.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();
    var checkpoint = try seed.captureOwned(std.testing.allocator);
    defer checkpoint.deinit();
    var threaded = std.Io.Threaded.init(std.testing.allocator, .{ .environ = .empty });
    defer threaded.deinit();
    var runtime = try AgentRuntime.init(
        std.testing.allocator,
        threaded.io(),
        &seed,
        1,
        .{ .worker_count = 1 },
    );
    defer runtime.deinit();

    const branch = try runtime.pool.acquire(&checkpoint);
    const branches = [_]pool_mod.BranchId{ branch, branch };
    const actions = [_]action_mod.Action{.{}} ** 2;
    var results: [2]action_mod.ActionResult = undefined;
    var states: [2]StateObservation = undefined;
    const before = (try runtime.pool.machine(branch)).observableDigest();
    try std.testing.expectError(error.DuplicateBranch, runtime.step(
        &branches,
        &actions,
        .{ .frame_encoding = null },
        .{ .results = &results, .states = &states, .frames = &.{} },
    ));
    try std.testing.expectEqual(before, (try runtime.pool.machine(branch)).observableDigest());
}
