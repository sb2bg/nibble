const std = @import("std");
const nibble = @import("nibble");
const cli = @import("agent_bench_cli.zig");

const action_repeats = [_]u16{ 1, 2, 4, 8 };
const target_branch_resets = 10_000;

const help_text =
    \\Usage: zig build agent-bench -Doptimize=ReleaseFast -- [OPTIONS] <ROM_FILE>
    \\
    \\Measure Nibble's complete local-agent data path.
    \\
    \\Options:
    \\  -h, --help                   Display this help and exit
    \\  -e, --environments <COUNT>   Preallocated environments (default 128)
    \\  -i, --iterations <COUNT>     Steps per repeat value (default 10)
    \\  -w, --warmup <COUNT>         Warmup steps per repeat value (default 2)
    \\  --workers <COUNT>             Explicit std.Io workers (default host CPUs)
    \\  --encoding <FORMAT>           packed, raw, or none (default packed)
    \\
;

pub fn main(init: std.process.Init) !void {
    const argv = try init.minimal.args.toSlice(init.arena.allocator());
    const options = cli.parse(init.gpa, argv[1..]) catch |err| {
        std.process.fatal("invalid agent benchmark arguments ({s}); try '--help'", .{@errorName(err)});
    };

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file_writer: std.Io.File.Writer = .init(.stdout(), init.io, &stdout_buffer);
    const stdout = &stdout_file_writer.interface;
    if (options.help) {
        try stdout.writeAll(help_text);
        try stdout.flush();
        return;
    }

    const rom_path = options.rom_path orelse
        std.process.fatal("ROM file path required; try '--help'", .{});
    const frame_encoding = toFrameEncoding(options.encoding);
    const cartridge = nibble.Cartridge.load(init.gpa, init.io, rom_path) catch |err| {
        std.process.fatal("unable to load ROM '{s}': {s}", .{ rom_path, @errorName(err) });
    };
    var seed = nibble.Machine.init(init.gpa, cartridge, .{
        .capture_audio = false,
        .capture_video = frame_encoding != null,
    });
    defer seed.deinit();
    var checkpoint = try seed.captureOwned(init.gpa);
    defer checkpoint.deinit();
    var runtime = try nibble.agent.AgentRuntime.init(
        init.gpa,
        init.io,
        &seed,
        options.environments,
        .{ .worker_count = options.workers },
    );
    defer runtime.deinit();

    const branches = try init.gpa.alloc(nibble.agent.BranchId, options.environments);
    defer init.gpa.free(branches);
    const actions = try init.gpa.alloc(nibble.agent.Action, options.environments);
    defer init.gpa.free(actions);
    const results = try init.gpa.alloc(nibble.agent.ActionResult, options.environments);
    defer init.gpa.free(results);
    const states = try init.gpa.alloc(nibble.agent.StateObservation, options.environments);
    defer init.gpa.free(states);
    const frame_bytes = try runtime.requiredFrameBytes(options.environments, frame_encoding);
    const frames = try init.gpa.alloc(u8, frame_bytes);
    defer init.gpa.free(frames);
    try runtime.pool.acquireMany(&checkpoint, branches);

    const cartridge_info = seed.inspectCartridge();
    try stdout.print("Nibble agent workload benchmark\n", .{});
    try stdout.print("  ROM: {s} ({s})\n", .{
        cartridge_info.header.getTitle(),
        @tagName(cartridge_info.mapper.mbc_type),
    });
    try stdout.print("  Environments: {d} preallocated, {d} std.Io workers\n", .{
        options.environments,
        runtime.worker_count,
    });
    try stdout.print("  Observation: {s}, {Bi:.2} per batch\n", .{
        @tagName(options.encoding),
        frame_bytes,
    });
    try stdout.print("  Mutable pool estimate: {Bi:.2}; checkpoint: {Bi:.2}\n", .{
        runtime.pool.estimatedMutableBytes(),
        checkpoint.byteSize(),
    });
    try stdout.writeAll("\n  repeat  env-steps/s    frames/s       obs GiB/s    policy-shim/s\n");

    var policy_identity: u64 = 0;
    for (action_repeats) |repeat| {
        try restoreAll(&runtime, branches, &checkpoint);
        initializeActions(actions, repeat);
        for (0..options.warmup) |_| {
            const batch = try runtime.step(branches, actions, .{
                .frame_encoding = frame_encoding,
            }, .{ .results = results, .states = states, .frames = frames });
            policy_identity +%= selectActions(batch, actions, repeat);
        }

        try restoreAll(&runtime, branches, &checkpoint);
        initializeActions(actions, repeat);
        var simulation_ns: u64 = 0;
        var policy_ns: u64 = 0;
        var simulated_frames: usize = 0;
        for (0..options.iterations) |_| {
            const simulation_start = std.Io.Clock.awake.now(init.io).nanoseconds;
            const batch = try runtime.step(branches, actions, .{
                .frame_encoding = frame_encoding,
            }, .{ .results = results, .states = states, .frames = frames });
            const simulation_finish = std.Io.Clock.awake.now(init.io).nanoseconds;
            simulation_ns += @intCast(simulation_finish - simulation_start);
            for (batch.results) |result| {
                if (result.timed_out) return error.AgentFrameTimeout;
                simulated_frames += result.frames_completed;
            }

            const policy_start = std.Io.Clock.awake.now(init.io).nanoseconds;
            policy_identity +%= selectActions(batch, actions, repeat);
            const policy_finish = std.Io.Clock.awake.now(init.io).nanoseconds;
            policy_ns += @intCast(policy_finish - policy_start);
        }

        const environment_steps = options.environments * options.iterations;
        const simulation_seconds = seconds(simulation_ns);
        const policy_seconds = seconds(policy_ns);
        const observation_gib = @as(f64, @floatFromInt(frame_bytes * options.iterations)) /
            (1024.0 * 1024.0 * 1024.0);
        try stdout.print("  {d: >6}  {d: >11.0}  {d: >12.0}  {d: >12.3}  {d: >13.0}\n", .{
            repeat,
            @as(f64, @floatFromInt(environment_steps)) / simulation_seconds,
            @as(f64, @floatFromInt(simulated_frames)) / simulation_seconds,
            if (simulation_seconds == 0) 0 else observation_gib / simulation_seconds,
            if (policy_seconds == 0) 0 else @as(f64, @floatFromInt(environment_steps)) / policy_seconds,
        });
    }

    const reset_rounds = @max(@as(usize, 1), target_branch_resets / options.environments);
    const reset_start = std.Io.Clock.awake.now(init.io).nanoseconds;
    for (0..reset_rounds) |_| {
        for (branches) |branch| try runtime.pool.release(branch);
        try runtime.pool.acquireMany(&checkpoint, branches);
    }
    const reset_finish = std.Io.Clock.awake.now(init.io).nanoseconds;
    const reset_count = reset_rounds * options.environments;
    const resets_per_second = @as(f64, @floatFromInt(reset_count)) /
        seconds(@intCast(reset_finish - reset_start));

    var replay_identity: u64 = 0;
    for (branches) |branch| replay_identity +%= (try runtime.pool.machine(branch)).observableDigest();
    try stdout.print("\n  Allocation-free branch resets/s: {d:.0}\n", .{resets_per_second});
    try stdout.print("  Policy identity: {X:0>16}\n", .{policy_identity});
    try stdout.print("  Replay identity: {X:0>16}\n", .{replay_identity});
    try stdout.flush();
}

fn toFrameEncoding(encoding: cli.Encoding) ?nibble.agent.FrameEncoding {
    return switch (encoding) {
        .packed_2bpp => .packed_2bpp,
        .palette_u8 => .palette_u8,
        .none => null,
    };
}

fn restoreAll(
    runtime: *nibble.agent.AgentRuntime,
    branches: []const nibble.agent.BranchId,
    checkpoint: *const nibble.OwnedSnapshot,
) !void {
    for (branches) |branch| try runtime.pool.restore(branch, checkpoint);
}

fn initializeActions(actions: []nibble.agent.Action, repeat: u16) void {
    for (actions, 0..) |*action, index| {
        action.* = .{
            .buttons = buttonsForIndex(index),
            .hold_frames = repeat,
        };
    }
}

/// Deterministic stand-in that consumes exactly the same visual batch contract
/// as MLX or PyTorch. It deliberately performs little work: the benchmark
/// reports this time separately and never presents it as neural inference.
fn selectActions(
    batch: nibble.agent.runtime.StepBatch,
    actions: []nibble.agent.Action,
    repeat: u16,
) u64 {
    var identity: u64 = 0;
    for (actions, 0..) |*action, index| {
        var signature: u64 = 0;
        if (batch.visual) |visual| {
            const frame = visual.frame(index).?;
            var offset: usize = 0;
            while (offset < frame.len) : (offset += 257) {
                signature = std.math.rotl(u64, signature, 7) ^ frame[offset];
            }
        } else {
            // Timing-only runs have no pixels to consume, so a cheap state
            // value keeps the benchmark loop observable to the optimizer.
            signature = batch.states[index].cpu.pc;
        }
        identity +%= signature;
        action.* = .{
            .buttons = buttonsForIndex(@intCast(signature)),
            .hold_frames = repeat,
        };
    }
    return identity;
}

fn buttonsForIndex(index: usize) nibble.Buttons {
    return switch (index & 7) {
        0 => .{ .right = true },
        1 => .{ .left = true },
        2 => .{ .up = true },
        3 => .{ .down = true },
        4 => .{ .a = true },
        5 => .{ .b = true },
        6 => .{ .start = true },
        else => .{ .select = true },
    };
}

fn seconds(nanoseconds: u64) f64 {
    return @as(f64, @floatFromInt(nanoseconds)) / std.time.ns_per_s;
}

test "policy shim consumes packed observations deterministically" {
    var results = [_]nibble.agent.ActionResult{.{
        .frames_completed = 1,
        .instructions = 1,
        .cycles = 4,
        .timed_out = false,
    }};
    var states = [_]nibble.agent.StateObservation{.{
        .cpu = .{
            .af = 0,
            .bc = 0,
            .de = 0,
            .hl = 0,
            .sp = 0,
            .pc = 0x1234,
            .ime = false,
            .halted = false,
            .stopped = false,
            .cycles = 4,
        },
        .instructions = 1,
        .frames = 1,
        .digest = null,
    }};
    var frame = [_]u8{0xA5} ** nibble.agent.observation.packed_frame_bytes;
    const batch: nibble.agent.runtime.StepBatch = .{
        .results = &results,
        .states = &states,
        .visual = .{ .encoding = .packed_2bpp, .frames = &frame, .count = 1 },
    };
    var first: [1]nibble.agent.Action = undefined;
    var second: [1]nibble.agent.Action = undefined;
    try std.testing.expectEqual(selectActions(batch, &first, 4), selectActions(batch, &second, 4));
    try std.testing.expectEqualDeep(first, second);
}
