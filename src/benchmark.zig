const std = @import("std");
const nibble = @import("nibble");
const bench_cli = @import("bench_cli.zig");

const dmg_clock_hz = 4_194_304.0;

const help_text =
    \\Usage: zig build bench -Doptimize=ReleaseFast -- [OPTIONS] <ROM_FILE>
    \\
    \\Measure deterministic, frontend-free Nibble execution.
    \\
    \\Options:
    \\  -h, --help             Display this help and exit
    \\  -s, --steps <COUNT>    Instructions measured per trial (default 10000000)
    \\  -w, --warmup <COUNT>   Warmup instructions (default 1000000)
    \\  -t, --trials <TRIALS>  Trial count, 1-21 (default 5)
    \\
;

pub fn main(init: std.process.Init) !void {
    const argv = try init.minimal.args.toSlice(init.arena.allocator());
    const options = bench_cli.parse(init.gpa, argv[1..]) catch |err| {
        std.process.fatal("invalid benchmark arguments ({s}); try '--help'", .{@errorName(err)});
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
    const cartridge = nibble.Cartridge.load(init.gpa, init.io, rom_path) catch |err| {
        std.process.fatal("unable to load ROM '{s}': {s}", .{ rom_path, @errorName(err) });
    };

    var machine = nibble.Machine.init(init.gpa, cartridge, .{ .capture_audio = false });
    defer machine.deinit();
    const initial = machine.capture();

    if (options.warmup != 0) machine.runInstructions(options.warmup);

    var elapsed_ns: [21]u64 = undefined;
    var expected_digest: ?u64 = null;
    var measured_cycles: u64 = 0;
    var measured_frames: usize = 0;

    for (0..options.trials) |trial| {
        machine.restore(initial);
        const start_cycles = machine.cpu.cycles;
        const start_frames = machine.frames;
        const start = std.Io.Clock.awake.now(init.io).nanoseconds;
        machine.runInstructions(options.steps);
        const finish = std.Io.Clock.awake.now(init.io).nanoseconds;

        elapsed_ns[trial] = @intCast(finish - start);
        measured_cycles = machine.cpu.cycles - start_cycles;
        measured_frames = machine.frames - start_frames;

        const digest = machine.observableDigest();
        if (expected_digest) |expected| {
            if (digest != expected) return error.NonDeterministicBenchmark;
        } else {
            expected_digest = digest;
        }
    }

    insertionSort(elapsed_ns[0..options.trials]);
    const median_ns = elapsed_ns[options.trials / 2];
    const seconds = @as(f64, @floatFromInt(median_ns)) / std.time.ns_per_s;
    const cycles_per_second = @as(f64, @floatFromInt(measured_cycles)) / seconds;
    const instructions_per_second = @as(f64, @floatFromInt(options.steps)) / seconds;
    const frames_per_second = @as(f64, @floatFromInt(measured_frames)) / seconds;
    const cartridge_info = machine.inspectCartridge();

    try stdout.print("Nibble headless benchmark\n", .{});
    try stdout.print("  ROM: {s} ({s})\n", .{
        cartridge_info.header.getTitle(),
        @tagName(cartridge_info.mapper.mbc_type),
    });
    try stdout.print("  Workload: {d} instructions, {d} trial{s}\n", .{
        options.steps,
        options.trials,
        if (options.trials == 1) "" else "s",
    });
    try stdout.print("  Median: {d:.3} s\n", .{seconds});
    try stdout.print("  Instructions/s: {d:.3}\n", .{instructions_per_second});
    try stdout.print("  T-cycles/s: {d:.3}\n", .{cycles_per_second});
    try stdout.print("  Real-time factor: {d:.2}x\n", .{cycles_per_second / dmg_clock_hz});
    try stdout.print("  Completed frames/s: {d:.3}\n", .{frames_per_second});
    try stdout.print("  State digest: {X:0>16}\n", .{expected_digest.?});
    try stdout.flush();
}

fn insertionSort(values: []u64) void {
    var index: usize = 1;
    while (index < values.len) : (index += 1) {
        const value = values[index];
        var insert_at = index;
        while (insert_at > 0 and values[insert_at - 1] > value) : (insert_at -= 1) {
            values[insert_at] = values[insert_at - 1];
        }
        values[insert_at] = value;
    }
}

test "benchmark median sort is deterministic" {
    var values = [_]u64{ 9, 2, 7, 1, 5 };
    insertionSort(&values);
    try std.testing.expectEqualSlices(u64, &.{ 1, 2, 5, 7, 9 }, &values);
}
