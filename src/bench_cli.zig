const std = @import("std");
const clap = @import("clap");

pub const Options = struct {
    help: bool = false,
    steps: usize = 10_000_000,
    warmup: usize = 1_000_000,
    trials: u8 = 5,
    rom_path: ?[]const u8 = null,
};

pub const ParseError = error{
    MissingOptionValue,
    InvalidCount,
    InvalidTrialCount,
    UnknownOption,
    MissingRomPath,
    TooManyRomPaths,
    OutOfMemory,
};

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit
    \\-s, --steps <COUNT>    Instructions measured per trial
    \\-w, --warmup <COUNT>   Warmup instructions before measurement
    \\-t, --trials <TRIALS>  Trial count (1-21)
    \\<ROM>...
    \\
);

const value_parsers = .{
    .COUNT = parseCount,
    .TRIALS = parseTrials,
    .ROM = clap.parsers.string,
};

pub fn parse(allocator: std.mem.Allocator, args: []const []const u8) ParseError!Options {
    var iter: clap.args.SliceIterator = .{ .args = args };
    var result = clap.parseEx(clap.Help, &params, value_parsers, &iter, .{
        .allocator = allocator,
    }) catch |err| return switch (err) {
        error.MissingValue => error.MissingOptionValue,
        error.InvalidCount => error.InvalidCount,
        error.InvalidTrialCount => error.InvalidTrialCount,
        error.OutOfMemory => error.OutOfMemory,
        else => error.UnknownOption,
    };
    defer result.deinit();

    const rom_paths = result.positionals[0];
    if (rom_paths.len > 1) return error.TooManyRomPaths;

    return .{
        .help = result.args.help != 0,
        .steps = result.args.steps orelse 10_000_000,
        .warmup = result.args.warmup orelse 1_000_000,
        .trials = result.args.trials orelse 5,
        .rom_path = if (rom_paths.len == 1) rom_paths[0] else null,
    };
}

fn parseCount(text: []const u8) error{InvalidCount}!usize {
    return std.fmt.parseInt(usize, text, 10) catch error.InvalidCount;
}

fn parseTrials(text: []const u8) error{InvalidTrialCount}!u8 {
    const count = std.fmt.parseInt(u8, text, 10) catch return error.InvalidTrialCount;
    if (count == 0 or count > 21) return error.InvalidTrialCount;
    return count;
}

test "benchmark CLI parses workload controls" {
    const options = try parse(std.testing.allocator, &.{
        "--steps", "250", "--warmup=10", "--trials", "3", "game.gb",
    });
    try std.testing.expectEqual(@as(usize, 250), options.steps);
    try std.testing.expectEqual(@as(usize, 10), options.warmup);
    try std.testing.expectEqual(@as(u8, 3), options.trials);
    try std.testing.expectEqualStrings("game.gb", options.rom_path.?);
}

test "benchmark CLI rejects invalid trial counts" {
    try std.testing.expectError(error.InvalidTrialCount, parse(std.testing.allocator, &.{ "--trials", "0", "game.gb" }));
    try std.testing.expectError(error.InvalidTrialCount, parse(std.testing.allocator, &.{ "--trials", "22", "game.gb" }));
}
