const std = @import("std");
const clap = @import("clap");

pub const Encoding = enum {
    packed_2bpp,
    palette_u8,
    none,
};

pub const Options = struct {
    help: bool = false,
    environments: usize = 128,
    iterations: usize = 10,
    warmup: usize = 2,
    workers: ?usize = null,
    encoding: Encoding = .packed_2bpp,
    rom_path: ?[]const u8 = null,
};

pub const ParseError = error{
    MissingOptionValue,
    InvalidCount,
    InvalidEncoding,
    UnknownOption,
    MissingRomPath,
    TooManyRomPaths,
    OutOfMemory,
};

const params = clap.parseParamsComptime(
    \\-h, --help                   Display this help and exit
    \\-e, --environments <COUNT>   Preallocated environment count
    \\-i, --iterations <COUNT>     Measured policy steps per action repeat
    \\-w, --warmup <COUNT>         Warmup policy steps per action repeat
    \\--workers <COUNT>             Explicit std.Io worker count
    \\--encoding <FORMAT>           packed, raw, or none
    \\<ROM>...
    \\
);

const value_parsers = .{
    .COUNT = parseCount,
    .FORMAT = parseEncoding,
    .ROM = clap.parsers.string,
};

pub fn parse(allocator: std.mem.Allocator, args: []const []const u8) ParseError!Options {
    var iter: clap.args.SliceIterator = .{ .args = args };
    var result = clap.parseEx(clap.Help, &params, value_parsers, &iter, .{
        .allocator = allocator,
    }) catch |err| return switch (err) {
        error.MissingValue => error.MissingOptionValue,
        error.InvalidCount => error.InvalidCount,
        error.InvalidEncoding => error.InvalidEncoding,
        error.OutOfMemory => error.OutOfMemory,
        else => error.UnknownOption,
    };
    defer result.deinit();

    const rom_paths = result.positionals[0];
    if (rom_paths.len > 1) return error.TooManyRomPaths;
    const environments = result.args.environments orelse 128;
    const iterations = result.args.iterations orelse 10;
    const workers = result.args.workers;
    if (environments == 0 or iterations == 0 or (workers != null and workers.? == 0)) {
        return error.InvalidCount;
    }

    return .{
        .help = result.args.help != 0,
        .environments = environments,
        .iterations = iterations,
        .warmup = result.args.warmup orelse 2,
        .workers = workers,
        .encoding = result.args.encoding orelse .packed_2bpp,
        .rom_path = if (rom_paths.len == 1) rom_paths[0] else null,
    };
}

fn parseCount(text: []const u8) error{InvalidCount}!usize {
    return std.fmt.parseInt(usize, text, 10) catch error.InvalidCount;
}

fn parseEncoding(text: []const u8) error{InvalidEncoding}!Encoding {
    if (std.mem.eql(u8, text, "packed")) return .packed_2bpp;
    if (std.mem.eql(u8, text, "raw")) return .palette_u8;
    if (std.mem.eql(u8, text, "none")) return .none;
    return error.InvalidEncoding;
}

test "agent benchmark CLI parses runtime controls" {
    const options = try parse(std.testing.allocator, &.{
        "--environments", "32", "--iterations=4", "--warmup", "0",
        "--workers",      "3",  "--encoding",     "raw",      "game.gb",
    });
    try std.testing.expectEqual(@as(usize, 32), options.environments);
    try std.testing.expectEqual(@as(usize, 4), options.iterations);
    try std.testing.expectEqual(@as(usize, 0), options.warmup);
    try std.testing.expectEqual(@as(?usize, 3), options.workers);
    try std.testing.expectEqual(Encoding.palette_u8, options.encoding);
    try std.testing.expectEqualStrings("game.gb", options.rom_path.?);
}

test "agent benchmark CLI rejects zero-sized workloads" {
    try std.testing.expectError(
        error.InvalidCount,
        parse(std.testing.allocator, &.{ "--environments", "0", "game.gb" }),
    );
    try std.testing.expectError(
        error.InvalidCount,
        parse(std.testing.allocator, &.{ "--workers", "0", "game.gb" }),
    );
}
