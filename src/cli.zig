const std = @import("std");
const clap = @import("clap");

pub const Options = struct {
    help: bool = false,
    debug: bool = false,
    max_steps: ?usize = null,
    breakpoint: ?u16 = null,
    headless: bool = false,
    mooneye_test: bool = false,
    rom_path: ?[]const u8 = null,
};

pub const ParseError = error{
    MissingOptionValue,
    InvalidStepCount,
    InvalidBreakpoint,
    UnknownOption,
    TooManyRomPaths,
    OutOfMemory,
};

const params = clap.parseParamsComptime(
    \\-h, --help              Display this help and exit
    \\-d, --debug             Enable instruction tracing
    \\-s, --steps <COUNT>     Stop after COUNT instructions
    \\-b, --breakpoint <HEX>  Stop before executing address HEX
    \\    --headless          Run without graphics
    \\    --mooneye-test      Run a Mooneye ROM and report its result
    \\<ROM>...
    \\
);

const value_parsers = .{
    .COUNT = parseStepCount,
    .HEX = parseAddress,
    .ROM = clap.parsers.string,
};

/// Parse already-sliced arguments through zig-clap. Returned strings continue
/// to borrow from args; only clap's temporary result arrays are allocated.
pub fn parse(allocator: std.mem.Allocator, args: []const []const u8) ParseError!Options {
    var iter: clap.args.SliceIterator = .{ .args = args };
    var result = clap.parseEx(clap.Help, &params, value_parsers, &iter, .{
        .allocator = allocator,
    }) catch |err| return switch (err) {
        error.MissingValue => error.MissingOptionValue,
        error.InvalidStepCount => error.InvalidStepCount,
        error.InvalidBreakpoint => error.InvalidBreakpoint,
        error.OutOfMemory => error.OutOfMemory,
        else => error.UnknownOption,
    };
    defer result.deinit();

    const rom_paths = result.positionals[0];
    if (rom_paths.len > 1) return error.TooManyRomPaths;

    return .{
        .help = result.args.help != 0,
        .debug = result.args.debug != 0,
        .max_steps = result.args.steps,
        .breakpoint = result.args.breakpoint,
        .headless = result.args.headless != 0,
        .mooneye_test = result.args.@"mooneye-test" != 0,
        .rom_path = if (rom_paths.len == 1) rom_paths[0] else null,
    };
}

fn parseStepCount(text: []const u8) error{InvalidStepCount}!usize {
    return std.fmt.parseInt(usize, text, 10) catch error.InvalidStepCount;
}

/// Breakpoint addresses are hexadecimal, with or without a 0x prefix.
fn parseAddress(text: []const u8) error{InvalidBreakpoint}!u16 {
    const digits = if (std.mem.startsWith(u8, text, "0x") or std.mem.startsWith(u8, text, "0X"))
        text[2..]
    else
        text;
    if (digits.len == 0) return error.InvalidBreakpoint;
    return std.fmt.parseInt(u16, digits, 16) catch error.InvalidBreakpoint;
}

test "parse command-line options" {
    const options = try parse(std.testing.allocator, &.{ "-d", "--steps=250", "-b", "0x0150", "--headless", "--mooneye-test", "game.gb" });
    try std.testing.expect(options.debug);
    try std.testing.expect(options.headless);
    try std.testing.expect(options.mooneye_test);
    try std.testing.expectEqual(@as(?usize, 250), options.max_steps);
    try std.testing.expectEqual(@as(?u16, 0x0150), options.breakpoint);
    try std.testing.expectEqualStrings("game.gb", options.rom_path.?);
}

test "double dash permits a ROM path beginning with a dash" {
    const options = try parse(std.testing.allocator, &.{ "--", "-test.gb" });
    try std.testing.expectEqualStrings("-test.gb", options.rom_path.?);
}

test "invalid options and values are reported" {
    try std.testing.expectError(error.UnknownOption, parse(std.testing.allocator, &.{"--wat"}));
    try std.testing.expectError(error.MissingOptionValue, parse(std.testing.allocator, &.{"--steps"}));
    try std.testing.expectError(error.InvalidStepCount, parse(std.testing.allocator, &.{ "--steps", "many" }));
    try std.testing.expectError(error.InvalidBreakpoint, parse(std.testing.allocator, &.{ "--breakpoint", "xyz" }));
    try std.testing.expectError(error.TooManyRomPaths, parse(std.testing.allocator, &.{ "one.gb", "two.gb" }));
}
