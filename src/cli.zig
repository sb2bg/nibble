const std = @import("std");

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
};

/// Parse command-line arguments without retaining or allocating any memory.
/// Breakpoint addresses are hexadecimal, with or without a `0x` prefix.
pub fn parse(args: []const []const u8) ParseError!Options {
    var options: Options = .{};
    var index: usize = 0;
    var options_enabled = true;

    while (index < args.len) : (index += 1) {
        const arg = args[index];

        if (options_enabled and std.mem.eql(u8, arg, "--")) {
            options_enabled = false;
            continue;
        }
        if (options_enabled and (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help"))) {
            options.help = true;
            continue;
        }
        if (options_enabled and (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--debug"))) {
            options.debug = true;
            continue;
        }
        if (options_enabled and std.mem.eql(u8, arg, "--headless")) {
            options.headless = true;
            continue;
        }
        if (options_enabled and std.mem.eql(u8, arg, "--mooneye-test")) {
            options.mooneye_test = true;
            continue;
        }
        if (options_enabled and (std.mem.eql(u8, arg, "-s") or std.mem.eql(u8, arg, "--steps"))) {
            index += 1;
            if (index >= args.len) return error.MissingOptionValue;
            options.max_steps = std.fmt.parseInt(usize, args[index], 10) catch return error.InvalidStepCount;
            continue;
        }
        if (options_enabled and std.mem.startsWith(u8, arg, "--steps=")) {
            options.max_steps = std.fmt.parseInt(usize, arg["--steps=".len..], 10) catch return error.InvalidStepCount;
            continue;
        }
        if (options_enabled and (std.mem.eql(u8, arg, "-b") or std.mem.eql(u8, arg, "--breakpoint"))) {
            index += 1;
            if (index >= args.len) return error.MissingOptionValue;
            options.breakpoint = parseAddress(args[index]) catch return error.InvalidBreakpoint;
            continue;
        }
        if (options_enabled and std.mem.startsWith(u8, arg, "--breakpoint=")) {
            options.breakpoint = parseAddress(arg["--breakpoint=".len..]) catch return error.InvalidBreakpoint;
            continue;
        }
        if (options_enabled and std.mem.startsWith(u8, arg, "-")) return error.UnknownOption;

        if (options.rom_path != null) return error.TooManyRomPaths;
        options.rom_path = arg;
    }

    return options;
}

fn parseAddress(text: []const u8) !u16 {
    const digits = if (std.mem.startsWith(u8, text, "0x") or std.mem.startsWith(u8, text, "0X"))
        text[2..]
    else
        text;
    if (digits.len == 0) return error.InvalidCharacter;
    return std.fmt.parseInt(u16, digits, 16);
}

test "parse command-line options" {
    const options = try parse(&.{ "-d", "--steps=250", "-b", "0x0150", "--headless", "--mooneye-test", "game.gb" });
    try std.testing.expect(options.debug);
    try std.testing.expect(options.headless);
    try std.testing.expect(options.mooneye_test);
    try std.testing.expectEqual(@as(?usize, 250), options.max_steps);
    try std.testing.expectEqual(@as(?u16, 0x0150), options.breakpoint);
    try std.testing.expectEqualStrings("game.gb", options.rom_path.?);
}

test "double dash permits a ROM path beginning with a dash" {
    const options = try parse(&.{ "--", "-test.gb" });
    try std.testing.expectEqualStrings("-test.gb", options.rom_path.?);
}

test "invalid options and values are reported" {
    try std.testing.expectError(error.UnknownOption, parse(&.{"--wat"}));
    try std.testing.expectError(error.MissingOptionValue, parse(&.{"--steps"}));
    try std.testing.expectError(error.InvalidStepCount, parse(&.{ "--steps", "many" }));
    try std.testing.expectError(error.InvalidBreakpoint, parse(&.{ "--breakpoint", "xyz" }));
    try std.testing.expectError(error.TooManyRomPaths, parse(&.{ "one.gb", "two.gb" }));
}
