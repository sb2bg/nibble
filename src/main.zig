const std = @import("std");
const clap = @import("clap");
const Emulator = @import("emulator.zig").Emulator;
const EmulatorOptions = @import("emulator.zig").EmulatorOptions;

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-d, --debug            Enable debug output during execution.
    \\-s, --steps <usize>    Maximum number of steps to execute (default: 100).
    \\-b, --breakpoint <u16> Set a breakpoint at the specified address (hex).
    \\<str>                  ROM file to load.
    \\
);

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Parse command-line arguments
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        // Report useful error and exit.
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    // Handle help
    if (res.args.help != 0) {
        const stdout = std.io.getStdOut().writer();
        try stdout.print(
            \\Usage: nibble [OPTIONS] <ROM_FILE>
            \\
            \\A Game Boy emulator written in Zig.
            \\
            \\Options:
            \\  -h, --help              Display this help and exit
            \\  -d, --debug             Enable debug output during execution
            \\  -s, --steps <COUNT>     Maximum number of steps to execute
            \\  -b, --breakpoint <ADDR> Set a breakpoint at the specified address (hex)
            \\
            \\Example:
            \\  nibble roms/cpu_instrs/cpu_instrs.gb
            \\  nibble -d -s 1000 roms/cpu_instrs/cpu_instrs.gb
            \\
        , .{});
        return;
    }

    // Get ROM path from positional argument
    const rom_path = if (res.positionals.len > 0) res.positionals[0] else {
        std.debug.print("Error: ROM file path required\n", .{});
        std.debug.print("Usage: nibble [OPTIONS] <ROM_FILE>\n", .{});
        std.debug.print("Try 'nibble --help' for more information.\n", .{});
        return;
    };

    // Parse options
    const max_steps: ?usize = if (res.args.steps) |s|
        std.fmt.parseInt(usize, s, 10) catch {
            std.debug.print("Error: Invalid steps value '{s}'\n", .{s});
            return;
        }
    else
        null;

    const breakpoint: ?u16 = if (res.args.breakpoint) |b|
        std.fmt.parseInt(u16, b, 16) catch {
            std.debug.print("Error: Invalid breakpoint address '{s}' (expected hex)\n", .{b});
            return;
        }
    else
        null;

    const options = EmulatorOptions{
        .debug = res.args.debug != 0,
        .max_steps = max_steps orelse 100, // Default to 100 for now
        .breakpoint = breakpoint,
    };

    // Initialize and run the emulator
    var emu = Emulator.init(allocator, rom_path, options) catch |err| {
        std.debug.print("Error initializing emulator with ROM '{s}': {any}\n", .{ rom_path, err });
        return;
    };
    defer emu.deinit();

    emu.run();
}
