const std = @import("std");
const clap = @import("clap");
const Emulator = @import("emulator.zig").Emulator;
const EmulatorOptions = @import("emulator.zig").EmulatorOptions;

const params = clap.parseParamsComptime(
    \\-h, --help             Display this help and exit.
    \\-d, --debug            Enable debug output during execution.
    \\-s, --steps <usize>    Maximum number of steps to execute (default: unlimited).
    \\-b, --breakpoint <u16> Set a breakpoint at the specified address (hex).
    \\--headless             Run without graphics (for testing).
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
        var stderr_buf: [1024]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
        diag.report(&stderr_writer.interface, err) catch {};
        stderr_writer.interface.flush() catch {};
        return err;
    };
    defer res.deinit();

    // Handle help
    if (res.args.help != 0) {
        var buf: [4096]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&buf);
        try stdout_writer.interface.print(
            \\Usage: nibble [OPTIONS] <ROM_FILE>
            \\
            \\A Game Boy emulator written in Zig.
            \\
            \\Options:
            \\  -h, --help              Display this help and exit
            \\  -d, --debug             Enable debug output during execution
            \\  -s, --steps <COUNT>     Maximum number of steps to execute
            \\  -b, --breakpoint <ADDR> Set a breakpoint at the specified address (hex)
            \\  --headless              Run without graphics (for testing)
            \\
            \\Example:
            \\  nibble roms/cpu_instrs/cpu_instrs.gb
            \\  nibble -d -s 1000 roms/cpu_instrs/cpu_instrs.gb
            \\  nibble --headless -s 10000000 roms/cpu_instrs/cpu_instrs.gb
            \\
        , .{});
        try stdout_writer.interface.flush();
        return;
    }

    // Get ROM path from positional argument
    const rom_path = if (res.positionals.len > 0) res.positionals[0].? else {
        std.debug.print("Error: ROM file path required\n", .{});
        std.debug.print("Usage: nibble [OPTIONS] <ROM_FILE>\n", .{});
        std.debug.print("Try 'nibble --help' for more information.\n", .{});
        return;
    };

    // Parse options
    const max_steps: ?usize = res.args.steps;

    const breakpoint: ?u16 = res.args.breakpoint;

    const options = EmulatorOptions{
        .debug = res.args.debug != 0,
        .max_steps = max_steps orelse null,
        .breakpoint = breakpoint,
        .headless = res.args.headless != 0,
    };

    // Initialize and run the emulator
    var emu = Emulator.init(allocator, rom_path, options) catch |err| {
        std.debug.print("Error initializing emulator with ROM '{s}': {any}\n", .{ rom_path, err });
        return;
    };
    defer emu.deinit();

    emu.run();

    // Print serial output summary (useful for test ROMs)
    if (!options.debug) {
        std.debug.print("\n=== Execution Complete ===\n", .{});
        std.debug.print("Total steps: {d}\n", .{emu.steps});
        std.debug.print("Total cycles: {d}\n", .{emu.cpu.cycles});
    }
}
