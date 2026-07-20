const std = @import("std");
const cli = @import("cli.zig");
const Emulator = @import("emulator.zig").Emulator;
const EmulatorOptions = @import("emulator.zig").EmulatorOptions;

const default_mooneye_steps = 10_000_000;

const help_text =
    \\Usage: nibble [OPTIONS] <ROM_FILE>
    \\
    \\A Game Boy emulator written in Zig.
    \\
    \\Options:
    \\  -h, --help              Display this help and exit
    \\  -d, --debug             Enable instruction tracing
    \\  -s, --steps <COUNT>     Stop after COUNT instructions
    \\  -b, --breakpoint <HEX>  Stop before executing address HEX
    \\  --headless              Run without graphics
    \\  --mooneye-test          Run a Mooneye ROM headlessly and report its result
    \\
    \\Controls:
    \\  D-pad   : Arrow keys
    \\  A       : X or A
    \\  B       : Z or S
    \\  Start   : Enter, keypad Enter, or Space
    \\  Select  : Backspace or Tab
    \\
    \\Management:
    \\  P       : Pause/resume emulation
    \\  R       : Reset emulator
    \\  F5/F9   : Save/load state for active slot
    \\  [ / ]   : Previous/next save slot
    \\  C       : Cycle display palette
    \\  M       : Mute/unmute audio
    \\  F1      : Open/hide detached debugger
    \\  F10     : Step one instruction while paused
    \\  F11     : Toggle fullscreen
    \\  Esc     : Quit
    \\
    \\Examples:
    \\  nibble roms/blargg/cpu_instrs/cpu_instrs.gb
    \\  nibble -d -s 1000 roms/blargg/cpu_instrs/cpu_instrs.gb
    \\  nibble --headless -s 10000000 roms/blargg/cpu_instrs/cpu_instrs.gb
    \\  nibble --mooneye-test acceptance/timer/div_write.gb
    \\
;

pub fn main(init: std.process.Init) !void {
    const argv = try init.minimal.args.toSlice(init.arena.allocator());
    const parsed = cli.parse(init.gpa, argv[1..]) catch |err| {
        std.process.fatal("invalid arguments ({s}); try 'nibble --help'", .{@errorName(err)});
    };

    if (parsed.help) {
        var stdout_buffer: [4096]u8 = undefined;
        var stdout_file_writer: std.Io.File.Writer = .init(.stdout(), init.io, &stdout_buffer);
        const stdout = &stdout_file_writer.interface;
        try stdout.writeAll(help_text);
        try stdout.flush();
        return;
    }

    const rom_path = parsed.rom_path orelse
        std.process.fatal("ROM file path required; try 'nibble --help'", .{});

    const options: EmulatorOptions = .{
        .debug = parsed.debug,
        .max_steps = parsed.max_steps orelse if (parsed.mooneye_test) default_mooneye_steps else null,
        .breakpoint = parsed.breakpoint,
        .headless = parsed.headless or parsed.mooneye_test,
        .mooneye_test = parsed.mooneye_test,
    };

    var emu = Emulator.init(init.gpa, init.io, rom_path, options) catch |err| {
        std.process.fatal("unable to initialize ROM '{s}': {s}", .{ rom_path, @errorName(err) });
    };
    defer emu.deinit();

    emu.run();
    emu.printSerialOutput();
    emu.printCartRamTestOutput();

    if (parsed.mooneye_test) {
        switch (emu.mooneyeResult() orelse {
            std.debug.print("Mooneye: NO RESULT after {d} steps\n", .{emu.machine.steps});
            std.process.exit(2);
        }) {
            .passed => std.debug.print("Mooneye: PASS ({d} steps)\n", .{emu.machine.steps}),
            .failed => {
                std.debug.print("Mooneye: FAIL ({d} steps)\n", .{emu.machine.steps});
                std.process.exit(1);
            },
        }
    }

    if (!options.debug) {
        std.debug.print("\n=== Execution Complete ===\n", .{});
        std.debug.print("Total steps: {d}\n", .{emu.machine.steps});
        std.debug.print("Total cycles: {d}\n", .{emu.machine.cpu.cycles});
    }
}
