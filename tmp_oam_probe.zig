const std = @import("std");
const Emulator = @import("src/emulator.zig").Emulator;
const EmulatorOptions = @import("src/emulator.zig").EmulatorOptions;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = std.process.args();
    _ = args.next();
    const rom = args.next() orelse return error.MissingRom;
    const steps_arg = args.next() orelse "20000000";
    const steps = try std.fmt.parseInt(usize, steps_arg, 10);

    const opts = EmulatorOptions{ .headless = true, .max_steps = steps };
    var emu = try Emulator.init(allocator, rom, opts);
    defer emu.deinit();

    emu.run();

    std.debug.print("steps={d} cycles={d}\n", .{ emu.steps, emu.cpu.cycles });

    const ram = emu.bus.cartridge.ram_data orelse {
        std.debug.print("no ram\n", .{});
        return;
    };

    std.debug.print("A000={X:0>2} sig={X:0>2} {X:0>2} {X:0>2}\n", .{
        ram[0], ram[1], ram[2], ram[3],
    });

    var i: usize = 4;
    std.debug.print("text=", .{});
    while (i < ram.len and ram[i] != 0 and i < 1024) : (i += 1) {
        const b = ram[i];
        if (b >= 0x20 and b < 0x7F or b == '\n' or b == '\r') {
            std.debug.print("{c}", .{b});
        } else {
            std.debug.print("[{X:0>2}]", .{b});
        }
    }
    std.debug.print("\n", .{});
}
