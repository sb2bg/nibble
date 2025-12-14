const std = @import("std");
const cpu = @import("cpu.zig");

pub fn main() void {
    const gb_cpu = cpu.Cpu.init();
    std.debug.print("CPU initialized. PC: {x}, SP: {x}\n", .{ gb_cpu.pc, gb_cpu.sp });
}
