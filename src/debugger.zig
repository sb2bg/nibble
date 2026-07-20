const std = @import("std");
const machine_mod = @import("machine.zig");
const Machine = machine_mod.Machine;
const Instruction = @import("cpu/instructions.zig").Instruction;

pub const MAX_BREAKPOINTS = 64;
pub const MAX_WATCHPOINTS = 64;
pub const EVENT_CAPACITY = 1024;

pub const EventKind = enum {
    instruction,
    breakpoint,
    watchpoint,
    mapper_bank,
    frame,
};

/// Compact debugger record. `subject` is an address for watchpoints, an opcode
/// for instructions, and the upper ROM bank for mapper transitions.
pub const Event = struct {
    kind: EventKind,
    instruction: usize,
    cycle: u64,
    pc: u16,
    subject: u16 = 0,
    before: u16 = 0,
    after: u16 = 0,
};

pub const StopReason = enum {
    breakpoint,
    watchpoint,
    instruction_limit,
};

pub const RunResult = struct {
    reason: StopReason,
    instructions: usize,
    pc: u16,
};

pub const DecodedInstruction = struct {
    address: u16,
    bytes: [3]u8,
    length: u2,
    mnemonic: []const u8,
};

const Watchpoint = struct {
    address: u16,
    value: u8,
};

/// Opt-in research debugger. It deliberately wraps `Machine.step` instead of
/// installing callbacks in the core hot path, so an unused debugger has zero
/// execution cost. Watchpoints report value transitions at instruction
/// boundaries; they are not bus-access watchpoints.
pub const Debugger = struct {
    breakpoints: [MAX_BREAKPOINTS]u16 = undefined,
    breakpoint_count: usize = 0,
    watchpoints: [MAX_WATCHPOINTS]Watchpoint = undefined,
    watchpoint_count: usize = 0,
    event_ring: [EVENT_CAPACITY]Event = undefined,
    event_start: usize = 0,
    event_count: usize = 0,
    trace_instructions: bool = false,

    pub fn addBreakpoint(self: *Debugger, address: u16) error{BreakpointCapacity}!void {
        if (self.hasBreakpoint(address)) return;
        if (self.breakpoint_count == self.breakpoints.len) return error.BreakpointCapacity;
        self.breakpoints[self.breakpoint_count] = address;
        self.breakpoint_count += 1;
    }

    pub fn removeBreakpoint(self: *Debugger, address: u16) bool {
        for (self.breakpoints[0..self.breakpoint_count], 0..) |candidate, index| {
            if (candidate != address) continue;
            self.breakpoints[index] = self.breakpoints[self.breakpoint_count - 1];
            self.breakpoint_count -= 1;
            return true;
        }
        return false;
    }

    pub fn hasBreakpoint(self: *const Debugger, address: u16) bool {
        for (self.breakpoints[0..self.breakpoint_count]) |candidate| {
            if (candidate == address) return true;
        }
        return false;
    }

    pub fn addWatchpoint(self: *Debugger, machine: *const Machine, address: u16) error{WatchpointCapacity}!void {
        for (self.watchpoints[0..self.watchpoint_count]) |watchpoint| {
            if (watchpoint.address == address) return;
        }
        if (self.watchpoint_count == self.watchpoints.len) return error.WatchpointCapacity;
        self.watchpoints[self.watchpoint_count] = .{
            .address = address,
            .value = machine.peek(address),
        };
        self.watchpoint_count += 1;
    }

    pub fn removeWatchpoint(self: *Debugger, address: u16) bool {
        for (self.watchpoints[0..self.watchpoint_count], 0..) |watchpoint, index| {
            if (watchpoint.address != address) continue;
            self.watchpoints[index] = self.watchpoints[self.watchpoint_count - 1];
            self.watchpoint_count -= 1;
            return true;
        }
        return false;
    }

    pub fn clearEvents(self: *Debugger) void {
        self.event_start = 0;
        self.event_count = 0;
    }

    pub fn eventCount(self: *const Debugger) usize {
        return self.event_count;
    }

    /// Return events in oldest-to-newest order without exposing ring layout.
    pub fn eventAt(self: *const Debugger, index: usize) ?Event {
        if (index >= self.event_count) return null;
        return self.event_ring[(self.event_start + index) % self.event_ring.len];
    }

    /// Execute exactly one instruction, ignoring a breakpoint at the current
    /// PC. Returns a watchpoint stop when an observed value changes.
    pub fn step(self: *Debugger, machine: *Machine) ?StopReason {
        const pc = machine.cpu.pc;
        const opcode = machine.peek(pc);
        const mapper_before = machine.inspectCartridge().mapper.upper_rom_bank;
        const result = machine.step();

        if (self.trace_instructions) {
            self.record(.{
                .kind = .instruction,
                .instruction = machine.steps,
                .cycle = machine.cpu.cycles,
                .pc = pc,
                .subject = opcode,
            });
        }

        var stopped = false;
        for (self.watchpoints[0..self.watchpoint_count]) |*watchpoint| {
            const next = machine.peek(watchpoint.address);
            if (next == watchpoint.value) continue;
            self.record(.{
                .kind = .watchpoint,
                .instruction = machine.steps,
                .cycle = machine.cpu.cycles,
                .pc = machine.cpu.pc,
                .subject = watchpoint.address,
                .before = watchpoint.value,
                .after = next,
            });
            watchpoint.value = next;
            stopped = true;
        }

        const mapper_after = machine.inspectCartridge().mapper.upper_rom_bank;
        if (mapper_before != mapper_after) {
            self.record(.{
                .kind = .mapper_bank,
                .instruction = machine.steps,
                .cycle = machine.cpu.cycles,
                .pc = machine.cpu.pc,
                .subject = mapper_after,
                .before = mapper_before,
                .after = mapper_after,
            });
        }
        if (result.frame_ready) {
            self.record(.{
                .kind = .frame,
                .instruction = machine.steps,
                .cycle = machine.cpu.cycles,
                .pc = machine.cpu.pc,
            });
        }
        return if (stopped) .watchpoint else null;
    }

    pub fn run(self: *Debugger, machine: *Machine, max_instructions: usize) RunResult {
        var executed: usize = 0;
        while (executed < max_instructions) {
            if (self.hasBreakpoint(machine.cpu.pc)) {
                self.record(.{
                    .kind = .breakpoint,
                    .instruction = machine.steps,
                    .cycle = machine.cpu.cycles,
                    .pc = machine.cpu.pc,
                });
                return .{ .reason = .breakpoint, .instructions = executed, .pc = machine.cpu.pc };
            }
            if (self.step(machine)) |reason| {
                return .{ .reason = reason, .instructions = executed + 1, .pc = machine.cpu.pc };
            }
            executed += 1;
        }
        return .{ .reason = .instruction_limit, .instructions = executed, .pc = machine.cpu.pc };
    }

    pub fn disassembleAt(machine: *const Machine, address: u16) DecodedInstruction {
        const Reader = struct {
            machine: *const Machine,
            start: u16,
            offset: u2 = 0,
            bytes: [3]u8 = .{ 0, 0, 0 },

            fn read(ptr: *anyopaque) u8 {
                const self: *@This() = @ptrCast(@alignCast(ptr));
                const value = self.machine.peek(self.start +% self.offset);
                if (self.offset < self.bytes.len) self.bytes[self.offset] = value;
                self.offset += 1;
                return value;
            }
        };

        var reader = Reader{ .machine = machine, .start = address };
        const instruction = Instruction.decode(.{
            .context = @ptrCast(&reader),
            .readFn = Reader.read,
        });
        return .{
            .address = address,
            .bytes = reader.bytes,
            .length = reader.offset,
            .mnemonic = @tagName(std.meta.activeTag(instruction)),
        };
    }

    fn record(self: *Debugger, event: Event) void {
        if (self.event_count < self.event_ring.len) {
            const index = (self.event_start + self.event_count) % self.event_ring.len;
            self.event_ring[index] = event;
            self.event_count += 1;
            return;
        }
        self.event_ring[self.event_start] = event;
        self.event_start = (self.event_start + 1) % self.event_ring.len;
    }
};

test "debugger stops before breakpoints and disassembles without stepping" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();
    var debugger: Debugger = .{};
    try debugger.addBreakpoint(0x0102);

    const decoded = Debugger.disassembleAt(&machine, 0x0100);
    try std.testing.expectEqualStrings("nop", decoded.mnemonic);
    try std.testing.expectEqual(@as(u2, 1), decoded.length);
    try std.testing.expectEqual(@as(usize, 0), machine.steps);

    const result = debugger.run(&machine, 10);
    try std.testing.expectEqual(StopReason.breakpoint, result.reason);
    try std.testing.expectEqual(@as(usize, 2), result.instructions);
    try std.testing.expectEqual(@as(u16, 0x0102), machine.cpu.pc);
    try std.testing.expectEqual(EventKind.breakpoint, debugger.eventAt(0).?.kind);
}

test "debugger value watchpoints report instruction-boundary changes" {
    var rom = [_]u8{0} ** 0x8000;
    rom[0x0100] = 0x3E; // LD A, $42
    rom[0x0101] = 0x42;
    rom[0x0102] = 0xEA; // LD ($C000), A
    rom[0x0103] = 0x00;
    rom[0x0104] = 0xC0;
    var machine = Machine.init(
        std.testing.allocator,
        try @import("cartridge/cartridge.zig").Cartridge.fromRom(std.testing.allocator, &rom),
        .{},
    );
    defer machine.deinit();
    var debugger: Debugger = .{};
    try debugger.addWatchpoint(&machine, 0xC000);

    const result = debugger.run(&machine, 10);
    try std.testing.expectEqual(StopReason.watchpoint, result.reason);
    try std.testing.expectEqual(@as(usize, 2), result.instructions);
    const event = debugger.eventAt(0).?;
    try std.testing.expectEqual(EventKind.watchpoint, event.kind);
    try std.testing.expectEqual(@as(u16, 0xC000), event.subject);
    try std.testing.expectEqual(@as(u16, 0x42), event.after);
}
