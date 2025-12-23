const std = @import("std");
const Allocator = std.mem.Allocator;
const Cpu = @import("cpu/cpu.zig").Cpu;
const Bus = @import("memory/bus.zig").Bus;
const Cartridge = @import("cartridge/cartridge.zig").Cartridge;
const Timer = @import("timer.zig").Timer;

pub const EmulatorOptions = struct {
    debug: bool = false,
    max_steps: ?usize = null, // null means run indefinitely
    breakpoint: ?u16 = null,
};

pub const Emulator = struct {
    allocator: Allocator,
    cpu: Cpu,
    bus: Bus,
    timer: Timer,
    options: EmulatorOptions,

    // Runtime state
    steps: usize = 0,
    running: bool = false,

    /// Initialize the emulator with a ROM file
    pub fn init(allocator: Allocator, rom_path: []const u8, options: EmulatorOptions) !Emulator {
        // Load the cartridge
        var cartridge = try Cartridge.load(allocator, rom_path);
        errdefer cartridge.deinit();

        return Emulator{
            .allocator = allocator,
            .cpu = Cpu.init(),
            .bus = Bus.init(allocator, cartridge),
            .timer = Timer.init(),
            .options = options,
            .steps = 0,
            .running = false,
        };
    }

    pub fn deinit(self: *Emulator) void {
        self.bus.deinit();
    }

    /// Run the emulator's main loop
    pub fn run(self: *Emulator) void {
        self.running = true;

        if (self.options.debug) {
            self.bus.cartridge.printInfo();
            std.debug.print("\n=== Starting Execution ===\n\n", .{});
            std.debug.print("Initial state:\n", .{});
            self.printCpuState();
        }

        while (self.running) {
            self.step();

            // Check max steps limit
            if (self.options.max_steps) |max| {
                if (self.steps >= max) {
                    if (self.options.debug) {
                        std.debug.print("\nReached max steps limit ({d})\n", .{max});
                    }
                    break;
                }
            }

            // Check breakpoint
            if (self.options.breakpoint) |bp| {
                if (self.cpu.pc == bp) {
                    if (self.options.debug) {
                        std.debug.print("\nBreakpoint hit at 0x{X:0>4}\n", .{bp});
                    }
                    break;
                }
            }
        }
    }

    /// Execute a single CPU step and tick other components
    pub fn step(self: *Emulator) void {
        const pc_before = self.cpu.pc;

        // Execute CPU instruction (returns cycles used)
        const cycles = self.cpu.step(&self.bus);

        // Tick timer with T-cycles (4 T-cycles per M-cycle for most instructions)
        self.timer.tick(cycles, &self.bus.io);

        self.steps += 1;

        if (self.options.debug) {
            std.debug.print("\nStep {d} (PC=0x{X:0>4}, cycles={d}):\n", .{ self.steps, pc_before, cycles });
            self.printCpuState();
        }
    }

    /// Stop the emulator
    pub fn stop(self: *Emulator) void {
        self.running = false;
    }

    /// Reset the emulator (keeps ROM loaded)
    pub fn reset(self: *Emulator) void {
        self.cpu.reset();
        self.bus.reset();
        self.timer.reset();
        self.steps = 0;
        self.running = false;
    }

    fn printCpuState(self: *const Emulator) void {
        const cpu = &self.cpu;
        std.debug.print("A: 0x{X:0>2} F: 0x{X:0>2} B: 0x{X:0>2} C: 0x{X:0>2} D: 0x{X:0>2} E: 0x{X:0>2} H: 0x{X:0>2} L: 0x{X:0>2}\n", .{
            cpu.a(),
            cpu.f().toU8(),
            cpu.b(),
            cpu.c(),
            cpu.d(),
            cpu.e(),
            cpu.h(),
            cpu.l(),
        });
        std.debug.print("SP: 0x{X:0>4} PC: 0x{X:0>4} IME: {s} Cycles: {d}\n", .{
            cpu.sp,
            cpu.pc,
            if (cpu.ime) "ON" else "OFF",
            cpu.cycles,
        });
    }

    /// Get cartridge info for display
    pub fn getCartridgeInfo(self: *const Emulator) *const Cartridge {
        return &self.bus.cartridge;
    }

    /// Print serial output (useful for test ROMs)
    pub fn printSerialOutput(self: *const Emulator) void {
        const output = self.bus.getSerialOutput();
        if (output.len > 0) {
            std.debug.print("\n=== Serial Output ({d} bytes) ===\n", .{output.len});
            for (output) |byte| {
                if (byte >= 0x20 and byte < 0x7F or byte == '\n' or byte == '\r') {
                    std.debug.print("{c}", .{byte});
                } else {
                    std.debug.print("[{X:0>2}]", .{byte});
                }
            }
            std.debug.print("\n", .{});
        }
    }
};
