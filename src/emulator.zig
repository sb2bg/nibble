const std = @import("std");
const Allocator = std.mem.Allocator;
const Cpu = @import("cpu.zig").Cpu;
const Bus = @import("bus.zig").Bus;
const Rom = @import("rom.zig").Rom;

pub const EmulatorOptions = struct {
    debug: bool = false,
    max_steps: ?usize = null, // null means run indefinitely
    breakpoint: ?u16 = null,
};

pub const Emulator = struct {
    allocator: Allocator,
    cpu: Cpu,
    bus: Bus,
    rom: Rom,
    options: EmulatorOptions,

    // Runtime state
    steps: usize = 0,
    running: bool = false,

    pub fn init(allocator: Allocator, rom_path: []const u8, options: EmulatorOptions) !Emulator {
        // Load the ROM
        var rom = try Rom.load(allocator, rom_path);
        errdefer rom.deinit();

        // Create bus with the loaded ROM
        var emu = Emulator{
            .allocator = allocator,
            .cpu = Cpu.init(),
            .bus = undefined, // Will be set below
            .rom = rom,
            .options = options,
            .steps = 0,
            .running = false,
        };

        // Initialize bus with pointer to the rom field
        emu.bus = Bus.init(&emu.rom);

        return emu;
    }

    pub fn deinit(self: *Emulator) void {
        self.rom.deinit();
    }

    /// Run the emulator's main loop
    pub fn run(self: *Emulator) void {
        self.running = true;

        if (self.options.debug) {
            self.rom.printInfo();
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

    /// Execute a single CPU step
    pub fn step(self: *Emulator) void {
        const pc_before = self.cpu.pc;
        self.cpu.step(&self.bus);
        self.steps += 1;

        if (self.options.debug) {
            std.debug.print("\nStep {d} (PC=0x{X:0>4}):\n", .{ self.steps, pc_before });
            self.printCpuState();
        }
    }

    /// Stop the emulator
    pub fn stop(self: *Emulator) void {
        self.running = false;
    }

    /// Reset the emulator (keeps ROM loaded)
    pub fn reset(self: *Emulator) void {
        self.cpu = Cpu.init();
        self.steps = 0;
        self.running = false;

        // Reset bus memory
        self.bus = Bus.init(&self.rom);

        // Reset ROM MBC state
        self.rom.rom_bank = 1;
        self.rom.ram_bank = 0;
        self.rom.ram_enabled = false;
        self.rom.banking_mode = 0;
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
        std.debug.print("SP: 0x{X:0>4} PC: 0x{X:0>4}\n", .{ cpu.sp, cpu.pc });
    }

    /// Get ROM info for display
    pub fn getRomInfo(self: *const Emulator) *const Rom {
        return &self.rom;
    }
};
