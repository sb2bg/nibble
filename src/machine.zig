const std = @import("std");
const Allocator = std.mem.Allocator;
const Cpu = @import("cpu/cpu.zig").Cpu;
const bus_mod = @import("memory/bus.zig");
const Bus = bus_mod.Bus;
const Dma = bus_mod.Dma;
const Cartridge = @import("cartridge/cartridge.zig").Cartridge;
const Timer = @import("timer.zig").Timer;
const Serial = @import("serial.zig").Serial;
const apu_mod = @import("apu.zig");
const Apu = apu_mod.Apu;
const Mbc = @import("memory/mbc.zig").Mbc;
const ppu_mod = @import("ppu/ppu.zig");
const Ppu = ppu_mod.Ppu;

const MAX_CART_RAM_BYTES = 128 * 1024;

pub const MachineOptions = struct {
    /// PCM is host output rather than hardware-visible state. Disabling its
    /// capture avoids mixing samples in simulations that only observe memory
    /// or video, while all APU registers and channel generators still tick.
    capture_audio: bool = false,

    /// Framebuffer pixels are host-observable output. Disabling their capture
    /// retains the complete PPU fetch/FIFO/timing path and only omits final
    /// palette-mapped framebuffer stores.
    capture_video: bool = true,
};

pub const MooneyeResult = enum {
    passed,
    failed,
};

pub const StepResult = struct {
    cycles: u8,
    frame_ready: bool,
};

pub const VideoObservation = enum {
    /// Do not update the framebuffer during this run.
    none,
    /// Capture only the last requested frame. Useful for frame skipping and AI.
    final_frame,
    /// Capture every completed frame for callers that inspect between steps.
    every_frame,
};

pub const FrameStepOptions = struct {
    video: VideoObservation = .final_frame,
    /// Null keeps the machine's configured PCM policy for this run.
    capture_audio: ?bool = null,
    /// Prevent an LCD-disabled or crashed ROM from blocking the caller.
    max_instructions_per_frame: usize = 1_000_000,
};

pub const FrameStepResult = struct {
    frames_completed: usize,
    instructions: usize,
    cycles: u64,
    timed_out: bool,
};

/// Input applied immediately before a requested frame begins. Events must be
/// sorted by `frame_offset`; duplicate offsets are allowed and apply in order.
pub const FrameInput = struct {
    frame_offset: usize,
    buttons: Buttons,
};

pub const CpuObservation = struct {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,
    ime: bool,
    halted: bool,
    cycles: u64,
};

/// Borrowed, allocation-free view of state commonly consumed by agents. The
/// slices remain valid until the machine is stepped, restored, or destroyed.
pub const Observation = struct {
    cpu: CpuObservation,
    instructions: usize,
    frames: usize,
    wram: []const u8,
    hram: []const u8,
    vram: []const u8,
    oam: []const u8,
    background_tilemap: []const u8,
    window_tilemap: []const u8,
    frame_buffer: *const [ppu_mod.SCREEN_HEIGHT][ppu_mod.SCREEN_WIDTH]ppu_mod.DmgColor,
};

pub const Buttons = struct {
    right: bool = false,
    left: bool = false,
    up: bool = false,
    down: bool = false,
    a: bool = false,
    b: bool = false,
    select: bool = false,
    start: bool = false,
};

const CpuState = struct {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,
    ime: bool,
    ime_enable_delay: u2,
    halted: bool,
    halt_bug: bool,
    cycles: u64,
};

const IoState = struct {
    data: [0x80]u8,
    joypad_select: u8,
    joypad_buttons: u8,
    oam_scan_row: u8,
    ppu_oam_read_blocked: bool,
    ppu_oam_write_blocked: bool,
    ppu_vram_read_blocked: bool,
    ppu_vram_write_blocked: bool,
    stat_irq_line: bool,
    stat_mode0_suppressed: bool,
    stat_read_early_hblank: bool,
    late_interrupts: u8,
};

const BusState = struct {
    wram: [0x2000]u8,
    hram: [0x7F]u8,
    oam: [0xA0]u8,
    vram: [0x2000]u8,
    io: IoState,
    ie_register: u8,
    timer: Timer,
    serial: Serial,
    apu: Apu,
    dma: Dma,
    mbc: Mbc.Snapshot,
    cart_ram_len: usize,
    cart_ram: [MAX_CART_RAM_BYTES]u8,
};

/// A complete mutable machine snapshot. ROM bytes and allocation ownership are
/// deliberately excluded, so restoring never invalidates host-owned storage.
pub const Snapshot = struct {
    cpu: CpuState,
    bus: BusState,
    ppu: Ppu,
    steps: usize,
    frames: usize,
};

/// Frontend-free deterministic DMG simulation core.
///
/// `Machine` owns emulated hardware and a cartridge, but no host clock,
/// filesystem, window, input device, or audio device. Frontends drive it by
/// stepping instructions and consuming the resulting frame/audio edges.
pub const Machine = struct {
    cpu: Cpu,
    bus: Bus,
    ppu: Ppu,
    options: MachineOptions,
    steps: usize = 0,
    frames: usize = 0,

    pub fn init(allocator: Allocator, cartridge: Cartridge, options: MachineOptions) Machine {
        var machine: Machine = .{
            .cpu = Cpu.init(),
            .bus = Bus.init(allocator, cartridge),
            .ppu = Ppu.init(),
            .options = options,
        };
        machine.ppu.setPixelCapture(options.capture_video);
        return machine;
    }

    pub fn deinit(self: *Machine) void {
        self.bus.deinit();
    }

    /// Branch the complete mutable machine state while sharing immutable ROM
    /// bytes. The returned machine has independent RAM, mapper, IO, and output
    /// allocations and may be stepped or destroyed on another worker.
    pub fn fork(self: *const Machine, allocator: Allocator) !Machine {
        const cartridge = try self.bus.cartridge.cloneForMachine(allocator);
        var branch = Machine.init(allocator, cartridge, self.options);
        branch.restore(self.capture());
        return branch;
    }

    /// Execute one instruction and preserve each bus access at its exact
    /// emulated phase. The callback is an internal compatibility bridge; the
    /// public machine API itself has no host callbacks.
    pub fn step(self: *Machine) StepResult {
        var clocked_cycles: u16 = 0;
        var frame_ready = false;

        const HookContext = struct {
            machine: *Machine,
            clocked: *u16,
            frame_ready: *bool,

            fn tick(ptr: *anyopaque, cycles: u8) void {
                const ctx: *@This() = @ptrCast(@alignCast(ptr));
                ctx.machine.tickPeripherals(cycles, ctx.frame_ready);
                ctx.clocked.* +%= cycles;
            }
        };
        var hook_ctx = HookContext{
            .machine = self,
            .clocked = &clocked_cycles,
            .frame_ready = &frame_ready,
        };
        self.bus.setCycleHook(.{
            .context = @ptrCast(&hook_ctx),
            .tickFn = HookContext.tick,
        });
        defer self.bus.setCycleHook(null);

        const cycles = self.cpu.step(&self.bus);
        if (clocked_cycles < @as(u16, cycles)) {
            const remaining: u8 = @intCast(@as(u16, cycles) - clocked_cycles);
            self.tickPeripherals(remaining, &frame_ready);
        }

        self.steps += 1;
        return .{ .cycles = cycles, .frame_ready = frame_ready };
    }

    pub fn runInstructions(self: *Machine, count: usize) void {
        for (0..count) |_| _ = self.step();
    }

    /// Run until the next completed video frame, bounded so a ROM that turns
    /// the LCD off cannot trap an automation caller indefinitely.
    pub fn runUntilFrame(self: *Machine, max_instructions: usize) ?usize {
        const start = self.steps;
        while (self.steps - start < max_instructions) {
            if (self.step().frame_ready) return self.steps - start;
        }
        return null;
    }

    /// Advance a fixed number of video frames without allocating. Output
    /// capture can be reduced independently from hardware timing.
    pub fn stepFrames(
        self: *Machine,
        frame_count: usize,
        options: FrameStepOptions,
    ) FrameStepResult {
        return self.stepFramesWithInputs(frame_count, &.{}, options) catch unreachable;
    }

    /// Apply a caller-owned input timeline at deterministic frame boundaries
    /// while advancing the requested number of frames.
    pub fn stepFramesWithInputs(
        self: *Machine,
        frame_count: usize,
        inputs: []const FrameInput,
        options: FrameStepOptions,
    ) error{ InvalidInputFrame, UnsortedInputs }!FrameStepResult {
        var previous_offset: usize = 0;
        for (inputs, 0..) |input, index| {
            if (input.frame_offset >= frame_count) return error.InvalidInputFrame;
            if (index != 0 and input.frame_offset < previous_offset) return error.UnsortedInputs;
            previous_offset = input.frame_offset;
        }

        const start_steps = self.steps;
        const start_cycles = self.cpu.cycles;
        const original_video = self.ppu.isPixelCaptureEnabled();
        const original_audio = self.options.capture_audio;
        defer {
            self.ppu.setPixelCapture(original_video);
            self.options.capture_audio = original_audio;
        }
        if (options.capture_audio) |capture_enabled| self.options.capture_audio = capture_enabled;

        var completed: usize = 0;
        var input_index: usize = 0;
        while (completed < frame_count) {
            while (input_index < inputs.len and inputs[input_index].frame_offset == completed) : (input_index += 1) {
                self.setButtons(inputs[input_index].buttons);
            }

            const capture_frame = switch (options.video) {
                .none => false,
                .final_frame => completed + 1 == frame_count,
                .every_frame => true,
            };
            self.ppu.setPixelCapture(capture_frame);

            if (self.runUntilFrame(options.max_instructions_per_frame) == null) {
                return .{
                    .frames_completed = completed,
                    .instructions = self.steps - start_steps,
                    .cycles = self.cpu.cycles - start_cycles,
                    .timed_out = true,
                };
            }
            completed += 1;
        }

        return .{
            .frames_completed = completed,
            .instructions = self.steps - start_steps,
            .cycles = self.cpu.cycles - start_cycles,
            .timed_out = false,
        };
    }

    pub fn observe(self: *const Machine) Observation {
        const lcdc = self.bus.io.getLcdc();
        const background_offset: usize = if ((lcdc & 0x08) != 0) 0x1C00 else 0x1800;
        const window_offset: usize = if ((lcdc & 0x40) != 0) 0x1C00 else 0x1800;
        return .{
            .cpu = .{
                .af = self.cpu.af,
                .bc = self.cpu.bc,
                .de = self.cpu.de,
                .hl = self.cpu.hl,
                .sp = self.cpu.sp,
                .pc = self.cpu.pc,
                .ime = self.cpu.ime,
                .halted = self.cpu.halted,
                .cycles = self.cpu.cycles,
            },
            .instructions = self.steps,
            .frames = self.frames,
            .wram = &self.bus.wram,
            .hram = &self.bus.hram,
            .vram = &self.bus.vram,
            .oam = &self.bus.oam,
            .background_tilemap = self.bus.vram[background_offset..][0..0x400],
            .window_tilemap = self.bus.vram[window_offset..][0..0x400],
            .frame_buffer = &self.ppu.frame_buffer,
        };
    }

    pub fn setButtons(self: *Machine, buttons: Buttons) void {
        var active_low: u8 = 0xFF;
        if (buttons.right) active_low &= ~@as(u8, 0x01);
        if (buttons.left) active_low &= ~@as(u8, 0x02);
        if (buttons.up) active_low &= ~@as(u8, 0x04);
        if (buttons.down) active_low &= ~@as(u8, 0x08);
        if (buttons.a) active_low &= ~@as(u8, 0x10);
        if (buttons.b) active_low &= ~@as(u8, 0x20);
        if (buttons.select) active_low &= ~@as(u8, 0x40);
        if (buttons.start) active_low &= ~@as(u8, 0x80);
        self.bus.io.setJoypadState(active_low);
    }

    pub fn reset(self: *Machine) void {
        self.cpu.reset();
        self.bus.reset();
        self.ppu.reset();
        self.steps = 0;
        self.frames = 0;
    }

    pub fn capture(self: *const Machine) Snapshot {
        return .{
            .cpu = .{
                .af = self.cpu.af,
                .bc = self.cpu.bc,
                .de = self.cpu.de,
                .hl = self.cpu.hl,
                .sp = self.cpu.sp,
                .pc = self.cpu.pc,
                .ime = self.cpu.ime,
                .ime_enable_delay = self.cpu.ime_enable_delay,
                .halted = self.cpu.halted,
                .halt_bug = self.cpu.halt_bug,
                .cycles = self.cpu.cycles,
            },
            .bus = self.captureBusState(),
            .ppu = self.ppu,
            .steps = self.steps,
            .frames = self.frames,
        };
    }

    pub fn restore(self: *Machine, state: Snapshot) void {
        self.cpu.af = state.cpu.af;
        self.cpu.bc = state.cpu.bc;
        self.cpu.de = state.cpu.de;
        self.cpu.hl = state.cpu.hl;
        self.cpu.sp = state.cpu.sp;
        self.cpu.pc = state.cpu.pc;
        self.cpu.ime = state.cpu.ime;
        self.cpu.ime_enable_delay = state.cpu.ime_enable_delay;
        self.cpu.halted = state.cpu.halted;
        self.cpu.halt_bug = state.cpu.halt_bug;
        self.cpu.cycles = state.cpu.cycles;
        self.cpu.reader_ctx = undefined;

        self.applyBusState(state.bus);
        const capture_pixels = self.ppu.isPixelCaptureEnabled();
        self.ppu = state.ppu;
        self.ppu.setPixelCapture(capture_pixels);
        self.steps = state.steps;
        self.frames = state.frames;
    }

    pub fn inspectCartridge(self: *const Machine) Cartridge.Inspection {
        return self.bus.cartridge.inspect();
    }

    /// Stable digest of state commonly observed by automation clients. This is
    /// a regression/replay identity, not a cryptographic hash or save format.
    pub fn observableDigest(self: *const Machine) u64 {
        var hash = std.hash.Wyhash.init(0);
        hashInteger(&hash, self.cpu.af);
        hashInteger(&hash, self.cpu.bc);
        hashInteger(&hash, self.cpu.de);
        hashInteger(&hash, self.cpu.hl);
        hashInteger(&hash, self.cpu.sp);
        hashInteger(&hash, self.cpu.pc);
        hashInteger(&hash, self.cpu.cycles);
        hashInteger(&hash, self.steps);
        hashInteger(&hash, self.frames);
        hashInteger(&hash, self.bus.timer.system_counter);
        hash.update(&self.bus.wram);
        hash.update(&self.bus.hram);
        hash.update(&self.bus.vram);
        hash.update(&self.bus.oam);
        hash.update(&self.bus.io.data);
        hash.update(std.mem.asBytes(&self.ppu.frame_buffer));
        if (self.bus.cartridge.ram_data) |ram| hash.update(ram);

        const mapper = self.bus.cartridge.mbc.inspect();
        hashInteger(&hash, @intFromEnum(mapper.mbc_type));
        hashInteger(&hash, mapper.rom_bank_register);
        hashInteger(&hash, mapper.ram_bank_register);
        hashInteger(&hash, mapper.banking_mode);
        hashInteger(&hash, @intFromBool(mapper.ram_enabled));
        hashInteger(&hash, mapper.rtc.seconds);
        hashInteger(&hash, mapper.rtc.minutes);
        hashInteger(&hash, mapper.rtc.hours);
        hashInteger(&hash, mapper.rtc.day);
        return hash.final();
    }

    /// Observe the CPU-visible address space without advancing emulated time.
    pub fn peek(self: *const Machine, addr: u16) u8 {
        return self.bus.peek(addr);
    }

    pub fn pendingAudio(self: *const Machine) []const apu_mod.StereoSample {
        return self.bus.apu.pendingSamples();
    }

    pub fn discardAudio(self: *Machine) void {
        self.bus.apu.discardSamples();
    }

    pub fn mooneyeResult(self: *const Machine) ?MooneyeResult {
        const pc = self.cpu.pc;
        if (pc > std.math.maxInt(u16) - 2) return null;
        if (self.peek(pc) != 0x40 or
            self.peek(pc + 1) != 0x18 or
            self.peek(pc + 2) != 0xFE)
        {
            return null;
        }

        return classifyMooneyeRegisters(.{
            self.cpu.b(),
            self.cpu.c(),
            self.cpu.d(),
            self.cpu.e(),
            self.cpu.h(),
            self.cpu.l(),
        });
    }

    fn tickPeripherals(self: *Machine, cycles: u8, frame_ready: *bool) void {
        if (cycles == 0) return;

        const lcdc = self.bus.io.getLcdc();
        const lcd_enabled = (lcdc & 0x80) != 0;
        if (self.ppu.isEnabled() != lcd_enabled) {
            self.ppu.setEnabled(lcd_enabled);
            self.ppu.syncIoState(&self.bus);

            if (!lcd_enabled) {
                self.bus.io.setLy(0);
                self.bus.io.setPpuMode(0);
                self.bus.io.setPpuMemoryBlocked(false, false);
                self.bus.io.releaseMode0Stat();
            }
        }

        self.ppu.tick(cycles, &self.bus);
        if (self.ppu.takeFrameReady()) {
            self.frames += 1;
            frame_ready.* = true;
        }

        const divider_start = self.bus.timer.system_counter;
        self.bus.tickTimer(cycles);
        self.bus.tickApu(cycles, divider_start, self.options.capture_audio);
        self.bus.tickSerial(cycles);
        self.bus.tickDma(cycles);
        self.bus.cartridge.mbc.tick(cycles);
    }

    fn captureBusState(self: *const Machine) BusState {
        var state = BusState{
            .wram = self.bus.wram,
            .hram = self.bus.hram,
            .oam = self.bus.oam,
            .vram = self.bus.vram,
            .io = .{
                .data = self.bus.io.data,
                .joypad_select = self.bus.io.joypad_select,
                .joypad_buttons = self.bus.io.joypad_buttons,
                .oam_scan_row = self.bus.io.oam_scan_row,
                .ppu_oam_read_blocked = self.bus.io.ppu_oam_read_blocked,
                .ppu_oam_write_blocked = self.bus.io.ppu_oam_write_blocked,
                .ppu_vram_read_blocked = self.bus.io.ppu_vram_read_blocked,
                .ppu_vram_write_blocked = self.bus.io.ppu_vram_write_blocked,
                .stat_irq_line = self.bus.io.stat_irq_line,
                .stat_mode0_suppressed = self.bus.io.stat_mode0_suppressed,
                .stat_read_early_hblank = self.bus.io.stat_read_early_hblank,
                .late_interrupts = self.bus.io.late_interrupts,
            },
            .ie_register = self.bus.ie_register,
            .timer = self.bus.timer,
            .serial = self.bus.serial,
            .apu = self.bus.apu,
            .dma = self.bus.dma,
            .mbc = self.bus.cartridge.mbc.snapshot(),
            .cart_ram_len = 0,
            .cart_ram = [_]u8{0} ** MAX_CART_RAM_BYTES,
        };

        if (self.bus.cartridge.ram_data) |ram| {
            state.cart_ram_len = @min(ram.len, MAX_CART_RAM_BYTES);
            @memcpy(state.cart_ram[0..state.cart_ram_len], ram[0..state.cart_ram_len]);
        }
        return state;
    }

    fn applyBusState(self: *Machine, state: BusState) void {
        self.bus.wram = state.wram;
        self.bus.hram = state.hram;
        self.bus.oam = state.oam;
        self.bus.vram = state.vram;

        self.bus.io.data = state.io.data;
        self.bus.io.joypad_select = state.io.joypad_select;
        self.bus.io.joypad_buttons = state.io.joypad_buttons;
        self.bus.io.oam_scan_row = state.io.oam_scan_row;
        self.bus.io.ppu_oam_read_blocked = state.io.ppu_oam_read_blocked;
        self.bus.io.ppu_oam_write_blocked = state.io.ppu_oam_write_blocked;
        self.bus.io.ppu_vram_read_blocked = state.io.ppu_vram_read_blocked;
        self.bus.io.ppu_vram_write_blocked = state.io.ppu_vram_write_blocked;
        self.bus.io.stat_irq_line = state.io.stat_irq_line;
        self.bus.io.stat_mode0_suppressed = state.io.stat_mode0_suppressed;
        self.bus.io.stat_read_early_hblank = state.io.stat_read_early_hblank;
        self.bus.io.late_interrupts = state.io.late_interrupts;
        self.bus.io.serial_output.clearRetainingCapacity();

        self.bus.ie_register = state.ie_register;
        self.bus.timer = state.timer;
        self.bus.serial = state.serial;
        self.bus.apu = state.apu;
        self.bus.apu.discardSamples();
        self.bus.dma = state.dma;
        self.bus.cartridge.mbc.restore(state.mbc);

        if (self.bus.cartridge.ram_data) |ram| {
            const len = @min(@min(ram.len, MAX_CART_RAM_BYTES), state.cart_ram_len);
            if (len > 0) @memcpy(ram[0..len], state.cart_ram[0..len]);
            if (ram.len > len) @memset(ram[len..], 0);
        }
    }
};

fn hashInteger(hash: *std.hash.Wyhash, value: anytype) void {
    var copy = value;
    hash.update(std.mem.asBytes(&copy));
}

fn classifyMooneyeRegisters(registers: [6]u8) ?MooneyeResult {
    if (std.mem.eql(u8, &registers, &.{ 3, 5, 8, 13, 21, 34 })) return .passed;
    if (std.mem.allEqual(u8, &registers, 0x42)) return .failed;
    return null;
}

test "Mooneye result register signatures" {
    try std.testing.expectEqual(MooneyeResult.passed, classifyMooneyeRegisters(.{ 3, 5, 8, 13, 21, 34 }).?);
    try std.testing.expectEqual(MooneyeResult.failed, classifyMooneyeRegisters(.{ 0x42, 0x42, 0x42, 0x42, 0x42, 0x42 }).?);
    try std.testing.expectEqual(@as(?MooneyeResult, null), classifyMooneyeRegisters(.{ 3, 5, 8, 13, 21, 33 }));
}

test "machine snapshot restores mutable hardware and mapper state" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    machine.cpu.pc = 0x1234;
    machine.bus.wram[7] = 0xA5;
    machine.bus.cartridge.mbc.writeRom(0x2000, 3);
    const state = machine.capture();

    machine.cpu.pc = 0xFFFF;
    machine.bus.wram[7] = 0;
    machine.restore(state);

    try std.testing.expectEqual(@as(u16, 0x1234), machine.cpu.pc);
    try std.testing.expectEqual(@as(u8, 0xA5), machine.bus.wram[7]);
    try std.testing.expectEqual(state.bus.mbc.rom_bank, machine.bus.cartridge.mbc.snapshot().rom_bank);
}

test "machine button API uses the DMG active-low layout" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    machine.setButtons(.{ .right = true, .a = true, .start = true });
    try std.testing.expectEqual(@as(u8, 0x6E), machine.bus.io.getJoypadState());
}

test "machine forks share ROM and isolate mutable state" {
    var parent = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer parent.deinit();
    parent.cpu.pc = 0x2345;
    parent.bus.wram[0] = 0x42;

    var branch = try parent.fork(std.testing.allocator);
    defer branch.deinit();

    try std.testing.expectEqual(parent.observableDigest(), branch.observableDigest());
    try std.testing.expectEqual(parent.bus.cartridge.rom_data.ptr, branch.bus.cartridge.rom_data.ptr);
    branch.bus.wram[0] = 0x99;
    try std.testing.expectEqual(@as(u8, 0x42), parent.bus.wram[0]);
    try std.testing.expect(parent.observableDigest() != branch.observableDigest());
}

test "frame stepping can omit pixels without omitting hardware frames" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    @memset(&machine.ppu.frame_buffer, [_]ppu_mod.DmgColor{.Black} ** ppu_mod.SCREEN_WIDTH);
    const timing_only = machine.stepFrames(1, .{
        .video = .none,
        .max_instructions_per_frame = 100_000,
    });
    try std.testing.expect(!timing_only.timed_out);
    try std.testing.expectEqual(@as(usize, 1), timing_only.frames_completed);
    try std.testing.expect(std.mem.allEqual(
        u8,
        std.mem.asBytes(&machine.ppu.frame_buffer),
        @intFromEnum(ppu_mod.DmgColor.Black),
    ));

    const observed = machine.stepFrames(1, .{
        .video = .final_frame,
        .max_instructions_per_frame = 100_000,
    });
    try std.testing.expect(!observed.timed_out);
    try std.testing.expectEqual(@as(usize, 1), observed.frames_completed);
    try std.testing.expect(!std.mem.allEqual(
        u8,
        std.mem.asBytes(&machine.ppu.frame_buffer),
        @intFromEnum(ppu_mod.DmgColor.Black),
    ));
}

test "frame input timelines apply at deterministic boundaries" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    const inputs = [_]FrameInput{
        .{ .frame_offset = 0, .buttons = .{ .a = true } },
        .{ .frame_offset = 1, .buttons = .{ .right = true, .start = true } },
    };
    const result = try machine.stepFramesWithInputs(2, &inputs, .{
        .video = .none,
        .max_instructions_per_frame = 100_000,
    });
    try std.testing.expect(!result.timed_out);
    try std.testing.expectEqual(@as(u8, 0x7E), machine.bus.io.getJoypadState());

    const observation = machine.observe();
    try std.testing.expectEqual(machine.frames, observation.frames);
    try std.testing.expectEqual(machine.cpu.pc, observation.cpu.pc);
    try std.testing.expectEqual(@as(usize, 0x400), observation.background_tilemap.len);
}
