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
const mbc_mod = @import("memory/mbc.zig");
const Mbc = mbc_mod.Mbc;
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

    /// Optional deterministic power-on value for cartridges with an MBC3 RTC.
    rtc_seed: ?mbc_mod.RtcSeed = null,
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

/// Input transition at an absolute emulated T-cycle. A transition that lands
/// inside an instruction is applied between peripheral dots before execution
/// continues, including before a bus read committed at that boundary.
pub const CycleInput = struct {
    cycle: u64,
    buttons: Buttons,
};

pub const CycleStepResult = struct {
    requested_cycle: u64,
    reached_cycle: u64,
    instructions: usize,
};

pub const ResetOptions = struct {
    /// AI/research resets normally begin without battery-backed episode state.
    clear_cartridge_ram: bool = true,
    rtc_seed: mbc_mod.RtcSeed = .{},
    buttons: Buttons = .{},
};

pub const RtcSeed = mbc_mod.RtcSeed;

pub const CpuObservation = struct {
    af: u16,
    bc: u16,
    de: u16,
    hl: u16,
    sp: u16,
    pc: u16,
    ime: bool,
    halted: bool,
    stopped: bool,
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
    stopped: bool,
    halt_bug: bool,
    cycles: u64,
};

const IoState = struct {
    data: [0x80]u8,
    joypad_select: u8,
    joypad_buttons: u8,
    stop_wake_requested: bool,
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

const CoreBusState = struct {
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
};

const CoreSnapshot = struct {
    cpu: CpuState,
    bus: CoreBusState,
    ppu: Ppu,
    steps: usize,
    frames: usize,
};

/// A complete, allocation-free mutable machine snapshot. The fixed cartridge
/// RAM reserve preserves the original value API; branch-heavy callers should
/// prefer `OwnedSnapshot`, which stores only the cartridge RAM actually used.
pub const Snapshot = struct {
    core: CoreSnapshot,
    cart_ram_len: usize,
    cart_ram: [MAX_CART_RAM_BYTES]u8,
};

pub const OwnedSnapshot = struct {
    allocator: Allocator,
    core: CoreSnapshot,
    cartridge_ram: []u8,

    pub fn deinit(self: *OwnedSnapshot) void {
        self.allocator.free(self.cartridge_ram);
        self.* = undefined;
    }

    pub fn byteSize(self: *const OwnedSnapshot) usize {
        return @sizeOf(CoreSnapshot) + self.cartridge_ram.len;
    }
};

const CycleInputCursor = struct {
    inputs: []const CycleInput,
    index: usize = 0,
    cycle: u64,
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
        if (options.rtc_seed) |seed| machine.bus.cartridge.mbc.seedRtc(seed);
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
        self.copyHardwareStateTo(&branch);
        return branch;
    }

    /// Execute one instruction and preserve each bus access at its exact
    /// emulated phase. The callback is an internal compatibility bridge; the
    /// public machine API itself has no host callbacks.
    pub fn step(self: *Machine) StepResult {
        return self.stepWithCycleInputs(null);
    }

    fn stepWithCycleInputs(self: *Machine, cycle_inputs: ?*CycleInputCursor) StepResult {
        var clocked_cycles: u16 = 0;
        var frame_ready = false;

        const HookContext = struct {
            machine: *Machine,
            clocked: *u16,
            frame_ready: *bool,
            cycle_inputs: ?*CycleInputCursor,

            fn tick(ptr: *anyopaque, cycles: u8) void {
                const ctx: *@This() = @ptrCast(@alignCast(ptr));
                ctx.machine.tickWithCycleInputs(cycles, ctx.frame_ready, ctx.cycle_inputs);
                ctx.clocked.* +%= cycles;
            }
        };
        var hook_ctx = HookContext{
            .machine = self,
            .clocked = &clocked_cycles,
            .frame_ready = &frame_ready,
            .cycle_inputs = cycle_inputs,
        };
        self.bus.setCycleHook(.{
            .context = @ptrCast(&hook_ctx),
            .tickFn = HookContext.tick,
        });
        defer self.bus.setCycleHook(null);

        const cycles = self.cpu.step(&self.bus);
        if (clocked_cycles < @as(u16, cycles)) {
            const remaining: u8 = @intCast(@as(u16, cycles) - clocked_cycles);
            self.tickWithCycleInputs(remaining, &frame_ready, cycle_inputs);
        }

        self.steps += 1;
        return .{ .cycles = cycles, .frame_ready = frame_ready };
    }

    pub fn runInstructions(self: *Machine, count: usize) void {
        for (0..count) |_| _ = self.step();
    }

    /// Advance until at least `target_cycle`, applying sorted input transitions
    /// at their exact T-cycle even when the final instruction overshoots it.
    pub fn runUntilCycle(
        self: *Machine,
        target_cycle: u64,
        inputs: []const CycleInput,
    ) error{ CycleInPast, InputOutsideRange, UnsortedInputs, Stopped }!CycleStepResult {
        if (target_cycle < self.cpu.cycles) return error.CycleInPast;

        var previous_cycle = self.cpu.cycles;
        for (inputs, 0..) |input, index| {
            if (input.cycle < self.cpu.cycles or input.cycle > target_cycle) {
                return error.InputOutsideRange;
            }
            if (index != 0 and input.cycle < previous_cycle) return error.UnsortedInputs;
            previous_cycle = input.cycle;
        }

        var cursor: CycleInputCursor = .{
            .inputs = inputs,
            .cycle = self.cpu.cycles,
        };
        self.applyCycleInputsAtCurrent(&cursor);
        const start_steps = self.steps;
        while (self.cpu.cycles < target_cycle) {
            if (self.stepWithCycleInputs(&cursor).cycles == 0) return error.Stopped;
        }

        return .{
            .requested_cycle = target_cycle,
            .reached_cycle = self.cpu.cycles,
            .instructions = self.steps - start_steps,
        };
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
                .stopped = self.cpu.stopped,
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

    /// Reinitialize an episode without consulting host time or retaining
    /// accidental battery/RTC state from a prior run.
    pub fn resetDeterministic(self: *Machine, options: ResetOptions) void {
        self.reset();
        if (options.clear_cartridge_ram) {
            if (self.bus.cartridge.ram_data) |ram| @memset(ram, 0);
        }
        self.bus.cartridge.mbc.seedRtc(options.rtc_seed);
        self.setButtons(options.buttons);
    }

    pub fn capture(self: *const Machine) Snapshot {
        var snapshot: Snapshot = .{
            .core = self.captureCoreState(),
            .cart_ram_len = 0,
            .cart_ram = [_]u8{0} ** MAX_CART_RAM_BYTES,
        };
        if (self.bus.cartridge.ram_data) |ram| {
            snapshot.cart_ram_len = @min(ram.len, MAX_CART_RAM_BYTES);
            @memcpy(snapshot.cart_ram[0..snapshot.cart_ram_len], ram[0..snapshot.cart_ram_len]);
        }
        return snapshot;
    }

    pub fn restore(self: *Machine, state: Snapshot) void {
        self.applyCoreState(state.core);
        if (self.bus.cartridge.ram_data) |ram| {
            const len = @min(@min(ram.len, MAX_CART_RAM_BYTES), state.cart_ram_len);
            if (len > 0) @memcpy(ram[0..len], state.cart_ram[0..len]);
            if (ram.len > len) @memset(ram[len..], 0);
        }
    }

    /// Capture a complete save state using only the cartridge RAM length this
    /// machine actually owns. The caller owns the result and must call deinit.
    pub fn captureOwned(self: *const Machine, allocator: Allocator) !OwnedSnapshot {
        const ram_len = if (self.bus.cartridge.ram_data) |ram| ram.len else 0;
        const cartridge_ram = try allocator.alloc(u8, ram_len);
        if (self.bus.cartridge.ram_data) |ram| @memcpy(cartridge_ram, ram);
        return .{
            .allocator = allocator,
            .core = self.captureCoreState(),
            .cartridge_ram = cartridge_ram,
        };
    }

    /// Refresh an existing compact checkpoint without allocating. The
    /// checkpoint must have been created for a cartridge with the same mutable
    /// RAM size; validation occurs before either snapshot region is changed.
    pub fn captureOwnedInto(
        self: *const Machine,
        destination: *OwnedSnapshot,
    ) error{CartridgeRamSizeMismatch}!void {
        const ram_len = if (self.bus.cartridge.ram_data) |ram| ram.len else 0;
        if (ram_len != destination.cartridge_ram.len) return error.CartridgeRamSizeMismatch;

        destination.core = self.captureCoreState();
        if (self.bus.cartridge.ram_data) |ram| @memcpy(destination.cartridge_ram, ram);
    }

    pub fn restoreOwned(
        self: *Machine,
        state: *const OwnedSnapshot,
    ) error{CartridgeRamSizeMismatch}!void {
        const ram_len = if (self.bus.cartridge.ram_data) |ram| ram.len else 0;
        if (ram_len != state.cartridge_ram.len) return error.CartridgeRamSizeMismatch;
        self.applyCoreState(state.core);
        if (self.bus.cartridge.ram_data) |ram| @memcpy(ram, state.cartridge_ram);
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
        hashInteger(&hash, @intFromBool(self.cpu.ime));
        hashInteger(&hash, @intFromBool(self.cpu.halted));
        hashInteger(&hash, @intFromBool(self.cpu.stopped));
        hashInteger(&hash, self.steps);
        hashInteger(&hash, self.frames);
        hashInteger(&hash, self.bus.timer.system_counter);
        hashInteger(&hash, self.bus.io.joypad_select);
        hashInteger(&hash, self.bus.io.joypad_buttons);
        hashInteger(&hash, @intFromBool(self.bus.io.stop_wake_requested));
        hashInteger(&hash, @intFromBool(self.bus.serial.active));
        hashInteger(&hash, @intFromEnum(self.bus.serial.clock_source));
        hashInteger(&hash, self.bus.serial.outgoing);
        hashInteger(&hash, self.bus.serial.bits_remaining);
        hashInteger(&hash, self.bus.serial.dots_until_shift);
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

    /// Inspect the active serial transfer without advancing emulated time.
    pub fn serialClockSource(self: *const Machine) ?Serial.ClockSource {
        return if (self.bus.serial.active) self.bus.serial.clock_source else null;
    }

    /// Sample the link port's SOUT level before a partner clocks the next bit.
    pub fn serialOutgoingBit(self: *const Machine) ?u1 {
        return self.bus.serial.outgoingBit(&self.bus.io);
    }

    /// Drive one external serial clock edge. This is the deterministic link
    /// boundary for a cable, printer, four-player adapter, or test harness.
    pub fn clockSerialExternal(self: *Machine, incoming: u1) ?u1 {
        return self.bus.serial.clockExternal(&self.bus.io, incoming);
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

    fn applyCycleInputsAtCurrent(self: *Machine, cursor: *CycleInputCursor) void {
        while (cursor.index < cursor.inputs.len and
            cursor.inputs[cursor.index].cycle == cursor.cycle) : (cursor.index += 1)
        {
            self.setButtons(cursor.inputs[cursor.index].buttons);
        }
    }

    fn tickWithCycleInputs(
        self: *Machine,
        cycles: u8,
        frame_ready: *bool,
        maybe_cursor: ?*CycleInputCursor,
    ) void {
        const cursor = maybe_cursor orelse {
            self.tickPeripherals(cycles, frame_ready);
            return;
        };

        var remaining = cycles;
        while (cursor.index < cursor.inputs.len) {
            const event_cycle = cursor.inputs[cursor.index].cycle;
            const end_cycle = cursor.cycle + remaining;
            if (event_cycle > end_cycle) break;

            const prefix: u8 = @intCast(event_cycle - cursor.cycle);
            if (prefix != 0) {
                self.tickPeripherals(prefix, frame_ready);
                cursor.cycle += prefix;
                remaining -= prefix;
            }
            self.applyCycleInputsAtCurrent(cursor);
        }

        if (remaining != 0) {
            self.tickPeripherals(remaining, frame_ready);
            cursor.cycle += remaining;
        }
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

    fn captureCoreState(self: *const Machine) CoreSnapshot {
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
                .stopped = self.cpu.stopped,
                .halt_bug = self.cpu.halt_bug,
                .cycles = self.cpu.cycles,
            },
            .bus = self.captureCoreBusState(),
            .ppu = self.ppu,
            .steps = self.steps,
            .frames = self.frames,
        };
    }

    fn captureCoreBusState(self: *const Machine) CoreBusState {
        return .{
            .wram = self.bus.wram,
            .hram = self.bus.hram,
            .oam = self.bus.oam,
            .vram = self.bus.vram,
            .io = .{
                .data = self.bus.io.data,
                .joypad_select = self.bus.io.joypad_select,
                .joypad_buttons = self.bus.io.joypad_buttons,
                .stop_wake_requested = self.bus.io.stop_wake_requested,
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
        };
    }

    /// Copy mutable hardware directly into a freshly initialized branch. This
    /// avoids materializing the legacy fixed-capacity snapshot, whose 128 KiB
    /// cartridge-RAM reserve dominated fork cost even for ROM-only games.
    fn copyHardwareStateTo(self: *const Machine, branch: *Machine) void {
        branch.cpu = self.cpu;
        branch.cpu.reader_ctx = undefined;

        branch.bus.wram = self.bus.wram;
        branch.bus.hram = self.bus.hram;
        branch.bus.oam = self.bus.oam;
        branch.bus.vram = self.bus.vram;

        branch.bus.io.data = self.bus.io.data;
        branch.bus.io.joypad_select = self.bus.io.joypad_select;
        branch.bus.io.joypad_buttons = self.bus.io.joypad_buttons;
        branch.bus.io.stop_wake_requested = self.bus.io.stop_wake_requested;
        branch.bus.io.oam_scan_row = self.bus.io.oam_scan_row;
        branch.bus.io.ppu_oam_read_blocked = self.bus.io.ppu_oam_read_blocked;
        branch.bus.io.ppu_oam_write_blocked = self.bus.io.ppu_oam_write_blocked;
        branch.bus.io.ppu_vram_read_blocked = self.bus.io.ppu_vram_read_blocked;
        branch.bus.io.ppu_vram_write_blocked = self.bus.io.ppu_vram_write_blocked;
        branch.bus.io.stat_irq_line = self.bus.io.stat_irq_line;
        branch.bus.io.stat_mode0_suppressed = self.bus.io.stat_mode0_suppressed;
        branch.bus.io.stat_read_early_hblank = self.bus.io.stat_read_early_hblank;
        branch.bus.io.late_interrupts = self.bus.io.late_interrupts;

        branch.bus.ie_register = self.bus.ie_register;
        branch.bus.timer = self.bus.timer;
        branch.bus.serial = self.bus.serial;
        branch.bus.apu = self.bus.apu;
        branch.bus.apu.discardSamples();
        branch.bus.dma = self.bus.dma;
        branch.bus.cartridge.mbc.restore(self.bus.cartridge.mbc.snapshot());

        branch.ppu = self.ppu;
        branch.steps = self.steps;
        branch.frames = self.frames;
    }

    fn applyCoreState(self: *Machine, state: CoreSnapshot) void {
        self.cpu.af = state.cpu.af;
        self.cpu.bc = state.cpu.bc;
        self.cpu.de = state.cpu.de;
        self.cpu.hl = state.cpu.hl;
        self.cpu.sp = state.cpu.sp;
        self.cpu.pc = state.cpu.pc;
        self.cpu.ime = state.cpu.ime;
        self.cpu.ime_enable_delay = state.cpu.ime_enable_delay;
        self.cpu.halted = state.cpu.halted;
        self.cpu.stopped = state.cpu.stopped;
        self.cpu.halt_bug = state.cpu.halt_bug;
        self.cpu.cycles = state.cpu.cycles;
        self.cpu.reader_ctx = undefined;

        self.applyCoreBusState(state.bus);
        const capture_pixels = self.ppu.isPixelCaptureEnabled();
        self.ppu = state.ppu;
        self.ppu.setPixelCapture(capture_pixels);
        self.steps = state.steps;
        self.frames = state.frames;
    }

    fn applyCoreBusState(self: *Machine, state: CoreBusState) void {
        self.bus.wram = state.wram;
        self.bus.hram = state.hram;
        self.bus.oam = state.oam;
        self.bus.vram = state.vram;

        self.bus.io.data = state.io.data;
        self.bus.io.joypad_select = state.io.joypad_select;
        self.bus.io.joypad_buttons = state.io.joypad_buttons;
        self.bus.io.stop_wake_requested = state.io.stop_wake_requested;
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
    try std.testing.expectEqual(state.core.bus.mbc.rom_bank, machine.bus.cartridge.mbc.snapshot().rom_bank);
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

test "machine STOP freezes peripherals and wakes from selected buttons" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    machine.cpu.pc = 0xC000;
    machine.bus.wram[0] = 0x10; // STOP
    machine.bus.wram[1] = 0x00; // Padding byte
    machine.bus.io.write(@intFromEnum(@import("memory/io.zig").IoReg.JOYP), 0x10);
    machine.bus.io.stop_wake_requested = false;

    const entered = machine.step();
    try std.testing.expectEqual(@as(u8, 8), entered.cycles);
    try std.testing.expect(machine.cpu.stopped);
    try std.testing.expectEqual(@as(u16, 0), machine.bus.timer.system_counter);
    const stopped_state = machine.capture();

    const ppu_before = machine.ppu;
    const serial_before = machine.bus.serial;
    const dma_before = machine.bus.dma;
    const apu_before = machine.bus.apu;
    const mapper_before = machine.bus.cartridge.mbc.snapshot();
    const io_before = machine.bus.io.data;
    const idle = machine.step();
    try std.testing.expectEqual(@as(u8, 0), idle.cycles);
    try std.testing.expectEqual(@as(u16, 0), machine.bus.timer.system_counter);
    try std.testing.expectEqualDeep(ppu_before, machine.ppu);
    try std.testing.expectEqualDeep(serial_before, machine.bus.serial);
    try std.testing.expectEqualDeep(dma_before, machine.bus.dma);
    try std.testing.expectEqualDeep(apu_before, machine.bus.apu);
    try std.testing.expectEqualDeep(mapper_before, machine.bus.cartridge.mbc.snapshot());
    try std.testing.expectEqualSlices(u8, &io_before, &machine.bus.io.data);
    try std.testing.expectError(
        error.Stopped,
        machine.runUntilCycle(machine.cpu.cycles + 4, &.{}),
    );

    machine.setButtons(.{ .a = true });
    const woke = machine.step();
    try std.testing.expectEqual(@as(u8, 4), woke.cycles);
    try std.testing.expect(!machine.cpu.stopped);
    try std.testing.expectEqual(@as(u16, 4), machine.bus.timer.system_counter);

    machine.restore(stopped_state);
    try std.testing.expect(machine.cpu.stopped);
    try std.testing.expectEqual(@as(u8, 0), machine.step().cycles);
}

test "machine exposes deterministic external serial clock edges" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    machine.bus.io.data[@intFromEnum(@import("memory/io.zig").IoReg.SB)] = 0x96;
    machine.bus.serial.writeControl(&machine.bus.io, 0x80);
    try std.testing.expectEqual(Serial.ClockSource.external, machine.serialClockSource().?);
    try std.testing.expectEqual(@as(u1, 1), machine.serialOutgoingBit().?);

    const digest_before_pulse = machine.observableDigest();
    var outgoing: u8 = machine.clockSerialExternal(0).?;
    try std.testing.expect(digest_before_pulse != machine.observableDigest());
    for (1..8) |index| {
        const incoming: u1 = @truncate(@as(u8, 0x3C) >> @intCast(7 - index));
        outgoing = (outgoing << 1) | machine.clockSerialExternal(incoming).?;
    }
    try std.testing.expectEqual(@as(u8, 0x96), outgoing);
    try std.testing.expectEqual(@as(?Serial.ClockSource, null), machine.serialClockSource());
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

test "cycle input timelines split instruction clocks at exact T-cycles" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();

    const inputs = [_]CycleInput{
        .{ .cycle = 2, .buttons = .{ .a = true } },
        .{ .cycle = 6, .buttons = .{ .start = true } },
    };
    const result = try machine.runUntilCycle(7, &inputs);
    try std.testing.expectEqual(@as(u64, 7), result.requested_cycle);
    try std.testing.expectEqual(@as(u64, 8), result.reached_cycle);
    try std.testing.expectEqual(@as(usize, 2), result.instructions);
    try std.testing.expectEqual(@as(u8, 0x7F), machine.bus.io.getJoypadState());
}

test "deterministic reset clears episode RAM and reseeds MBC3 time" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").rtcCartridge(std.testing.allocator),
        .{ .rtc_seed = .{ .seconds = 12, .day = 4 } },
    );
    defer machine.deinit();

    try std.testing.expectEqual(@as(u8, 12), machine.inspectCartridge().mapper.rtc.seconds);
    machine.bus.cartridge.ram_data.?[3] = 0xCC;
    machine.bus.cartridge.mbc.seedRtc(.{ .seconds = 59, .day = 8 });

    machine.resetDeterministic(.{
        .rtc_seed = .{ .minutes = 7, .day = 42, .cycle_accumulator = 99 },
        .buttons = .{ .b = true },
    });
    const inspection = machine.inspectCartridge();
    try std.testing.expectEqual(@as(u8, 0), machine.bus.cartridge.ram_data.?[3]);
    try std.testing.expectEqual(@as(u8, 0), inspection.mapper.rtc.seconds);
    try std.testing.expectEqual(@as(u8, 7), inspection.mapper.rtc.minutes);
    try std.testing.expectEqual(@as(u9, 42), inspection.mapper.rtc.day);
    try std.testing.expectEqual(@as(u32, 99), inspection.mapper.rtc.cycle_accumulator);
    try std.testing.expectEqual(@as(u8, 0xDF), machine.bus.io.getJoypadState());
}

test "owned snapshots store only the cartridge RAM in use" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").rtcCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();
    machine.cpu.pc = 0x4567;
    machine.bus.wram[9] = 0xAB;
    machine.bus.cartridge.ram_data.?[17] = 0xCD;

    var state = try machine.captureOwned(std.testing.allocator);
    defer state.deinit();
    try std.testing.expectEqual(@as(usize, 0x2000), state.cartridge_ram.len);
    try std.testing.expect(state.byteSize() < @sizeOf(Snapshot));

    machine.cpu.pc = 0;
    machine.bus.wram[9] = 0;
    machine.bus.cartridge.ram_data.?[17] = 0;
    try machine.restoreOwned(&state);
    try std.testing.expectEqual(@as(u16, 0x4567), machine.cpu.pc);
    try std.testing.expectEqual(@as(u8, 0xAB), machine.bus.wram[9]);
    try std.testing.expectEqual(@as(u8, 0xCD), machine.bus.cartridge.ram_data.?[17]);
}

test "owned snapshot storage can be refreshed without reallocation" {
    var machine = Machine.init(
        std.testing.allocator,
        try @import("test_support.zig").rtcCartridge(std.testing.allocator),
        .{},
    );
    defer machine.deinit();
    var state = try machine.captureOwned(std.testing.allocator);
    defer state.deinit();
    const storage = state.cartridge_ram.ptr;

    machine.cpu.pc = 0x8123;
    machine.bus.cartridge.ram_data.?[31] = 0xA7;
    try machine.captureOwnedInto(&state);
    try std.testing.expectEqual(storage, state.cartridge_ram.ptr);

    machine.cpu.pc = 0;
    machine.bus.cartridge.ram_data.?[31] = 0;
    try machine.restoreOwned(&state);
    try std.testing.expectEqual(@as(u16, 0x8123), machine.cpu.pc);
    try std.testing.expectEqual(@as(u8, 0xA7), machine.bus.cartridge.ram_data.?[31]);
}
