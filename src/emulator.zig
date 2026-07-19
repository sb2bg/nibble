const std = @import("std");
const Allocator = std.mem.Allocator;
const Cpu = @import("cpu/cpu.zig").Cpu;
const bus_mod = @import("memory/bus.zig");
const Bus = bus_mod.Bus;
const Dma = bus_mod.Dma;
const Cartridge = @import("cartridge/cartridge.zig").Cartridge;
const Timer = @import("timer.zig").Timer;
const Serial = @import("serial.zig").Serial;
const Mbc = @import("memory/mbc.zig").Mbc;
const ppu_mod = @import("ppu/ppu.zig");
const Ppu = ppu_mod.Ppu;
const frontend_mod = @import("frontend/sdl_frontend.zig");
const SdlFrontend = frontend_mod.SdlFrontend;

pub const EmulatorOptions = struct {
    debug: bool = false,
    max_steps: ?usize = null, // null means run indefinitely
    breakpoint: ?u16 = null,
    headless: bool = false, // Run without graphics (for testing)
    mooneye_test: bool = false,
};

pub const MooneyeResult = enum {
    passed,
    failed,
};

const SAVE_SLOT_COUNT = 10;
const MAX_CART_RAM_BYTES = 128 * 1024;

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
    dma: Dma,
    mbc: Mbc.Snapshot,
    cart_ram_len: usize,
    cart_ram: [MAX_CART_RAM_BYTES]u8,
};

const SaveState = struct {
    cpu: CpuState,
    bus: BusState,
    ppu: Ppu,
    steps: usize,
};

pub const Emulator = struct {
    io: std.Io,
    cpu: Cpu,
    bus: Bus,
    ppu: Ppu,
    frontend: ?SdlFrontend,
    options: EmulatorOptions,

    // Runtime state
    steps: usize = 0,
    running: bool = false,
    paused: bool = false,
    active_save_slot: u8 = 0,
    status_message: [48]u8 = [_]u8{0} ** 48,
    status_message_len: usize = 0,
    save_slots: [SAVE_SLOT_COUNT]?SaveState = [_]?SaveState{null} ** SAVE_SLOT_COUNT,
    last_ui_redraw_ns: i96 = 0,
    next_frame_deadline_ns: i96 = 0,

    /// Initialize the emulator with a ROM file
    pub fn init(allocator: Allocator, io: std.Io, rom_path: []const u8, options: EmulatorOptions) !Emulator {
        // Load the cartridge
        var cartridge = try Cartridge.load(allocator, io, rom_path);
        errdefer cartridge.deinit();

        // The PPU is always present. SDL is an optional host adapter, so a
        // missing display can never disable LY/STAT/VBlank behavior.
        var frontend: ?SdlFrontend = null;
        if (!options.headless) {
            frontend = SdlFrontend.init() catch |err| blk: {
                std.debug.print("Warning: Failed to initialize SDL frontend: {any}\n", .{err});
                std.debug.print("Falling back to headless execution\n", .{});
                break :blk null;
            };
        }
        errdefer if (frontend) |*active| active.deinit();

        return Emulator{
            .io = io,
            .cpu = Cpu.init(),
            .bus = Bus.init(allocator, cartridge),
            .ppu = Ppu.init(),
            .frontend = frontend,
            .options = options,
            .steps = 0,
            .running = false,
        };
    }

    pub fn deinit(self: *Emulator) void {
        if (self.frontend) |*frontend| frontend.deinit();
        self.bus.deinit();
    }

    /// Run the emulator's main loop
    pub fn run(self: *Emulator) void {
        self.running = true;
        self.paused = false;
        self.last_ui_redraw_ns = std.Io.Clock.awake.now(self.io).nanoseconds;
        self.next_frame_deadline_ns = 0;
        if (self.status_message_len == 0) {
            self.setStatusMessage("READY");
        }

        if (self.options.debug) {
            self.bus.cartridge.printInfo();
            std.debug.print("\n=== Starting Execution ===\n\n", .{});
            std.debug.print("Initial state:\n", .{});
            self.printCpuState();
        }

        // Enable PPU if LCDC bit 7 is set
        const lcdc = self.bus.io.getLcdc();
        self.ppu.setEnabled((lcdc & 0x80) != 0);
        self.syncUiStatus();

        while (self.running) {
            // Polling SDL for every CPU instruction was a significant host-side
            // hot path. A 256-instruction cadence is still well below a frame;
            // paused execution polls every loop so management keys stay live.
            const should_poll_frontend = self.paused or (self.steps & 0xFF) == 0;
            const actions: frontend_mod.UiActions = if (should_poll_frontend and self.frontend != null)
                self.frontend.?.pollEvents(&self.bus)
            else
                .{};
            if (actions.quit) break;
            if (should_poll_frontend) {
                self.handleUiActions(actions);
                self.syncUiStatus();
                if (actions.redraw) {
                    if (self.frontend) |*frontend| frontend.present(&self.ppu.frame_buffer);
                }
                self.maybeRedrawUi();
            }

            if (self.paused) {
                std.Io.sleep(self.io, .fromMilliseconds(8), .awake) catch {};
                continue;
            }

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

            if (self.options.mooneye_test and self.mooneyeResult() != null) break;

            self.step();
        }
    }

    /// Detect Mooneye's hardware-test result protocol. The register signature
    /// is only meaningful once the ROM reaches its `LD B,B; JR -2` stop loop;
    /// checking both avoids mistaking an intermediate Fibonacci value for a
    /// completed test.
    pub fn mooneyeResult(self: *const Emulator) ?MooneyeResult {
        const pc = self.cpu.pc;
        if (pc > std.math.maxInt(u16) - 2) return null;
        if (self.bus.read(pc) != 0x40 or
            self.bus.read(pc + 1) != 0x18 or
            self.bus.read(pc + 2) != 0xFE)
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

    /// Execute a single CPU step and tick other components
    pub fn step(self: *Emulator) void {
        const pc_before = self.cpu.pc;
        var clocked_cycles: u16 = 0;

        const HookContext = struct {
            emu: *Emulator,
            clocked: *u16,

            fn tick(ptr: *anyopaque, cycles: u8) void {
                const ctx: *@This() = @ptrCast(@alignCast(ptr));
                ctx.emu.tickPeripherals(cycles);
                ctx.clocked.* +%= cycles;
            }
        };
        var hook_ctx = HookContext{ .emu = self, .clocked = &clocked_cycles };
        self.bus.setCycleHook(.{
            .context = @ptrCast(&hook_ctx),
            .tickFn = HookContext.tick,
        });
        defer self.bus.setCycleHook(null);

        // Execute CPU instruction (returns cycles used)
        const cycles = self.cpu.step(&self.bus);

        // Some instructions have internal cycles without memory accesses.
        if (clocked_cycles < @as(u16, cycles)) {
            const remaining: u8 = @intCast(@as(u16, cycles) - clocked_cycles);
            self.tickPeripherals(remaining);
        }

        self.steps += 1;

        if (self.options.debug) {
            std.debug.print("\nStep {d} (PC=0x{X:0>4}, cycles={d}):\n", .{ self.steps, pc_before, cycles });
            self.printCpuState();
        }
    }

    fn tickPeripherals(self: *Emulator, cycles: u8) void {
        if (cycles == 0) return;

        // Update PPU enabled state from LCDC.
        const lcdc = self.bus.io.getLcdc();
        self.ppu.setEnabled((lcdc & 0x80) != 0);
        self.ppu.syncIoState(&self.bus);

        if ((lcdc & 0x80) == 0) {
            // LY reads as 0 while LCD is disabled.
            self.bus.io.setLy(0);
            self.bus.io.setPpuMode(0);
            self.bus.io.setPpuMemoryBlocked(false, false);
        }

        self.ppu.tick(cycles, &self.bus);
        if (self.ppu.takeFrameReady()) {
            if (self.frontend) |*frontend| {
                frontend.present(&self.ppu.frame_buffer);
                self.paceFrame();
            }
        }

        self.bus.tickTimer(cycles);
        self.bus.tickSerial(cycles);
        self.bus.tickDma(cycles);
        self.bus.cartridge.mbc.tick(cycles);
    }

    fn maybeRedrawUi(self: *Emulator) void {
        if (!self.paused) return;
        const frontend = if (self.frontend) |*active| active else return;
        const now = std.Io.Clock.awake.now(self.io).nanoseconds;
        if (now - self.last_ui_redraw_ns >= 16 * std.time.ns_per_ms) {
            frontend.redraw(&self.ppu.frame_buffer);
            self.last_ui_redraw_ns = now;
        }
    }

    /// SDL's vsync may be absent, disabled by the driver, or target a 120 Hz
    /// display. Pace against the DMG's 70,224-dot frame instead of assuming a
    /// host refresh rate. Large host stalls resynchronize rather than trying to
    /// run a burst of catch-up frames.
    fn paceFrame(self: *Emulator) void {
        const frame_ns: i96 = (70_224 * std.time.ns_per_s) / 4_194_304;
        var now = std.Io.Clock.awake.now(self.io).nanoseconds;

        if (self.next_frame_deadline_ns == 0) {
            self.next_frame_deadline_ns = now + frame_ns;
            return;
        }

        if (now < self.next_frame_deadline_ns) {
            std.Io.sleep(
                self.io,
                .fromNanoseconds(self.next_frame_deadline_ns - now),
                .awake,
            ) catch {};
            now = std.Io.Clock.awake.now(self.io).nanoseconds;
        }

        const next = self.next_frame_deadline_ns + frame_ns;
        self.next_frame_deadline_ns = if (now > next + frame_ns) now + frame_ns else next;
    }

    fn handleUiActions(self: *Emulator, actions: frontend_mod.UiActions) void {
        if (actions.prev_slot) {
            const slot = (@as(usize, self.active_save_slot) + SAVE_SLOT_COUNT - 1) % SAVE_SLOT_COUNT;
            self.active_save_slot = @intCast(slot);
            var buf: [24]u8 = undefined;
            const text = std.fmt.bufPrint(&buf, "SLOT {d}", .{self.active_save_slot}) catch "SLOT ?";
            self.setStatusMessage(text);
        }
        if (actions.next_slot) {
            const slot = (@as(usize, self.active_save_slot) + 1) % SAVE_SLOT_COUNT;
            self.active_save_slot = @intCast(slot);
            var buf: [24]u8 = undefined;
            const text = std.fmt.bufPrint(&buf, "SLOT {d}", .{self.active_save_slot}) catch "SLOT ?";
            self.setStatusMessage(text);
        }
        if (actions.toggle_pause) {
            self.paused = !self.paused;
            self.setStatusMessage(if (self.paused) "PAUSED" else "RUNNING");
        }
        if (actions.reset) {
            self.reset();
            self.running = true;
            self.paused = false;
            self.setStatusMessage("RESET");
        }
        if (actions.save_state) {
            self.saveStateToSlot(self.active_save_slot);
        }
        if (actions.load_state) {
            self.loadStateFromSlot(self.active_save_slot);
        }
    }

    fn syncUiStatus(self: *Emulator) void {
        const frontend = if (self.frontend) |*active| active else return;
        const slot_has_state = self.save_slots[self.active_save_slot] != null;
        frontend.setUiStatus(
            self.paused,
            self.active_save_slot,
            slot_has_state,
            self.status_message[0..self.status_message_len],
        );
    }

    fn setStatusMessage(self: *Emulator, message: []const u8) void {
        self.status_message_len = @min(message.len, self.status_message.len);
        var i: usize = 0;
        while (i < self.status_message_len) : (i += 1) {
            self.status_message[i] = std.ascii.toUpper(message[i]);
        }
    }

    fn saveStateToSlot(self: *Emulator, slot: u8) void {
        const state = SaveState{
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
        };

        self.save_slots[slot] = state;

        var buf: [32]u8 = undefined;
        const text = std.fmt.bufPrint(&buf, "SAVED SLOT {d}", .{slot}) catch "SAVED";
        self.setStatusMessage(text);
    }

    fn loadStateFromSlot(self: *Emulator, slot: u8) void {
        const slot_state = self.save_slots[slot] orelse {
            self.setStatusMessage("EMPTY SLOT");
            return;
        };

        self.cpu.af = slot_state.cpu.af;
        self.cpu.bc = slot_state.cpu.bc;
        self.cpu.de = slot_state.cpu.de;
        self.cpu.hl = slot_state.cpu.hl;
        self.cpu.sp = slot_state.cpu.sp;
        self.cpu.pc = slot_state.cpu.pc;
        self.cpu.ime = slot_state.cpu.ime;
        self.cpu.ime_enable_delay = slot_state.cpu.ime_enable_delay;
        self.cpu.halted = slot_state.cpu.halted;
        self.cpu.halt_bug = slot_state.cpu.halt_bug;
        self.cpu.cycles = slot_state.cpu.cycles;
        self.cpu.reader_ctx = undefined;

        self.applyBusState(slot_state.bus);

        self.ppu = slot_state.ppu;
        if (self.frontend) |*frontend| frontend.redraw(&self.ppu.frame_buffer);

        self.steps = slot_state.steps;
        self.paused = false;

        var buf: [32]u8 = undefined;
        const text = std.fmt.bufPrint(&buf, "LOADED SLOT {d}", .{slot}) catch "LOADED";
        self.setStatusMessage(text);
    }

    fn captureBusState(self: *const Emulator) BusState {
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
            },
            .ie_register = self.bus.ie_register,
            .timer = self.bus.timer,
            .serial = self.bus.serial,
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

    fn applyBusState(self: *Emulator, state: BusState) void {
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
        self.bus.io.serial_output.clearRetainingCapacity();

        self.bus.ie_register = state.ie_register;
        self.bus.timer = state.timer;
        self.bus.serial = state.serial;
        self.bus.dma = state.dma;

        self.bus.cartridge.mbc.restore(state.mbc);

        if (self.bus.cartridge.ram_data) |ram| {
            const len = @min(@min(ram.len, MAX_CART_RAM_BYTES), state.cart_ram_len);
            if (len > 0) @memcpy(ram[0..len], state.cart_ram[0..len]);
            if (ram.len > len) @memset(ram[len..], 0);
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
        self.ppu.reset();
        self.next_frame_deadline_ns = 0;
        self.steps = 0;
        self.paused = false;
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

    /// Print blargg-style test output from cartridge RAM ($A000+), if present.
    pub fn printCartRamTestOutput(self: *const Emulator) void {
        const ram = self.bus.cartridge.ram_data orelse return;
        if (ram.len < 5) return;
        if (!(ram[1] == 0xDE and ram[2] == 0xB0 and ram[3] == 0x61)) return;

        const status = ram[0];
        std.debug.print("\n=== Cart RAM Test Output ===\n", .{});
        if (status == 0x80) {
            std.debug.print("Status: running\n", .{});
        } else {
            std.debug.print("Status code: {d}\n", .{status});
        }

        std.debug.print("Text: ", .{});
        var i: usize = 4;
        while (i < ram.len and ram[i] != 0) : (i += 1) {
            const byte = ram[i];
            if (byte >= 0x20 and byte < 0x7F or byte == '\n' or byte == '\r') {
                std.debug.print("{c}", .{byte});
            } else {
                std.debug.print("[{X:0>2}]", .{byte});
            }
        }
        std.debug.print("\n", .{});
    }
};

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
