const std = @import("std");
const Allocator = std.mem.Allocator;
const Cpu = @import("cpu/cpu.zig").Cpu;
const Bus = @import("memory/bus.zig").Bus;
const Cartridge = @import("cartridge/cartridge.zig").Cartridge;
const Timer = @import("timer.zig").Timer;
const ppu_mod = @import("ppu/ppu.zig");
const Ppu = ppu_mod.Ppu;
const PpuMode = ppu_mod.PpuMode;
const DmgColor = ppu_mod.DmgColor;
const SCREEN_WIDTH = ppu_mod.SCREEN_WIDTH;
const SCREEN_HEIGHT = ppu_mod.SCREEN_HEIGHT;

pub const EmulatorOptions = struct {
    debug: bool = false,
    max_steps: ?usize = null, // null means run indefinitely
    breakpoint: ?u16 = null,
    headless: bool = false, // Run without graphics (for testing)
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

const TimerState = struct {
    div_counter: u16,
    tima_counter: u16,
    prev_and_result: bool,
};

const IoState = struct {
    data: [0x80]u8,
    div_counter: u16,
    joypad_select: u8,
    joypad_buttons: u8,
    dma_active: bool,
    dma_source: u16,
    dma_offset: u8,
    dma_delay: u8,
    oam_scan_row: u8,
};

const MbcState = struct {
    rom_bank: u16,
    ram_bank: u8,
    ram_enabled: bool,
    banking_mode: u1,
};

const BusState = struct {
    wram: [0x2000]u8,
    hram: [0x7F]u8,
    oam: [0xA0]u8,
    vram: [0x2000]u8,
    io: IoState,
    ie_register: u8,
    mbc: MbcState,
    cart_ram_len: usize,
    cart_ram: [MAX_CART_RAM_BYTES]u8,
};

const PpuState = struct {
    frame_buffer: [SCREEN_HEIGHT][SCREEN_WIDTH]DmgColor,
    mode: PpuMode,
    mode_cycles: u32,
    ly: u8,
    enabled: bool,
};

const SaveState = struct {
    cpu: CpuState,
    timer: TimerState,
    bus: BusState,
    ppu: ?PpuState,
    steps: usize,
};

pub const Emulator = struct {
    allocator: Allocator,
    cpu: Cpu,
    bus: Bus,
    timer: Timer,
    ppu: ?Ppu,
    options: EmulatorOptions,

    // Runtime state
    steps: usize = 0,
    running: bool = false,
    paused: bool = false,
    ui_show_panel: bool = true,
    active_save_slot: u8 = 0,
    status_message: [48]u8 = [_]u8{0} ** 48,
    status_message_len: usize = 0,
    save_slots: [SAVE_SLOT_COUNT]?SaveState = [_]?SaveState{null} ** SAVE_SLOT_COUNT,
    last_ui_redraw_ns: i128 = 0,

    /// Initialize the emulator with a ROM file
    pub fn init(allocator: Allocator, rom_path: []const u8, options: EmulatorOptions) !Emulator {
        // Load the cartridge
        var cartridge = try Cartridge.load(allocator, rom_path);
        errdefer cartridge.deinit();

        // Initialize PPU. In headless mode (or if SDL fails), keep a logic-only
        // PPU active so LY/STAT/VBlank timing still advances.
        const ppu: ?Ppu = if (options.headless)
            Ppu.initHeadless()
        else
            Ppu.init() catch |err| blk: {
                std.debug.print("Warning: Failed to initialize SDL PPU: {any}\n", .{err});
                std.debug.print("Falling back to headless PPU timing\n", .{});
                break :blk Ppu.initHeadless();
            };
        errdefer if (ppu) |*p| p.deinit();

        return Emulator{
            .allocator = allocator,
            .cpu = Cpu.init(),
            .bus = Bus.init(allocator, cartridge),
            .timer = Timer.init(),
            .ppu = ppu,
            .options = options,
            .steps = 0,
            .running = false,
        };
    }

    pub fn deinit(self: *Emulator) void {
        if (self.ppu) |*p| p.deinit();
        self.bus.deinit();
    }

    /// Run the emulator's main loop
    pub fn run(self: *Emulator) void {
        self.running = true;
        self.paused = false;
        self.last_ui_redraw_ns = std.time.nanoTimestamp();
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
        if (self.ppu) |*ppu| {
            const lcdc = self.bus.io.getLcdc();
            ppu.setEnabled((lcdc & 0x80) != 0);
        }
        self.syncUiPanelState();

        while (self.running) {
            // Check UI events and hotkeys
            if (self.ppu) |*ppu| {
                const actions = ppu.pollEvents(&self.bus);
                if (actions.quit) break;
                self.handleUiActions(actions);
            }
            self.syncUiPanelState();
            self.maybeRedrawUi();

            if (self.paused) {
                std.Thread.sleep(8 * std.time.ns_per_ms);
                continue;
            }

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

        if (self.ppu) |*ppu| {
            // Update PPU enabled state from LCDC
            const lcdc = self.bus.io.getLcdc();
            ppu.setEnabled((lcdc & 0x80) != 0);

            if ((lcdc & 0x80) == 0) {
                // LY reads as 0 while LCD is disabled.
                self.bus.io.setLy(0);
            }

            ppu.tick(cycles, &self.bus);
        }

        self.timer.tick(cycles, &self.bus.io);
    }

    fn maybeRedrawUi(self: *Emulator) void {
        if (self.ppu) |*ppu| {
            if (!ppu.sdl_initialized) return;
            const now = std.time.nanoTimestamp();
            if (now - self.last_ui_redraw_ns >= 16 * std.time.ns_per_ms) {
                ppu.redraw();
                self.last_ui_redraw_ns = now;
            }
        }
    }

    fn handleUiActions(self: *Emulator, actions: ppu_mod.UiActions) void {
        if (actions.toggle_panel) {
            self.ui_show_panel = !self.ui_show_panel;
            self.setStatusMessage(if (self.ui_show_panel) "PANEL ON" else "PANEL OFF");
        }
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

    fn syncUiPanelState(self: *Emulator) void {
        if (self.ppu) |*ppu| {
            const slot_has_state = self.save_slots[self.active_save_slot] != null;
            ppu.setUiPanelState(
                self.paused,
                self.active_save_slot,
                slot_has_state,
                self.bus.io.getJoypadState(),
                self.ui_show_panel,
                self.status_message[0..self.status_message_len],
            );
        }
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
            .timer = .{
                .div_counter = self.timer.div_counter,
                .tima_counter = self.timer.tima_counter,
                .prev_and_result = self.timer.prev_and_result,
            },
            .bus = self.captureBusState(),
            .ppu = if (self.ppu) |*ppu| PpuState{
                .frame_buffer = ppu.frame_buffer,
                .mode = ppu.mode,
                .mode_cycles = ppu.mode_cycles,
                .ly = ppu.ly,
                .enabled = ppu.enabled,
            } else null,
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

        self.timer.div_counter = slot_state.timer.div_counter;
        self.timer.tima_counter = slot_state.timer.tima_counter;
        self.timer.prev_and_result = slot_state.timer.prev_and_result;

        self.applyBusState(slot_state.bus);

        if (self.ppu) |*ppu| {
            if (slot_state.ppu) |ppu_state| {
                ppu.frame_buffer = ppu_state.frame_buffer;
                ppu.mode = ppu_state.mode;
                ppu.mode_cycles = ppu_state.mode_cycles;
                ppu.ly = ppu_state.ly;
                ppu.enabled = ppu_state.enabled;
            }
            ppu.redraw();
        }

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
                .div_counter = self.bus.io.div_counter,
                .joypad_select = self.bus.io.joypad_select,
                .joypad_buttons = self.bus.io.joypad_buttons,
                .dma_active = self.bus.io.dma_active,
                .dma_source = self.bus.io.dma_source,
                .dma_offset = self.bus.io.dma_offset,
                .dma_delay = self.bus.io.dma_delay,
                .oam_scan_row = self.bus.io.oam_scan_row,
            },
            .ie_register = self.bus.ie_register,
            .mbc = .{
                .rom_bank = self.bus.cartridge.mbc.rom_bank,
                .ram_bank = self.bus.cartridge.mbc.ram_bank,
                .ram_enabled = self.bus.cartridge.mbc.ram_enabled,
                .banking_mode = self.bus.cartridge.mbc.banking_mode,
            },
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
        self.bus.io.div_counter = state.io.div_counter;
        self.bus.io.joypad_select = state.io.joypad_select;
        self.bus.io.joypad_buttons = state.io.joypad_buttons;
        self.bus.io.dma_active = state.io.dma_active;
        self.bus.io.dma_source = state.io.dma_source;
        self.bus.io.dma_offset = state.io.dma_offset;
        self.bus.io.dma_delay = state.io.dma_delay;
        self.bus.io.oam_scan_row = state.io.oam_scan_row;
        self.bus.io.serial_output.clearRetainingCapacity();

        self.bus.ie_register = state.ie_register;

        self.bus.cartridge.mbc.rom_bank = state.mbc.rom_bank;
        self.bus.cartridge.mbc.ram_bank = state.mbc.ram_bank;
        self.bus.cartridge.mbc.ram_enabled = state.mbc.ram_enabled;
        self.bus.cartridge.mbc.banking_mode = state.mbc.banking_mode;

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
        self.timer.reset();
        if (self.ppu) |*ppu| ppu.reset();
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
