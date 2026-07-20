const std = @import("std");
const Allocator = std.mem.Allocator;
const Cartridge = @import("cartridge/cartridge.zig").Cartridge;
const machine_mod = @import("machine.zig");
const Machine = machine_mod.Machine;
const Snapshot = machine_mod.Snapshot;
pub const MooneyeResult = machine_mod.MooneyeResult;
const frontend_mod = @import("frontend/sdl_frontend.zig");
const SdlFrontend = frontend_mod.SdlFrontend;
const Debugger = @import("debugger.zig").Debugger;

pub const EmulatorOptions = struct {
    debug: bool = false,
    max_steps: ?usize = null,
    breakpoint: ?u16 = null,
    headless: bool = false,
    mooneye_test: bool = false,
};

const SAVE_SLOT_COUNT = 10;

/// Interactive application adapter around the deterministic `Machine` core.
/// Host clocks, SDL, pacing, pause state, and UI save slots belong here; none
/// of them can influence the frontend-free hardware scheduler.
pub const Emulator = struct {
    io: std.Io,
    machine: Machine,
    frontend: ?SdlFrontend,
    options: EmulatorOptions,

    running: bool = false,
    paused: bool = false,
    active_save_slot: u8 = 0,
    status_message: [48]u8 = [_]u8{0} ** 48,
    status_message_len: usize = 0,
    save_slots: [SAVE_SLOT_COUNT]?Snapshot = [_]?Snapshot{null} ** SAVE_SLOT_COUNT,
    last_ui_redraw_ns: i96 = 0,
    next_frame_deadline_ns: i96 = 0,
    fps_window_start_ns: i96 = 0,
    fps_window_frames: u32 = 0,
    fps_x100: u32 = 0,

    pub fn init(allocator: Allocator, io: std.Io, rom_path: []const u8, options: EmulatorOptions) !Emulator {
        var cartridge = try Cartridge.load(allocator, io, rom_path);
        errdefer cartridge.deinit();

        var frontend: ?SdlFrontend = null;
        if (!options.headless) {
            frontend = SdlFrontend.init() catch |err| blk: {
                std.debug.print("Warning: Failed to initialize SDL frontend: {any}\n", .{err});
                std.debug.print("Falling back to headless execution\n", .{});
                break :blk null;
            };
        }
        errdefer if (frontend) |*active| active.deinit();

        return .{
            .io = io,
            .machine = Machine.init(allocator, cartridge, .{
                .capture_audio = frontend != null,
            }),
            .frontend = frontend,
            .options = options,
        };
    }

    pub fn deinit(self: *Emulator) void {
        if (self.frontend) |*frontend| frontend.deinit();
        self.machine.deinit();
    }

    pub fn run(self: *Emulator) void {
        self.running = true;
        self.paused = false;

        // The automation path should be the core loop, not the interactive
        // loop with a forest of always-false SDL and pacing checks. Debug mode
        // intentionally retains the interactive loop's per-step reporting.
        if (self.frontend == null and !self.options.debug) {
            self.runHeadless();
            return;
        }

        self.last_ui_redraw_ns = std.Io.Clock.awake.now(self.io).nanoseconds;
        self.fps_window_start_ns = self.last_ui_redraw_ns;
        self.next_frame_deadline_ns = 0;
        if (self.status_message_len == 0) self.setStatusMessage("READY");

        if (self.options.debug) {
            self.machine.bus.cartridge.printInfo();
            std.debug.print("\n=== Starting Execution ===\n\nInitial state:\n", .{});
            self.printCpuState();
        }

        self.syncUiStatus();
        self.syncInspector();

        while (self.running) {
            // Polling SDL for every instruction is unnecessarily expensive.
            // Paused execution still polls continuously so controls stay live.
            const should_poll_frontend = self.paused or (self.machine.steps & 0xFF) == 0;
            const actions: frontend_mod.UiActions = if (should_poll_frontend and self.frontend != null)
                self.frontend.?.pollEvents(&self.machine.bus)
            else
                .{};
            if (actions.quit) break;
            if (should_poll_frontend) {
                self.handleUiActions(actions);
                self.syncUiStatus();
                if (actions.redraw) {
                    self.redrawFrontend();
                }
                self.maybeRedrawUi();
            }

            if (self.paused) {
                std.Io.sleep(self.io, .fromMilliseconds(8), .awake) catch {};
                continue;
            }

            if (self.options.max_steps) |max| {
                if (self.machine.steps >= max) {
                    if (self.options.debug) std.debug.print("\nReached max steps limit ({d})\n", .{max});
                    break;
                }
            }

            if (self.options.breakpoint) |bp| {
                if (self.machine.cpu.pc == bp) {
                    if (self.options.debug) std.debug.print("\nBreakpoint hit at 0x{X:0>4}\n", .{bp});
                    break;
                }
            }

            if (self.options.mooneye_test and self.mooneyeResult() != null) break;
            self.step();
        }
    }

    fn runHeadless(self: *Emulator) void {
        while (self.running) {
            if (self.options.max_steps) |max| {
                if (self.machine.steps >= max) return;
            }
            if (self.options.breakpoint) |bp| {
                if (self.machine.cpu.pc == bp) return;
            }
            if (self.options.mooneye_test and self.machine.mooneyeResult() != null) return;

            // Fixed-length workloads are common enough to deserve the tight
            // core loop. Conditional runs remain instruction-granular so
            // breakpoints and test completion never overshoot.
            if (self.options.breakpoint == null and !self.options.mooneye_test) {
                if (self.options.max_steps) |max| {
                    self.machine.runInstructions(max - self.machine.steps);
                    return;
                }
                self.machine.runInstructions(1_000_000);
            } else {
                _ = self.machine.step();
            }
        }
    }

    pub fn mooneyeResult(self: *const Emulator) ?MooneyeResult {
        return self.machine.mooneyeResult();
    }

    pub fn step(self: *Emulator) void {
        const pc_before = self.machine.cpu.pc;
        const result = self.machine.step();

        if (result.frame_ready) {
            if (self.frontend) |*frontend| {
                self.recordPresentedFrame();
                self.syncInspector();
                frontend.present(&self.machine.ppu.frame_buffer);
                self.paceFrame();
            }
        }

        if (self.frontend) |*frontend| {
            const pending_audio = self.machine.pendingAudio();
            if (frontend.audioBatchReady(pending_audio.len)) {
                frontend.queueAudio(pending_audio);
                self.machine.discardAudio();
            }
        }

        if (self.options.debug) {
            std.debug.print("\nStep {d} (PC=0x{X:0>4}, cycles={d}):\n", .{
                self.machine.steps,
                pc_before,
                result.cycles,
            });
            self.printCpuState();
        }
    }

    fn maybeRedrawUi(self: *Emulator) void {
        if (!self.paused) return;
        const frontend = if (self.frontend) |*active| active else return;
        const now = std.Io.Clock.awake.now(self.io).nanoseconds;
        if (now - self.last_ui_redraw_ns >= 16 * std.time.ns_per_ms) {
            self.syncInspector();
            frontend.redraw(&self.machine.ppu.frame_buffer);
            self.last_ui_redraw_ns = now;
        }
    }

    /// Pace against the DMG's 70,224-dot frame instead of assuming the host
    /// display refresh rate. Large stalls resynchronize instead of bursting.
    fn paceFrame(self: *Emulator) void {
        const frame_ns: i96 = (70_224 * std.time.ns_per_s) / 4_194_304;
        var now = std.Io.Clock.awake.now(self.io).nanoseconds;

        if (self.next_frame_deadline_ns == 0) {
            self.next_frame_deadline_ns = now + frame_ns;
            return;
        }

        if (now < self.next_frame_deadline_ns) {
            std.Io.sleep(self.io, .fromNanoseconds(self.next_frame_deadline_ns - now), .awake) catch {};
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
            self.setStatusMessage(std.fmt.bufPrint(&buf, "SLOT {d}", .{self.active_save_slot}) catch "SLOT ?");
        }
        if (actions.next_slot) {
            const slot = (@as(usize, self.active_save_slot) + 1) % SAVE_SLOT_COUNT;
            self.active_save_slot = @intCast(slot);
            var buf: [24]u8 = undefined;
            self.setStatusMessage(std.fmt.bufPrint(&buf, "SLOT {d}", .{self.active_save_slot}) catch "SLOT ?");
        }
        if (actions.toggle_pause) {
            self.paused = !self.paused;
            self.setStatusMessage(if (self.paused) "PAUSED" else "RUNNING");
        }
        if (actions.step_instruction and self.paused) self.stepPausedInstruction();
        if (actions.toggle_mute) {
            if (self.frontend) |*frontend| {
                const muted = frontend.toggleAudioMute();
                self.setStatusMessage(if (muted) "AUDIO MUTED" else "AUDIO ON");
            }
        }
        if (actions.reset) {
            self.reset();
            self.running = true;
            self.paused = false;
            self.setStatusMessage("RESET");
        }
        if (actions.save_state) self.saveStateToSlot(self.active_save_slot);
        if (actions.load_state) self.loadStateFromSlot(self.active_save_slot);
    }

    fn syncUiStatus(self: *Emulator) void {
        const frontend = if (self.frontend) |*active| active else return;
        frontend.setUiStatus(
            self.paused,
            self.active_save_slot,
            self.save_slots[self.active_save_slot] != null,
            self.status_message[0..self.status_message_len],
        );
    }

    fn syncInspector(self: *Emulator) void {
        const frontend = if (self.frontend) |*active| active else return;
        if (!frontend.debuggerVisible()) return;
        const cpu = &self.machine.cpu;
        const mapper = self.machine.inspectCartridge().mapper;
        frontend.setInspector(.{
            .pc = cpu.pc,
            .sp = cpu.sp,
            .af = cpu.af,
            .bc = cpu.bc,
            .de = cpu.de,
            .hl = cpu.hl,
            .cycles = cpu.cycles,
            .frames = self.machine.frames,
            .upper_rom_bank = mapper.upper_rom_bank,
            .effective_ram_bank = mapper.effective_ram_bank,
            .fps_x100 = self.fps_x100,
            .mnemonic = Debugger.disassembleAt(&self.machine, cpu.pc).mnemonic,
        });
    }

    fn redrawFrontend(self: *Emulator) void {
        self.syncInspector();
        if (self.frontend) |*frontend| frontend.present(&self.machine.ppu.frame_buffer);
    }

    fn stepPausedInstruction(self: *Emulator) void {
        _ = self.machine.step();
        self.setStatusMessage("STEP");
        self.redrawFrontend();
    }

    fn recordPresentedFrame(self: *Emulator) void {
        const now = std.Io.Clock.awake.now(self.io).nanoseconds;
        if (self.fps_window_start_ns == 0) {
            self.fps_window_start_ns = now;
            self.fps_window_frames = 0;
            return;
        }
        self.fps_window_frames += 1;
        const elapsed = now - self.fps_window_start_ns;
        if (elapsed < 500 * std.time.ns_per_ms) return;
        self.fps_x100 = @intCast(@divTrunc(
            @as(i128, self.fps_window_frames) * 100 * std.time.ns_per_s,
            elapsed,
        ));
        self.fps_window_frames = 0;
        self.fps_window_start_ns = now;
    }

    fn setStatusMessage(self: *Emulator, message: []const u8) void {
        self.status_message_len = @min(message.len, self.status_message.len);
        for (message[0..self.status_message_len], 0..) |byte, index| {
            self.status_message[index] = std.ascii.toUpper(byte);
        }
    }

    fn saveStateToSlot(self: *Emulator, slot: u8) void {
        self.save_slots[slot] = self.machine.capture();
        var buf: [32]u8 = undefined;
        self.setStatusMessage(std.fmt.bufPrint(&buf, "SAVED SLOT {d}", .{slot}) catch "SAVED");
    }

    fn loadStateFromSlot(self: *Emulator, slot: u8) void {
        const state = self.save_slots[slot] orelse {
            self.setStatusMessage("EMPTY SLOT");
            return;
        };

        self.machine.restore(state);
        if (self.frontend) |*frontend| {
            frontend.clearAudioQueue();
            self.syncInspector();
            frontend.redraw(&self.machine.ppu.frame_buffer);
        }
        self.paused = false;

        var buf: [32]u8 = undefined;
        self.setStatusMessage(std.fmt.bufPrint(&buf, "LOADED SLOT {d}", .{slot}) catch "LOADED");
    }

    pub fn stop(self: *Emulator) void {
        self.running = false;
    }

    pub fn reset(self: *Emulator) void {
        self.machine.reset();
        if (self.frontend) |*frontend| frontend.clearAudioQueue();
        self.next_frame_deadline_ns = 0;
        self.fps_window_start_ns = 0;
        self.fps_window_frames = 0;
        self.fps_x100 = 0;
        self.paused = false;
        self.running = false;
    }

    fn printCpuState(self: *const Emulator) void {
        const cpu = &self.machine.cpu;
        std.debug.print("A: 0x{X:0>2} F: 0x{X:0>2} B: 0x{X:0>2} C: 0x{X:0>2} D: 0x{X:0>2} E: 0x{X:0>2} H: 0x{X:0>2} L: 0x{X:0>2}\n", .{
            cpu.a(), cpu.f().toU8(), cpu.b(), cpu.c(), cpu.d(), cpu.e(), cpu.h(), cpu.l(),
        });
        std.debug.print("SP: 0x{X:0>4} PC: 0x{X:0>4} IME: {s} Cycles: {d}\n", .{
            cpu.sp,
            cpu.pc,
            if (cpu.ime) "ON" else "OFF",
            cpu.cycles,
        });
    }

    pub fn getCartridgeInfo(self: *const Emulator) *const Cartridge {
        return &self.machine.bus.cartridge;
    }

    pub fn printSerialOutput(self: *const Emulator) void {
        const output = self.machine.bus.getSerialOutput();
        if (output.len == 0) return;

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

    pub fn printCartRamTestOutput(self: *const Emulator) void {
        const ram = self.machine.bus.cartridge.ram_data orelse return;
        if (ram.len < 5 or !(ram[1] == 0xDE and ram[2] == 0xB0 and ram[3] == 0x61)) return;

        std.debug.print("\n=== Cart RAM Test Output ===\n", .{});
        if (ram[0] == 0x80) {
            std.debug.print("Status: running\n", .{});
        } else {
            std.debug.print("Status code: {d}\n", .{ram[0]});
        }

        std.debug.print("Text: ", .{});
        var index: usize = 4;
        while (index < ram.len and ram[index] != 0) : (index += 1) {
            const byte = ram[index];
            if (byte >= 0x20 and byte < 0x7F or byte == '\n' or byte == '\r') {
                std.debug.print("{c}", .{byte});
            } else {
                std.debug.print("[{X:0>2}]", .{byte});
            }
        }
        std.debug.print("\n", .{});
    }
};
