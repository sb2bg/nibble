# nibble

**A Nintendo Game Boy (DMG) emulator and deterministic simulation core, written in Zig.**

![Zig](https://img.shields.io/badge/Zig-0.16.x-f7a41d?logo=zig&logoColor=white)
![Platform](https://img.shields.io/badge/platform-macOS%20%7C%20Linux-lightgrey)
![Hardware](https://img.shields.io/badge/hardware-DMG-8bac0f)
![Status](https://img.shields.io/badge/status-in%20development-blue)

nibble runs commercial and test ROMs through a dot-driven PPU, a cycle-driven CPU
bus, and a four-channel APU. Its hardware core is frontend-free. The
same `Machine` that drives the SDL window can be embedded, snapshotted, forked,
and stepped in parallel with no SDL, host-clock, or filesystem dependency. That
makes it usable both as a normal emulator and as a deterministic environment for
planning, replay, fuzzing, and reinforcement-learning workloads.

> **Status:** actively in development. The DMG core passes the [validation baseline](#validation-baseline) below. CGB hardware is not implemented.

---

## Contents

- [Features](#features)
- [Quick start](#quick-start)
- [Usage](#usage)
- [Embedding the core](#embedding-the-core)
- [Benchmarks](#benchmarks)
- [Validation baseline](#validation-baseline)
- [Project layout](#project-layout)
- [Documentation](#documentation)
- [Notes and licensing](#notes-and-licensing)

## Features

### Hardware (DMG)

- CPU instruction decode/execute loop with interrupt handling
- Memory bus with cartridge support and MBC banking (`ROM`, `MBC1`, `MBC2`, `MBC3`, `MBC5`)
- Timer (`DIV`/`TIMA`/`TMA`/`TAC`) driven by the hidden system counter
- Dot-driven PPU with background/window and object pixel FIFOs, plus cancelable
  phased object fetches that share the background fetch pipeline
- Timed OAM DMA with CPU bus lockout
- OAM corruption, including row-latch effects from 16-bit IDU operations
- Four-channel APU with sweep, envelopes, length counters, wave RAM, and
  divider-driven frame sequencing
- Resistor-network stereo mixing with measured-rate high-pass filtering
- Cycle-driven serial transfers with completion interrupts
- Oscillator-gated `STOP`, including DIV reset and selected-joypad wake
- Deterministic MBC3 real-time clock registers and latching
- Joypad input mapping with joypad interrupt signaling

### Frontend

- SDL2 window output, with automatic headless fallback if SDL init fails
- Resizable, high-DPI presentation with frame pacing, palette themes, and fullscreen
- Buffered 48 kHz stereo audio with pause-safe queueing and mute control
- Optional detached debugger window: antialiased text, CPU state, decoded
  instruction, mapper banks, emulated time, host FPS, and paused stepping
- Management hotkeys for pause, reset, save/load state, and slot selection
- In-memory save states (10 slots per run session)

### Simulation core

- Frontend-free deterministic `Machine` API for embedding and automation
- Bounded frame stepping, explicit button input, and observable state digests
- Complete in-memory machine snapshots, compact owned snapshots, and forks
- Non-intrusive memory peeks and structured live cartridge/mapper inspection
- Host-driven external serial clock/data edges for deterministic link partners
- Headless mode and serial output capture for test-ROM workflows

## Quick start

### Requirements

| Requirement                | Notes                                         |
| -------------------------- | --------------------------------------------- |
| Zig `0.16.x`               | Project minimum is `0.16.0` (`build.zig.zon`) |
| SDL2 development libraries | Only needed for the graphical frontend        |
| `pkg-config`               | Used by the Zig build to locate SDL2          |

```bash
# macOS
brew install sdl2 pkg-config

# Debian / Ubuntu
sudo apt install libsdl2-dev pkg-config

# Arch
sudo pacman -S sdl2 pkgconf
```

Zig `0.16.x` is best installed from [ziglang.org/download](https://ziglang.org/download/),
since distribution packages often lag behind.

### Build

```bash
zig build
```

This produces the executable at `zig-out/bin/nibble`.

### Run

> ROMs are **not** vendored — `roms/` is git-ignored. Point the commands below at
> your own dumps and test-ROM builds.

```bash
# graphical mode (SDL window)
zig build run -- "roms/Dr. Mario (World).gb"

# headless mode (useful for test ROMs / CI)
zig build run -- --headless roms/blargg/cpu_instrs/cpu_instrs.gb

# limit execution to N instructions
zig build run -- --headless -s 100000 roms/blargg/cpu_instrs/cpu_instrs.gb

# debug trace mode
zig build run -- -d -s 1000 roms/blargg/cpu_instrs/cpu_instrs.gb

# run a Mooneye acceptance ROM and return a test-friendly exit status
zig build run -- --mooneye-test path/to/acceptance/timer/div_write.gb
```

## Usage

### CLI options

| Option                     | Description                                                                                                            |
| -------------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| `-h`, `--help`             | Show help                                                                                                              |
| `-d`, `--debug`            | Verbose step-by-step debug output                                                                                      |
| `-s`, `--steps <COUNT>`    | Stop after a maximum number of steps                                                                                   |
| `-b`, `--breakpoint <HEX>` | Stop when `PC == HEX` (with or without a `0x` prefix)                                                                  |
| `--headless`               | Run without graphics                                                                                                   |
| `--mooneye-test`           | Run headlessly until Mooneye reports pass/fail (exit `0`/`1`, or `2` after the default 10-million-instruction timeout) |

### Controls

| Button | Keys                                |
| ------ | ----------------------------------- |
| D-pad  | Arrow keys                          |
| A      | `X` or `A`                          |
| B      | `Z` or `S`                          |
| Start  | `Enter`, keypad `Enter`, or `Space` |
| Select | `Backspace` or `Tab`                |

### Management hotkeys (SDL mode)

| Key         | Action                                                     |
| ----------- | ---------------------------------------------------------- |
| `F1`        | Open/hide the detached debugger window (hidden by default) |
| `P`         | Pause/resume emulation                                     |
| `F10`       | Execute one instruction while paused                       |
| `R`         | Reset emulator                                             |
| `F5` / `F9` | Save / load state at the active slot                       |
| `[` / `]`   | Previous / next save slot                                  |
| `C`         | Cycle Classic, Pocket, Mono, and Amber palettes            |
| `M`         | Mute/unmute audio                                          |
| `F11`       | Toggle fullscreen                                          |
| `Esc`       | Quit                                                       |

The debugger window shows run/pause state, the next instruction, CPU registers,
active ROM and RAM banks, frame/dot counters, and the measured presentation rate.
It starts hidden, and closing it hides the debugger without quitting emulation.
The window title shows run/pause state, palette, audio state, active slot, and the
last status message.

Save states are currently **in-memory only** — session-local, not persisted to
disk. Ten slots (`0-9`) are available, managed with `[` and `]`.

## Embedding the core

The public `nibble` module exports `Machine`, `Cartridge`, `Snapshot`, `Buttons`,
and mapper inspection types. `Machine` has no SDL, host-clock, or filesystem
dependency: load or construct a cartridge, step the machine, and consume only the
outputs the caller needs.

```zig
const std = @import("std");
const nibble = @import("nibble");

pub fn main(init: std.process.Init) !void {
    const cartridge = try nibble.Cartridge.load(init.gpa, init.io, "game.gb");

    var machine = nibble.Machine.init(init.gpa, cartridge, .{
        .capture_audio = false,
        .capture_video = true,
    });
    defer machine.deinit();

    // Advance to a decision point without paying for framebuffer stores.
    _ = machine.stepFrames(600, .{ .video = .none });
    const checkpoint = machine.capture();

    // Branch A: hold A for eight frames.
    machine.setButtons(.{ .a = true });
    _ = machine.stepFrames(8, .{ .video = .final_frame });
    const a_digest = machine.observableDigest();

    // Branch B: same state, different action — reproducible bit for bit.
    machine.restore(checkpoint);
    machine.setButtons(.{ .left = true });
    _ = machine.stepFrames(8, .{ .video = .final_frame });
    const b_digest = machine.observableDigest();

    const frame = machine.observe().frame_buffer;
    std.debug.print("a={x} b={x} pixels={d}\n", .{ a_digest, b_digest, frame.len });
}
```

### Automation surface

**Execution**

- `step`, bounded `runUntilFrame`, and allocation-free `stepFrames`
- Per-run video policies: every frame, final frame only, or timing-only execution
  with no framebuffer stores
- `runUntilCycle` and `CycleInput` for transitions on exact emulated T-cycles,
  including transitions inside a CPU instruction

**Input**

- `setButtons` and frame-boundary `FrameInput` timelines with explicit,
  host-independent input state
- `serialOutgoingBit` and `clockSerialExternal` for caller-owned link cables,
  printers, adapters, and protocol test harnesses

**Observation**

- `observe` for borrowed CPU, RAM, VRAM, OAM, tile-map, and framebuffer views
- `peek` for observations that do not advance time or trigger CPU bus effects
- `observableDigest` for regression and replay identity
- `inspectCartridge` for live mapper banks, RAM enable state, and MBC3 RTC state

**Branching and determinism**

- Deterministic power-on RTC seeds and `resetDeterministic` for reproducible
  episodes that optionally clear battery-backed cartridge RAM
- Allocation-free `capture`/`restore`, compact `captureOwned`/`restoreOwned`, and
  `fork` for deterministic branches and replay
- `MachineBatch` for parallel instruction, bounded-frame, or multi-frame
  observation-selective advancement, machine-ordered heterogeneous actions,
  action repeat, and deterministic batch resets
- `agent.AgentRuntime` for generation-safe preallocated branch slots, temporal
  hold/release actions, injected `std.Io` scheduling, and allocation-free
  contiguous `palette_u8` or packed 2bpp model observations

### Debugger

`Debugger` is an opt-in research wrapper around `Machine`. It provides
fixed-capacity PC breakpoints, instruction-boundary value watchpoints, mapper-bank
transition events, frame events, a bounded history ring, and non-intrusive
disassembly. Because it drives `Machine.step` from the outside, normal headless
execution contains no debugger callback or trace branch — useful for investigating
cartridge behavior without taxing training runs.

### Ownership and timing notes

Forks retain one atomically reference-counted immutable ROM allocation while
owning independent hardware and cartridge-RAM state. Forking copies hardware
directly instead of materializing the fixed 128 KiB cartridge-RAM reserve.
`OwnedSnapshot` similarly allocates only the cartridge RAM present in the loaded
cartridge; the larger value `Snapshot` remains available for callers that require
allocation-free capture and restore.

`Machine.step` returns zero cycles while the CPU is in `STOP`, because the
oscillator and peripherals are not advancing. A selected joypad transition applied
through `setButtons` wakes it, and the next step performs the wake M-cycle.
`runUntilCycle` returns `error.Stopped` if no current-cycle input wakes the
machine. External serial clock pulses are likewise explicit host events rather
than synthetic elapsed time.

## Benchmarks

### Simulation core

`nibble-bench` measures the simulation core directly: it does not initialize SDL,
pace frames, or mix host PCM samples. Every trial restores the same machine
snapshot and verifies that it ends with the same observable-state digest. It also
reports owned snapshot capture/restore rates, deterministic machine forks per
second, and aggregate multicore throughput through Zig 0.16's `std.Io.Group`
concurrency.

```bash
zig build bench -Doptimize=ReleaseFast -- \
  --steps 10000000 --warmup 1000000 --trials 5 \
  "roms/Tetris (JUE) (V1.1) [!].gb"
```

| Flag                      | Description                                    |
| ------------------------- | ---------------------------------------------- |
| `-s`, `--steps <COUNT>`   | Instructions measured per trial                |
| `-w`, `--warmup <COUNT>`  | Warmup instructions before measurement         |
| `-t`, `--trials <TRIALS>` | Trial count (1-21)                             |
| `--no-video`              | Preserve PPU timing without framebuffer stores |

The report includes instruction and T-cycle throughput, real-time factor,
completed frames per second, and the deterministic digest — plus the active
observation policy. Use the same ROM, step count, Zig version, and host when
comparing changes.

### Agent workload

The complete agent workload has its own benchmark, covering macro-actions,
preallocated checkpoint branches, parallel stepping, contiguous raw or packed
observations, a deterministic policy-boundary shim, and allocation-free branch
resets.

```bash
zig build agent-bench -Doptimize=ReleaseFast -- \
  --environments 128 --iterations 100 --warmup 4 \
  "roms/Tetris (JUE) (V1.1) [!].gb"
```

| Flag                           | Description                             |
| ------------------------------ | --------------------------------------- |
| `-e`, `--environments <COUNT>` | Preallocated environment count          |
| `-i`, `--iterations <COUNT>`   | Measured policy steps per action repeat |
| `-w`, `--warmup <COUNT>`       | Warmup policy steps per action repeat   |
| `--workers <COUNT>`            | Explicit `std.Io` worker count          |
| `--encoding <FORMAT>`          | `packed`, `raw`, or `none`              |

## Validation baseline

```bash
zig build test
```

Test and reference ROMs live under `roms/` (for example `roms/blargg/` and
`roms/scribbltests/`). Mooneye binaries are downloaded separately rather than
vendored; its documented register protocol is supported directly by
`--mooneye-test`.

The current DMG baseline passes:

- all unit tests in Debug and ReleaseSafe builds
- 62 applicable Mooneye acceptance ROMs (excluding boot-state and non-DMG-model variants)
- all 11 `blargg/cpu_instrs` cases
- `blargg/instr_timing` and both three-part memory-timing suites
- all 8 `blargg/oam_bug` cases
- all 12 individual `blargg/dmg_sound` cases

The `cgb_sound` suite is outside the baseline because CGB hardware is not implemented.

## Project layout

```
src/
├── main.zig              CLI entrypoint
├── nibble.zig            public frontend-free package surface
├── machine.zig           deterministic hardware scheduler and snapshots
├── batch.zig             parallel multi-machine advancement
├── emulator.zig          SDL/host adapter around Machine
├── debugger.zig          opt-in research wrapper around Machine
├── benchmark.zig         reproducible headless benchmark executable
├── agent_benchmark.zig   complete agent-workload benchmark executable
├── agent/                temporal actions, model observations, branch pool,
│                         and parallel agent runtime
├── cpu/                  CPU core + instruction decode/execute
├── memory/               memory bus, IO registers, and MBC logic
├── ppu/                  PPU timing and rendering
├── cartridge/            ROM header parsing and cartridge ownership
├── frontend/             optional host presentation and input adapters
├── apu.zig               audio registers, channel timing, mixing, PCM handoff
├── timer.zig             timer/divider logic
├── serial.zig            serial transfer engine and clock sources
├── sdl.zig               minimal SDL2 bindings
└── *_cli.zig             argument parsing for each executable
roms/                     local ROMs used for development/testing (git-ignored)
docs/                     architecture and runtime documentation
```

## Documentation

| Document                                               | Contents                                                                                                      |
| ------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------- |
| [`docs/ARCHITECTURE.md`](docs/ARCHITECTURE.md)         | Component ownership, timing model, and accuracy limits                                                        |
| [`docs/RESEARCH_RUNTIME.md`](docs/RESEARCH_RUNTIME.md) | Planning/training niche, measured performance, accuracy tradeoffs, and a counterfactual-search demo direction |
| [`docs/AGENT_RUNTIME.md`](docs/AGENT_RUNTIME.md)       | Agent API, buffer formats, M1 Pro crossover measurements, and the planned local PyTorch/MLX worker boundary   |

## Notes and licensing

This project is for educational and development purposes. Use only ROMs you are
legally allowed to run.

The optional SDL debugger embeds Inter under the SIL Open Font License and uses
`stb_truetype` under its MIT/public-domain dual license. `zig build` installs
their complete license texts under `share/nibble/licenses/`.
