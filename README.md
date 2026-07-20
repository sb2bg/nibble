# nibble

A Nintendo Game Boy (DMG) emulator written in Zig.

## Current status

`nibble` is actively in-progress but already runs many ROMs and test ROMs.

Implemented core pieces:
- Frontend-free deterministic `Machine` API for embedding and automation
- Bounded frame stepping, explicit button input, observable state digests, and
  complete in-memory machine snapshots
- Non-intrusive memory peeks and structured live cartridge/mapper inspection
- CPU instruction decode/execute loop with interrupt handling
- Memory bus with cartridge support and MBC banking (`ROM`, `MBC1`, `MBC2`, `MBC3`, `MBC5`)
- Timer (`DIV/TIMA/TMA/TAC`)
- Timed DMG OAM DMA and CPU bus lockout
- DMG OAM corruption, including row-latch effects from 16-bit IDU operations
- Dot-driven PPU timing with background/window and object pixel FIFOs
- Deterministic MBC3 real-time clock registers and latching
- SDL2 window output (with automatic headless fallback if SDL init fails)
- Joypad input mapping + joypad interrupt signaling
- Resizable, high-DPI frontend with frame pacing, palette themes, and fullscreen
- Optional detached debugger window with antialiased text, CPU state, decoded
  instruction, mapper banks, emulated time, host FPS, and paused stepping
- Emulator management hotkeys (pause, reset, save/load state, slot selection)
- In-memory save states (10 slots per run session)
- Headless mode and serial output capture for test ROM workflows
- Cycle-driven DMG serial transfers with completion interrupts
- Four-channel DMG APU with sweep, envelopes, length counters, wave RAM, and
  divider-driven frame sequencing
- Buffered 48 kHz stereo SDL audio with pause-safe queueing and mute control

Known gaps:
- `STOP` instruction behavior is only partially modeled
- Object fetch cancellation and fetcher arbitration are still approximate
- External-clock serial transfers have no link-partner implementation
- The analog audio path uses a practical linear mixer and high-pass filter;
  individual DMG board revisions have additional nonlinear characteristics

## Requirements

- Zig `0.16.x` (project minimum in `build.zig.zon` is `0.16.0`)
- `SDL2` development libraries
- `pkg-config` (used by Zig build to find SDL2)

## Build

```bash
zig build
```

This produces the executable at `zig-out/bin/nibble`.

## Run

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

## Headless benchmark

`nibble-bench` measures the simulation core directly: it does not initialize
SDL, pace frames, or mix host PCM samples. Every trial restores the same machine
snapshot and verifies that it ends with the same observable-state digest. It
also reports owned snapshot capture/restore rates, deterministic machine forks
per second, and aggregate multicore throughput through Zig 0.16's
`std.Io.Group` concurrency.

```bash
zig build bench -Doptimize=ReleaseFast -- \
  --steps 10000000 --warmup 1000000 --trials 5 \
  "roms/Tetris (JUE) (V1.1) [!].gb"
```

The report includes instruction and T-cycle throughput, real-time factor,
completed frames per second, and the deterministic digest. Use the same ROM,
step count, Zig version, and host when comparing changes.
Pass `--no-video` to benchmark timing-only PPU execution without framebuffer
stores; the report always prints the active observation policy.

The [research runtime notes](docs/RESEARCH_RUNTIME.md) explain the intended
planning/training niche, current measured performance, accuracy tradeoffs, and
a substantive counterfactual-search demo direction.

The complete agent workload has its own benchmark:

```bash
zig build agent-bench -Doptimize=ReleaseFast -- \
  --environments 128 --iterations 100 --warmup 4 \
  "roms/Tetris (JUE) (V1.1) [!].gb"
```

It measures macro-actions, preallocated checkpoint branches, parallel stepping,
contiguous raw or packed observations, a deterministic policy-boundary shim,
and allocation-free branch resets. See [agent runtime and local visual
models](docs/AGENT_RUNTIME.md) for the API, buffer formats, current M1 Pro
crossover measurements, and the planned local PyTorch/MLX worker boundary.

## Embedding the core

The public `nibble` module exports `Machine`, `Cartridge`, `Snapshot`, `Buttons`,
and mapper inspection types. `Machine` has no SDL, host-clock, or filesystem
dependency: load or construct a cartridge, step the machine, and consume only
the outputs the caller needs.

Important automation operations include:

- `step`, bounded `runUntilFrame`, and allocation-free `stepFrames` execution;
- per-run video policies for every frame, the final frame only, or timing-only
  execution with no framebuffer stores;
- `observe` for borrowed CPU, RAM, VRAM, OAM, tile-map, and framebuffer views;
- `setButtons` and frame-boundary `FrameInput` timelines with explicit,
  host-independent input state;
- `runUntilCycle` and `CycleInput` for transitions on exact emulated T-cycles,
  including transitions inside a CPU instruction;
- deterministic power-on RTC seeds and `resetDeterministic` for reproducible
  episodes that optionally clear battery-backed cartridge RAM;
- allocation-free `capture`/`restore`, compact `captureOwned`/`restoreOwned`,
  and `fork` for deterministic branches and replay;
- `MachineBatch` for parallel instruction, bounded-frame, or multi-frame
  observation-selective advancement, machine-ordered heterogeneous actions,
  action repeat, and deterministic batch resets;
- `agent.AgentRuntime` for generation-safe preallocated branch slots, temporal
  hold/release actions, injected `std.Io` scheduling, and allocation-free
  contiguous `palette_u8` or packed 2bpp model observations;
- `peek` for observations that do not advance time or trigger CPU bus effects;
- `observableDigest` for regression and replay identity; and
- `inspectCartridge` for live mapper banks, RAM enable state, and MBC3 RTC state.

`Debugger` is an opt-in research wrapper around `Machine`: it provides fixed-
capacity PC breakpoints, instruction-boundary value watchpoints, mapper-bank
transition events, frame events, a bounded history ring, and non-intrusive
disassembly. Because it drives `Machine.step` from the outside, normal headless
execution contains no debugger callback or trace branch. This makes it useful
for investigating cartridge behavior without taxing training runs.

Forks retain one atomically reference-counted immutable ROM allocation while
owning independent hardware and cartridge-RAM state. Forking copies hardware
directly instead of materializing the fixed 128 KiB cartridge-RAM reserve.
`OwnedSnapshot` similarly allocates only the cartridge RAM present in the
loaded cartridge; the larger value `Snapshot` remains available for callers
that require allocation-free capture and restore.

CLI options:
- `-h`, `--help`: show help
- `-d`, `--debug`: verbose step-by-step debug output
- `-s`, `--steps <COUNT>`: stop after a maximum number of steps
- `-b`, `--breakpoint <HEX>`: stop when `PC == HEX` (with or without a `0x` prefix)
- `--headless`: run without graphics
- `--mooneye-test`: run headlessly until Mooneye reports pass/fail (exit 0/1,
  or 2 after the default 10-million-instruction timeout)

Controls (default):
- D-pad: Arrow keys
- A: `X` or `A`
- B: `Z` or `S`
- Start: `Enter`, keypad `Enter`, or `Space`
- Select: `Backspace` or `Tab`

Management hotkeys (SDL mode):
- `F1`: open/hide the detached debugger window (hidden by default)
- `P`: pause/resume emulation
- `F10`: execute one instruction while paused
- `R`: reset emulator
- `F5`: save state to active slot
- `F9`: load state from active slot
- `[ / ]`: previous/next save slot
- `C`: cycle Classic, Pocket, Mono, and Amber palettes
- `M`: mute/unmute audio
- `F11`: toggle fullscreen
- `Esc`: quit
- Debugger window: shows run/pause state, next instruction, CPU registers,
  active ROM and RAM banks, frame/dot counters, and measured presentation rate.
  It starts hidden; closing it hides the debugger without quitting emulation.
- Window title: shows run/pause state, palette, audio state, active slot, and
  the last status message

Save state notes:
- Save states are currently in-memory only (session-local, not persisted to disk).
- 10 slots are available (`0-9`), managed with `[ / ]`.

## Tests

```bash
zig build test
```

Test and reference ROMs are available under `roms/` (for example `roms/blargg/` and `roms/scribbltests/`).
Mooneye binaries are downloaded separately rather than vendored; its documented
register protocol is supported directly by `--mooneye-test`.

The current DMG validation baseline passes:

- all unit tests in Debug and ReleaseSafe builds;
- 62 applicable Mooneye acceptance ROMs (excluding boot-state and non-DMG-model variants);
- all 11 `blargg/cpu_instrs` cases;
- `blargg/instr_timing` and both three-part memory-timing suites; and
- all eight `blargg/oam_bug` cases; and
- all 12 individual `blargg/dmg_sound` cases.

The `cgb_sound` suite is outside the baseline because CGB hardware is not
implemented.

## Project layout

- `src/main.zig`: CLI entrypoint
- `src/nibble.zig`: public frontend-free package surface
- `src/machine.zig`: deterministic hardware scheduler and snapshots
- `src/emulator.zig`: SDL/host adapter around `Machine`
- `src/benchmark.zig`: reproducible headless benchmark executable
- `src/agent/`: temporal actions, model observations, reusable branch pool,
  and parallel agent runtime
- `src/agent_benchmark.zig`: complete agent-workload benchmark executable
- `src/cpu/`: CPU core + instruction decode/execute
- `src/memory/`: memory bus, IO registers, and MBC logic
- `src/ppu/`: PPU timing and rendering
- `src/apu.zig`: DMG audio registers, channel timing, mixing, and PCM handoff
- `src/frontend/`: optional host presentation and input adapters
- `src/timer.zig`: timer/divider logic
- `src/sdl.zig`: minimal SDL2 bindings
- `roms/`: local ROMs used for development/testing
- `docs/ARCHITECTURE.md`: component ownership, timing model, and accuracy limits
- `docs/AGENT_RUNTIME.md`: local visual-model boundary and buffer contract

## Notes

This project is for educational and development purposes. Use only ROMs you are legally allowed to run.

The optional SDL debugger embeds Inter under the SIL Open Font License and uses
`stb_truetype` under its MIT/public-domain dual license. `zig build` installs
their complete license texts under `share/nibble/licenses/`.
