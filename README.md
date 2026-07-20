# nibble

A Nintendo Game Boy (DMG) emulator written in Zig.

## Current status

`nibble` is actively in-progress but already runs many ROMs and test ROMs.

Implemented core pieces:
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
- `P`: pause/resume emulation
- `R`: reset emulator
- `F5`: save state to active slot
- `F9`: load state from active slot
- `[ / ]`: previous/next save slot
- `C`: cycle Classic, Pocket, Mono, and Amber palettes
- `M`: mute/unmute audio
- `F11`: toggle fullscreen
- `Esc`: quit
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
- `src/emulator.zig`: emulator orchestration loop
- `src/cpu/`: CPU core + instruction decode/execute
- `src/memory/`: memory bus, IO registers, and MBC logic
- `src/ppu/`: PPU timing and rendering
- `src/apu.zig`: DMG audio registers, channel timing, mixing, and PCM handoff
- `src/frontend/`: optional host presentation and input adapters
- `src/timer.zig`: timer/divider logic
- `src/sdl.zig`: minimal SDL2 bindings
- `roms/`: local ROMs used for development/testing
- `docs/ARCHITECTURE.md`: component ownership, timing model, and accuracy limits

## Notes

This project is for educational and development purposes. Use only ROMs you are legally allowed to run.
