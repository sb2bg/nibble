# nibble

A Nintendo Game Boy (DMG) emulator written in Zig.

## Current status

`nibble` is actively in-progress but already runs many ROMs and test ROMs.

Implemented core pieces:
- CPU instruction decode/execute loop with interrupt handling
- Memory bus with cartridge support and MBC banking (`ROM`, `MBC1`, `MBC2`, `MBC3`, `MBC5`)
- Timer (`DIV/TIMA/TMA/TAC`)
- Timed DMG OAM DMA and CPU bus lockout
- Dot-driven PPU timing with background/window and object pixel FIFOs
- Deterministic MBC3 real-time clock registers and latching
- SDL2 window output (with automatic headless fallback if SDL init fails)
- Joypad input mapping + joypad interrupt signaling
- Resizable, high-DPI frontend with frame pacing, palette themes, and fullscreen
- Emulator management hotkeys (pause, reset, save/load state, slot selection)
- In-memory save states (10 slots per run session)
- Headless mode and serial output capture for test ROM workflows

Known gaps:
- No audio/APU emulation
- OAM corruption behavior is only partially accurate (`blargg/oam_bug` still has failing subtests)
- `STOP` instruction behavior is only partially modeled
- Object fetch cancellation and fetcher arbitration are still approximate
- Serial transfers complete immediately rather than at link-cable timing

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
```

CLI options:
- `-h`, `--help`: show help
- `-d`, `--debug`: verbose step-by-step debug output
- `-s`, `--steps <COUNT>`: stop after a maximum number of steps
- `-b`, `--breakpoint <HEX>`: stop when `PC == HEX` (with or without a `0x` prefix)
- `--headless`: run without graphics

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
- `F11`: toggle fullscreen
- `Esc`: quit
- Window title: shows run/pause state, active slot, and the last status message

Save state notes:
- Save states are currently in-memory only (session-local, not persisted to disk).
- 10 slots are available (`0-9`), managed with `[ / ]`.

## Tests

```bash
zig build test
```

Test and reference ROMs are available under `roms/` (for example `roms/blargg/` and `roms/scribbltests/`).

## Project layout

- `src/main.zig`: CLI entrypoint
- `src/emulator.zig`: emulator orchestration loop
- `src/cpu/`: CPU core + instruction decode/execute
- `src/memory/`: memory bus, IO registers, and MBC logic
- `src/ppu/`: PPU timing and rendering
- `src/frontend/`: optional host presentation and input adapters
- `src/timer.zig`: timer/divider logic
- `src/sdl.zig`: minimal SDL2 bindings
- `roms/`: local ROMs used for development/testing
- `docs/ARCHITECTURE.md`: component ownership, timing model, and accuracy limits

## Notes

This project is for educational and development purposes. Use only ROMs you are legally allowed to run.
