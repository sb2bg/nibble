# nibble

A Nintendo Game Boy (DMG) emulator written in Zig.

## Current status

`nibble` is actively in-progress but already runs many ROMs and test ROMs.

Implemented core pieces:
- CPU instruction decode/execute loop with interrupt handling
- Memory bus with cartridge support and MBC banking (`ROM`, `MBC1`, `MBC2`, `MBC3`, `MBC5`)
- Timer (`DIV/TIMA/TMA/TAC`)
- PPU timing + background/window/sprite rendering
- SDL2 window output (with automatic headless fallback if SDL init fails)
- Joypad input mapping + joypad interrupt signaling
- Around-screen control deck UI:
  - Left side joypad visualization (D-pad, A/B, Start/Select)
  - Right side management panel with status, slot info, and action buttons
- Emulator management hotkeys (pause, reset, save/load state, slot selection)
- In-memory save states (10 slots per run session)
- Headless mode and serial output capture for test ROM workflows

Known gaps:
- No audio/APU emulation
- OAM corruption behavior is only partially accurate (`blargg/oam_bug` still has failing subtests)
- `STOP` instruction behavior is stubbed
- MBC3 RTC latch/register behavior is not implemented

## Requirements

- Zig `0.15.x` (project minimum in `build.zig.zon` is `0.15.0-dev.383+927f233ff`)
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
zig build run -- roms/Dr.\ Mario\ \(World\).gb

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
- `-b`, `--breakpoint <ADDR>`: stop when `PC == ADDR`
- `--headless`: run without graphics

Controls (default):
- D-pad: Arrow keys
- A: `X` or `A`
- B: `Z` or `S`
- Start: `Enter`, keypad `Enter`, or `Space`
- Select: `Backspace` or `Tab`
- Mouse: click the on-screen joypad/buttons in the side panels

Management hotkeys (SDL mode):
- `P`: pause/resume emulation
- `R`: reset emulator
- `F5`: save state to active slot
- `F9`: load state from active slot
- `[ / ]`: previous/next save slot
- `F1`: toggle side panel visibility
- `Esc`: quit
- Mouse: click `PAUSE`, `RESET`, `SAVE`, `LOAD`, and slot buttons in the right panel

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
- `src/timer.zig`: timer/divider logic
- `src/sdl.zig`: minimal SDL2 bindings
- `roms/`: local ROMs used for development/testing

## Notes

This project is for educational and development purposes. Use only ROMs you are legally allowed to run.
