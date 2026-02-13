# nibble

A Nintendo Game Boy (DMG) emulator written in Zig.

## Current status

`nibble` is actively in-progress but already runs many ROMs and test ROMs.

Implemented core pieces:
- CPU instruction decode/execute loop with interrupt handling
- Memory bus with cartridge support and MBC banking (`ROM`, `MBC1`, `MBC2`, `MBC3`, `MBC5`)
- Timer (`DIV/TIMA/TMA/TAC`)
- PPU timing + background/window rendering
- SDL2 window output (with automatic headless fallback if SDL init fails)
- Headless mode and serial output capture for test ROM workflows

Known gaps:
- No audio/APU emulation
- No joypad input mapping yet
- No sprite/OAM rendering yet (background/window only)
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
