# Nibble architecture

Nibble is organized around hardware ownership rather than around opcode or UI
features. `Machine` is the deterministic scheduler: the CPU advances the bus in
T-cycles, and the scheduler advances the PPU, timer, APU, DMA engine, serial
engine, and cartridge clock by the same amount. `Emulator` is an application
adapter that owns SDL, host clocks, frame pacing, pause state, and UI save slots.

## Component boundaries

- `Cpu` owns LR35902 register and interrupt-execution state. It reaches memory
  only through `Bus`.
- `Bus` owns the Game Boy address map and arbitration. CPU access restrictions
  during PPU mode 3, OAM scan, and DMG OAM DMA belong here because they affect
  which device answers a memory access.
- `Timer` owns the hidden 16-bit system counter and delayed TIMA reload. The bus
  routes DIV/TIMA/TMA/TAC writes to it so there is one source of timer state.
- `Serial` owns link-transfer progress. The bus routes SC writes to it and
  advances the 8,192 Hz DMG internal clock alongside the other peripherals.
- `Apu` owns `FF10-FF3F`, the four channel generators, frame sequencer, length,
  envelope and sweep units, DMG wave-RAM arbitration, and the PCM handoff
  buffer. It observes the timer's hidden counter but does not own host audio.
- `IoRegisters` owns memory-mapped register values and register-local behavior.
  It also owns the combined edge-triggered STAT line and selected joypad lines.
- `Mbc` owns mapper registers, address translation, MBC2 internal nibble RAM,
  and the deterministic MBC3 RTC. Mapper snapshots intentionally exclude ROM
  and external-RAM pointers. Its structured inspection result exposes effective
  banks and RTC selection without allowing debugger code to mutate registers.
- `Ppu` owns dot timing, background/window fetch state, pixel FIFOs,
  window-line state, and the logical DMG frame buffer. It emits a frame-ready
  edge but has no dependency on SDL or host input.
- `SdlFrontend` owns the host window, texture conversion, keyboard mapping,
  queued audio device, and management UI. It also owns presentation-only
  choices such as scaling, fullscreen state, mute, and color themes. It is
  optional; graphical and headless runs execute the same PPU and APU cores.

The public `nibble` module exports only frontend-independent pieces. A machine
can be driven from a native application, benchmark, test runner, or future
language binding without constructing `std.Io` or SDL. Filesystem loading is a
convenience on `Cartridge`; `Cartridge.fromRom` is the in-memory boundary.

Host outputs are explicit. Disabling PCM capture skips sample mixing but never
disables APU registers, generators, the frame sequencer, or wave-RAM access
windows. Pixel capture follows the same rule: timing-only and final-frame modes
retain the fetcher, FIFOs, sprite arbitration, palette timing, and frame edges,
while omitting framebuffer stores that the caller will not observe. `stepFrames`
applies caller-owned input timelines at deterministic frame boundaries, and
`observe` returns borrowed memory, tile-map, CPU, and framebuffer views without
allocating. Cycle-input timelines are split inside the CPU's bus hook, so an
input transition can land on an exact T-cycle even when the enclosing
instruction finishes later. MBC3 clocks are seeded from explicit emulated state,
never host wall time, and deterministic episode resets may clear external RAM.
Similarly, a non-intrusive `peek` does not consume a bus cycle or cause CPU-only
DMA/PPU arbitration side effects.

`Machine.Snapshot` captures component-owned state without allocation, while
immutable ROM data and owned allocations remain in place. Its fixed cartridge
RAM capacity is useful for save slots but wasteful in large search trees, so
`OwnedSnapshot` stores only the loaded cartridge's actual RAM. Any newly
persistent hardware field should be added to the shared core snapshot at the
same time it is introduced. Snapshot restore clears host output queues because
queued serial text and PCM are effects, not hardware state. The observable-state
digest is a replay/regression identity, not a serialized save-state format or
cryptographic hash.

`Machine.fork` constructs an independent mutable machine while retaining the
cartridge's immutable ROM allocation through an atomic reference count. It
copies hardware directly into the branch instead of materializing a fixed-size
snapshot. Each branch owns its IO allocations, external cartridge RAM, mapper,
and peripheral state, so branches may be scheduled on different workers after
they are created.

`MachineBatch` partitions independent machines into at most one chunk per host
CPU and schedules those chunks with Zig 0.16's `std.Io.Group.concurrent`. The
emulation loop itself remains single-threaded and deterministic; concurrency is
only introduced across machines. Backends that cannot guarantee concurrency
fall back to completing unsubmitted chunks on the caller.

## Accuracy model

The shared clock is measured in T-cycles (4,194,304 per second on DMG). CPU bus
accesses consume one four-T-cycle M-cycle through the bus hook; instruction-only
cycles with observable bus behavior are clocked at their exact execution point.
Any remaining peripheral-invisible cycles are reconciled by the emulator after
execution.

Implemented timing details include:

- timer falling-edge behavior for normal counting and DIV/TAC writes;
- four-T-cycle TIMA overflow delay and cancellation by a TIMA write;
- 160-M-cycle OAM DMA with DMG CPU bus lockout;
- mode-2 OAM row-latch corruption for reads, writes, and hidden 16-bit IDU bus
  events, including secondary, tertiary, and DMG quaternary read patterns;
- one rising-edge-detected STAT line across mode and LYC sources;
- variable mode 3 duration for fine SCX scrolling, window startup, and selected
  objects, with HBlank shortened so every visible line stays 456 dots;
- two-dot background/window tile fetch stages, FIFO startup, fine-scroll pixel
  discard, and FIFO restart at the window boundary;
- mode-2 selection of at most ten objects, DMG X/OAM priority ordering, and an
  eight-pixel object FIFO mixed with background pixels at LCD output time;
- palette lookup at pixel-output time, allowing mid-scanline BGP changes to
  affect only pixels that have not reached the LCD yet;
- a window line counter that advances only on lines where the window is drawn;
- cycle-driven MBC3 RTC state, making emulation and save states deterministic.
- eight-bit internal-clock serial transfers over 4,096 dots, including visible
  SB shifts, SC completion, disconnected-high input, and the serial interrupt.
- DIV-APU falling-edge frame sequencing with hardware length, envelope, and
  sweep cadence, including DIV-write clocks and DMG power-off length behavior;
- all four DMG audio generators, NR50/NR51 stereo routing, DAC power gating,
  wave-RAM access windows, and channel-3 retrigger corruption; and
- fixed-rate 48 kHz PCM generation with a high-pass filter and a bounded SDL
  queue that cannot feed host timing back into the emulated clock.

## Deliberate approximations

The PPU has separate background and object FIFOs, but an object tile fetch is
still represented as one scheduled stall and atomic tile-row read instead of
the hardware's cancelable fetch micro-steps. Rapid mid-fetch LCDC changes and
some background/object fetcher arbitration therefore remain approximate.
CPU cycles that are not attached to a memory access are generally applied after
the instruction, so a few sub-instruction peripheral races remain approximate.
External serial transfers wait for a clock indefinitely because link partners
are not implemented. STOP is a low-power approximation. The APU's digital
timing is hardware-tested, while the final analog stage deliberately uses a
linear per-channel mix rather than board-revision-specific nonlinear transfer
functions and capacitor characteristics.

These references define the current fidelity targets:

- [Pan Docs: timer obscure behavior](https://gbdev.io/pandocs/Timer_Obscure_Behaviour.html)
- [Pan Docs: OAM DMA](https://gbdev.io/pandocs/OAM_DMA_Transfer.html)
- [Pan Docs: OAM corruption bug](https://gbdev.io/pandocs/OAM_Corruption_Bug.html)
- [Pan Docs: rendering](https://gbdev.io/pandocs/Rendering.html)
- [Pan Docs: interrupt sources and STAT blocking](https://gbdev.io/pandocs/Interrupt_Sources.html)
- [Pan Docs: serial data transfer](https://gbdev.io/pandocs/Serial_Data_Transfer_%28Link_Cable%29.html)
- [Pan Docs: audio](https://gbdev.io/pandocs/Audio.html) and
  [audio registers](https://gbdev.io/pandocs/Audio_Registers.html)
- [Pan Docs: MBC1](https://gbdev.io/pandocs/MBC1.html), [MBC2](https://gbdev.io/pandocs/MBC2.html), and [MBC3](https://gbdev.io/pandocs/MBC3.html)
- [Mooneye Test Suite](https://github.com/Gekkio/mooneye-test-suite), whose 62
  applicable DMG acceptance ROMs form the hardware-timing baseline.
- [Blargg's Game Boy test ROMs](https://github.com/retrio/gb-test-roms), used
  for CPU, instruction, memory, OAM-corruption, and DMG-audio regression
  coverage.
