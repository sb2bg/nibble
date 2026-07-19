# Nibble architecture

Nibble is organized around hardware ownership rather than around opcode or UI
features. The `Emulator` is the scheduler: the CPU advances the bus in T-cycles,
and the scheduler advances the PPU, timer, DMA engine, and cartridge clock by the
same amount.

## Component boundaries

- `Cpu` owns LR35902 register and interrupt-execution state. It reaches memory
  only through `Bus`.
- `Bus` owns the Game Boy address map and arbitration. CPU access restrictions
  during PPU mode 3, OAM scan, and DMG OAM DMA belong here because they affect
  which device answers a memory access.
- `Timer` owns the hidden 16-bit system counter and delayed TIMA reload. The bus
  routes DIV/TIMA/TMA/TAC writes to it so there is one source of timer state.
- `IoRegisters` owns memory-mapped register values and register-local behavior.
  It also owns the combined edge-triggered STAT line and selected joypad lines.
- `Mbc` owns mapper registers, address translation, MBC2 internal nibble RAM,
  and the deterministic MBC3 RTC. Mapper snapshots intentionally exclude ROM
  and external-RAM pointers.
- `Ppu` owns dot timing, background/window fetch state, pixel FIFOs,
  window-line state, and the logical DMG frame buffer. It emits a frame-ready
  edge but has no dependency on SDL or host input.
- `SdlFrontend` owns the host window, texture conversion, keyboard mapping, and
  management UI. It also owns presentation-only choices such as scaling,
  fullscreen state, and color themes. It is optional; graphical and headless
  runs execute the same PPU core.

Save states snapshot component-owned state, while immutable ROM data and owned
allocations remain in place. Any newly persistent hardware field should be added
to its component snapshot at the same time it is introduced.

## Accuracy model

The shared clock is measured in T-cycles (4,194,304 per second on DMG). CPU bus
accesses consume one four-T-cycle M-cycle through the bus hook; instruction-only
cycles are applied by the emulator after execution.

Implemented timing details include:

- timer falling-edge behavior for normal counting and DIV/TAC writes;
- four-T-cycle TIMA overflow delay and cancellation by a TIMA write;
- 160-M-cycle OAM DMA with DMG CPU bus lockout;
- one rising-edge-detected STAT line across mode and LYC sources;
- variable mode 3 duration for fine SCX scrolling, window startup, and selected
  objects, with HBlank shortened so every visible line stays 456 dots;
- two-dot background/window tile fetch stages, FIFO startup, fine-scroll pixel
  discard, and FIFO restart at the window boundary;
- palette lookup at pixel-output time, allowing mid-scanline BGP changes to
  affect only pixels that have not reached the LCD yet;
- a window line counter that advances only on lines where the window is drawn;
- cycle-driven MBC3 RTC state, making emulation and save states deterministic.

## Deliberate approximations

The background/window PPU is dot-driven, but object fetches currently stall the
background fetcher according to their dot penalties and mix sprite pixels after
the line; it does not yet have the hardware's separate object FIFO. Mid-scanline
object palette/OAM effects therefore remain approximate.
CPU cycles that are not attached to a memory access are generally applied after
the instruction, so a few sub-instruction peripheral races remain approximate.
OAM corruption is modeled but does not pass every `blargg/oam_bug` case. Serial
transfers complete immediately, STOP is a low-power approximation, and the APU
is not implemented.

These references define the current fidelity targets:

- [Pan Docs: timer obscure behavior](https://gbdev.io/pandocs/Timer_Obscure_Behaviour.html)
- [Pan Docs: OAM DMA](https://gbdev.io/pandocs/OAM_DMA_Transfer.html)
- [Pan Docs: rendering](https://gbdev.io/pandocs/Rendering.html)
- [Pan Docs: interrupt sources and STAT blocking](https://gbdev.io/pandocs/Interrupt_Sources.html)
- [Pan Docs: MBC1](https://gbdev.io/pandocs/MBC1.html), [MBC2](https://gbdev.io/pandocs/MBC2.html), and [MBC3](https://gbdev.io/pandocs/MBC3.html)
- [Mooneye Test Suite](https://github.com/Gekkio/mooneye-test-suite), whose
  hardware-verified acceptance tests should be the next automated ROM suite.
