# Nibble research runtime

Nibble's useful niche is not a cartridge-specific reward wrapper. It is a
deterministic, branchable Game Boy state machine for planning, replay,
causality, fuzzing, and reinforcement-learning workloads. Cartridge-specific
knowledge can live in a caller without contaminating hardware behavior or
making the emulator useful only for games with a maintained wrapper.

## Research primitives

The public Zig API provides a small set of composable operations:

- `resetDeterministic` defines episode RAM, buttons, RTC time, and sub-second
  RTC phase without consulting the host clock.
- `stepFrames` selects no video, final-frame video, or every-frame video while
  retaining dot-level PPU timing in every mode.
- `FrameInput` applies sorted actions on frame boundaries. `CycleInput` applies
  actions at exact emulated T-cycles, including inside an instruction.
- `observe` borrows CPU state and hardware memory without allocating or copying.
- `captureOwned`, `restoreOwned`, and `fork` support checkpoints,
  counterfactual branches, replay, and search trees. Forks share immutable ROM
  bytes but own independent mutable hardware and cartridge RAM.
- `MachineBatch.stepFramesWithButtonsParallel` applies one ordered action per
  environment and advances a shared action-repeat interval using Zig 0.16's
  `std.Io.Group` worker pool.
- `Debugger` adds PC breakpoints, value watchpoints, bank-switch/frame events,
  bounded trace history, and non-mutating disassembly without adding a branch
  to normal execution.

This split follows the same systems observation that motivates EnvPool:
environment execution and cross-process coordination can become the training
bottleneck. It also follows the vector-environment convention of ordered batched
actions and observations, but leaves reset/termination policy in the caller
because an emulator cannot infer a game's episode boundary generically.

## Performance envelope

The benchmark is intentionally reproducible: every trial restores the same
snapshot and must produce the same state digest. On an Apple M1 Pro (10 cores),
Zig 0.16.0, `ReleaseFast`, and Tetris, a 20-million-instruction timing-only run
currently measures:

- 13.87 million instructions per second;
- 127.91 million emulated T-cycles per second;
- **30.50x real time** and 1,813 completed frames per second; and
- roughly **219 real-time machines** in the 20-machine timing-only frame batch.

The exact command was:

```bash
zig build bench -Doptimize=ReleaseFast -- \
  --steps 20000000 --warmup 1000000 --trials 7 --no-video \
  "roms/Tetris (JUE) (V1.1) [!].gb"
```

These are local measurements, not universal claims. ROM behavior, host load,
compiler version, thermal state, and observation policy all matter. The
benchmark prints them explicitly enough to make before/after comparisons
meaningful.

Nibble does **not** currently reach 100x real time. Timing-only video is only
slightly faster than framebuffer capture because palette stores are cheap; the
background/window fetcher, FIFOs, object arbitration, and mode edges still run
at dot granularity. Reaching 100x without weakening the accuracy model will
require a deeper event-jump PPU or a substantially different CPU execution
engine, not a misleading “headless” switch that silently stops simulating
hardware.

The defensible performance niche today is therefore:

> high aggregate branch throughput with exact, inspectable, reproducible
> hardware state—not the fastest single opaque game loop.

## A demo with substance

The strongest demo is a counterfactual timeline explorer:

1. Run any ROM to a user-selected checkpoint and capture it.
2. Fork tens or hundreds of branches that share the ROM allocation.
3. Apply different button sequences through machine-ordered batch stepping with
   timing-only video.
4. Rank or filter branches using a caller-supplied generic or game-specific
   objective.
5. Restore and replay selected branches with final-frame capture, exact-cycle
   input, debugger events, and the GUI inspector.

This is useful beyond a screenshot: it is the same primitive needed by
model-based planning, automated test-case minimization, save-state search,
causal debugging, and trajectory generation. The emulator supplies truthful
hardware transitions; researchers decide what a state means.

Suggested post title: **“Forking Time on a Game Boy: Dot-Accurate
Counterfactual Search in Zig 0.16.”** The honest hook is that fast snapshots and
vectorized branches matter more than a cartridge-specific microscope, while
the failed optimization experiments show why fidelity-preserving speed work is
not equivalent to skipping rendering.

## Research and hardware references

- [EnvPool: A Highly Parallel Reinforcement Learning Environment Execution
  Engine](https://arxiv.org/abs/2206.10558)
- [The Arcade Learning Environment: An Evaluation Platform for General
  Agents](https://arxiv.org/abs/1207.4708)
- [Gymnasium vector environments](https://gymnasium.farama.org/api/vector/)
- [PyBoy API](https://docs.pyboy.dk/), including final-frame rendering and
  larger multi-frame ticks for AI workloads
- [Pan Docs rendering model](https://gbdev.io/pandocs/Rendering.html)
- [Mooneye Test Suite](https://github.com/Gekkio/mooneye-test-suite)
