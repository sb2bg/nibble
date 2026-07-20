# Agent runtime and local visual models

Nibble's agent layer is a host-side adapter around the same deterministic
`Machine` used by the GUI and test runner. It does not add a second, relaxed
emulator. The implemented path is:

1. `MachinePool` preallocates mutable machine slots that share immutable ROM.
2. Generation-tagged `BranchId` values make slot reuse explicit and reject
   stale handles.
3. `Action` holds a button state for a requested number of frames, releases it,
   and optionally advances through a release interval.
4. `AgentRuntime.step` partitions independent branches through an injected
   Zig 0.16 `std.Io` worker pool.
5. Each worker writes its final framebuffer directly into the caller's slice
   of one contiguous observation batch while that machine is still cache-hot.
6. One policy invocation consumes the whole batch and produces the next action
   array.

Game-specific rewards, termination tests, privileged training labels, and
model code stay above this layer. `Machine` continues to represent hardware
only.

## Zig API

The important ownership rule is that the caller owns action, result, state,
and frame buffers. The runtime does not allocate while stepping.

```zig
var runtime = try nibble.agent.AgentRuntime.init(
    allocator,
    io,
    &seed_machine,
    256,
    .{},
);
defer runtime.deinit();

var checkpoint = try seed_machine.captureOwned(allocator);
defer checkpoint.deinit();

try runtime.pool.acquireMany(&checkpoint, branch_ids);
const batch = try runtime.step(branch_ids, actions, .{
    .frame_encoding = .palette_u8,
}, .{
    .results = results,
    .states = states,
    .frames = frame_storage,
});

// Submit batch.visual.? once to the local policy backend.
```

`captureOwnedInto` refreshes an existing checkpoint allocation. Pool acquire,
restore, release, macro-action stepping, and observation extraction then reuse
existing storage.

## Observation formats

`palette_u8` is a `[batch, 144, 160]` array of logical DMG palette indices
from 0 through 3. It costs 23,040 bytes per environment and is the preferred
input for a local GPU because a model backend can upload and normalize it
without a CPU unpack pass.

`packed_2bpp` stores four left-to-right pixels per byte, from the high two bits
to the low two bits. It costs 5,760 bytes per environment and is preferable for
trajectory storage, shared-memory pressure, or a backend with a native packed
input kernel. Both formats preserve palette indices rather than presentation
RGB; palette themes remain a GUI concern.

Set `frame_encoding` to null for timing-only rollouts. This still executes the
PPU fetchers, FIFOs, arbitration, and frame edges; it only omits final
framebuffer stores and observation encoding.

## Local model process

The first model adapter should be a narrow local worker, not model-framework
code embedded in the emulator:

```text
Zig / std.Io workers                 Local policy worker
--------------------                 -------------------
MachinePool                          PyTorch MPS (Mac)
macro-actions       contiguous       or PyTorch CUDA (desktop)
final-frame encode  observations --> one batched inference
branch identities   actions      <-- buttons + frame durations
```

The production transport should use two fixed shared-memory regions and a
small control channel:

- Zig publishes a generation number, encoding, batch count, and branch IDs
  after filling an observation region.
- The worker maps that region as `uint8`, performs one device upload and one
  inference call, and writes fixed-size actions into the return region.
- Recurrent hidden state is keyed by the full generation-tagged `BranchId`, so
  releasing and reusing a slot cannot leak memory from an old trajectory.
- Zig waits through its injected `std.Io` backend rather than adding a host
  callback or process primitive to `Machine`.

This keeps models local. On this repository's M1 Pro, PyTorch's Metal backend
is already available; the same worker contract can select CUDA on a desktop.
MLX is a reasonable later Mac backend, but it is not required to validate the
architecture and should not dictate the emulator API.

## Measured crossover point

On an M1 Pro with 16 GB unified memory, Zig 0.16.0, `ReleaseFast`, Tetris, 128
environments, and packed final-frame observations, the current workload
benchmark measures roughly 10,000 one-frame environment steps per second and
13,000 aggregate emulated frames per second at action repeat 8. Allocation-free
branch resets are roughly 750,000 per second. Exact results vary with host load.

A local 9.0-million-parameter CNN plus GRU measured about 30,900 observations
per second in PyTorch MPS inference, or 25,200 observations per second including
uploading and normalizing a CPU `uint8` batch. That is not a trained policy or a
universal model benchmark; it identifies the current systems crossover. For a
compact first policy, simulation is slower than inference. A substantially
larger encoder or planner will eventually move the bottleneck back to the GPU.

The optional policy-side benchmark requires PyTorch and reproduces both paths:

```bash
python3 tools/benchmark_visual_policy.py --batch 128
```

Reproduce the emulator half with:

```bash
zig build agent-bench -Doptimize=ReleaseFast -- \
  --environments 128 --iterations 100 --warmup 4 \
  "roms/Tetris (JUE) (V1.1) [!].gb"
```

Compare `--encoding packed`, `raw`, and `none` on the same host. The benchmark
reports simulation and policy-shim time separately; the shim verifies buffer
consumption and must not be presented as neural-network throughput.
