//! Agent-facing primitives built on the deterministic hardware core.
//!
//! This layer owns decision-time concepts such as temporal actions and batched
//! observations. Game-specific rewards and model runtimes belong above it.

pub const Action = @import("action.zig").Action;
pub const ActionResult = @import("action.zig").ActionResult;
pub const stepAction = @import("action.zig").step;
pub const FrameEncoding = @import("observation.zig").FrameEncoding;
pub const ObservationBatch = @import("observation.zig").Batch;
pub const observation = @import("observation.zig");
pub const BranchId = @import("pool.zig").BranchId;
pub const MachinePool = @import("pool.zig").MachinePool;
pub const AgentRuntime = @import("runtime.zig").AgentRuntime;
pub const RuntimeInitOptions = @import("runtime.zig").InitOptions;
pub const RuntimeStepOptions = @import("runtime.zig").StepOptions;
pub const RuntimeStepBuffers = @import("runtime.zig").StepBuffers;
pub const StateObservation = @import("runtime.zig").StateObservation;

test {
    _ = @import("action.zig");
    _ = @import("observation.zig");
    _ = @import("pool.zig");
    _ = @import("runtime.zig");
}
