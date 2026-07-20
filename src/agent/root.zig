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

test {
    _ = @import("action.zig");
    _ = @import("observation.zig");
}
