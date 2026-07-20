const std = @import("std");
const machine_mod = @import("../machine.zig");

const Allocator = std.mem.Allocator;
const Machine = machine_mod.Machine;
const OwnedSnapshot = machine_mod.OwnedSnapshot;

/// Opaque-in-practice branch identity. A generation prevents a released slot
/// from being mistaken for a later branch that reuses the same storage.
pub const BranchId = struct {
    slot: u32,
    generation: u64,
};

pub const Error = error{
    CapacityTooLarge,
    PoolExhausted,
    InvalidBranch,
    CartridgeRamSizeMismatch,
};

/// Fixed-capacity machine storage for allocation-free checkpoint restores and
/// branch reuse. Every slot owns mutable hardware and cartridge RAM while all
/// slots share the seed machine's immutable ROM.
pub const MachinePool = struct {
    allocator: Allocator,
    machines: []Machine,
    generations: []u64,
    occupied: []bool,
    free_slots: []u32,
    free_count: usize,
    cartridge_ram_len: usize,

    pub fn initForked(
        allocator: Allocator,
        seed: *const Machine,
        slot_count: usize,
    ) (Allocator.Error || Error)!MachinePool {
        if (slot_count > std.math.maxInt(u32)) return error.CapacityTooLarge;

        const machines = try allocator.alloc(Machine, slot_count);
        errdefer allocator.free(machines);
        const generations = try allocator.alloc(u64, slot_count);
        errdefer allocator.free(generations);
        const occupied = try allocator.alloc(bool, slot_count);
        errdefer allocator.free(occupied);
        const free_slots = try allocator.alloc(u32, slot_count);
        errdefer allocator.free(free_slots);

        var initialized: usize = 0;
        errdefer for (machines[0..initialized]) |*branch_machine| branch_machine.deinit();
        while (initialized < slot_count) : (initialized += 1) {
            machines[initialized] = try seed.fork(allocator);
        }

        @memset(generations, 1);
        @memset(occupied, false);
        for (free_slots, 0..) |*slot, index| {
            slot.* = @intCast(slot_count - index - 1);
        }

        return .{
            .allocator = allocator,
            .machines = machines,
            .generations = generations,
            .occupied = occupied,
            .free_slots = free_slots,
            .free_count = slot_count,
            .cartridge_ram_len = seed.inspectCartridge().ram_bytes,
        };
    }

    pub fn deinit(self: *MachinePool) void {
        for (self.machines) |*branch_machine| branch_machine.deinit();
        self.allocator.free(self.free_slots);
        self.allocator.free(self.occupied);
        self.allocator.free(self.generations);
        self.allocator.free(self.machines);
        self.* = undefined;
    }

    pub fn capacity(self: *const MachinePool) usize {
        return self.machines.len;
    }

    pub fn activeCount(self: *const MachinePool) usize {
        return self.machines.len - self.free_count;
    }

    pub fn availableCount(self: *const MachinePool) usize {
        return self.free_count;
    }

    /// Approximate mutable pool storage, excluding the single shared ROM and
    /// allocator bookkeeping. Dynamic serial/audio buffers are empty in the
    /// headless agent configuration and are therefore not included.
    pub fn estimatedMutableBytes(self: *const MachinePool) usize {
        return self.machines.len * (@sizeOf(Machine) + self.cartridge_ram_len) +
            self.generations.len * @sizeOf(u64) +
            self.occupied.len * @sizeOf(bool) +
            self.free_slots.len * @sizeOf(u32);
    }

    pub fn acquire(
        self: *MachinePool,
        checkpoint: *const OwnedSnapshot,
    ) Error!BranchId {
        if (checkpoint.cartridge_ram.len != self.cartridge_ram_len) {
            return error.CartridgeRamSizeMismatch;
        }
        if (self.free_count == 0) return error.PoolExhausted;

        self.free_count -= 1;
        const slot = self.free_slots[self.free_count];
        self.machines[slot].restoreOwned(checkpoint) catch unreachable;
        self.occupied[slot] = true;
        return .{ .slot = slot, .generation = self.generations[slot] };
    }

    /// Fill caller-owned IDs with fresh branches restored from one checkpoint.
    /// Validation happens before any pool slot is consumed.
    pub fn acquireMany(
        self: *MachinePool,
        checkpoint: *const OwnedSnapshot,
        branches: []BranchId,
    ) Error!void {
        if (checkpoint.cartridge_ram.len != self.cartridge_ram_len) {
            return error.CartridgeRamSizeMismatch;
        }
        if (branches.len > self.free_count) return error.PoolExhausted;
        for (branches) |*branch| branch.* = try self.acquire(checkpoint);
    }

    pub fn release(self: *MachinePool, branch: BranchId) Error!void {
        const slot = try self.validate(branch);
        self.occupied[slot] = false;
        self.generations[slot] +%= 1;
        if (self.generations[slot] == 0) self.generations[slot] = 1;
        self.free_slots[self.free_count] = @intCast(slot);
        self.free_count += 1;
    }

    pub fn restore(
        self: *MachinePool,
        branch: BranchId,
        checkpoint: *const OwnedSnapshot,
    ) Error!void {
        const slot = try self.validate(branch);
        self.machines[slot].restoreOwned(checkpoint) catch
            return error.CartridgeRamSizeMismatch;
    }

    pub fn captureInto(
        self: *const MachinePool,
        branch: BranchId,
        checkpoint: *OwnedSnapshot,
    ) Error!void {
        const slot = try self.validate(branch);
        self.machines[slot].captureOwnedInto(checkpoint) catch
            return error.CartridgeRamSizeMismatch;
    }

    pub fn machine(self: *MachinePool, branch: BranchId) Error!*Machine {
        return &self.machines[try self.validate(branch)];
    }

    pub fn machineConst(self: *const MachinePool, branch: BranchId) Error!*const Machine {
        return &self.machines[try self.validate(branch)];
    }

    pub fn isValid(self: *const MachinePool, branch: BranchId) bool {
        _ = self.validate(branch) catch return false;
        return true;
    }

    fn validate(self: *const MachinePool, branch: BranchId) Error!usize {
        const slot: usize = branch.slot;
        if (slot >= self.machines.len or
            !self.occupied[slot] or
            self.generations[slot] != branch.generation)
        {
            return error.InvalidBranch;
        }
        return slot;
    }
};

test "machine pool restores reusable branches without reallocating slots" {
    var seed = Machine.init(
        std.testing.allocator,
        try @import("../test_support.zig").rtcCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();
    seed.cpu.pc = 0x4567;
    seed.bus.cartridge.ram_data.?[9] = 0xA5;
    var checkpoint = try seed.captureOwned(std.testing.allocator);
    defer checkpoint.deinit();

    var pool = try MachinePool.initForked(std.testing.allocator, &seed, 2);
    defer pool.deinit();
    const first = try pool.acquire(&checkpoint);
    const second = try pool.acquire(&checkpoint);
    try std.testing.expectError(error.PoolExhausted, pool.acquire(&checkpoint));
    try std.testing.expectEqual(@as(usize, 2), pool.activeCount());

    const first_machine = try pool.machine(first);
    try std.testing.expectEqual(@as(u16, 0x4567), first_machine.cpu.pc);
    try std.testing.expectEqual(@as(u8, 0xA5), first_machine.bus.cartridge.ram_data.?[9]);
    first_machine.bus.wram[0] = 0x77;
    try std.testing.expectEqual(@as(u8, 0), (try pool.machine(second)).bus.wram[0]);

    try pool.release(first);
    try std.testing.expect(!pool.isValid(first));
    const replacement = try pool.acquire(&checkpoint);
    try std.testing.expectEqual(first.slot, replacement.slot);
    try std.testing.expect(first.generation != replacement.generation);
    try std.testing.expectError(error.InvalidBranch, pool.machine(first));
}

test "machine pool validates bulk acquisition before mutation" {
    var seed = Machine.init(
        std.testing.allocator,
        try @import("../test_support.zig").emptyCartridge(std.testing.allocator),
        .{},
    );
    defer seed.deinit();
    var checkpoint = try seed.captureOwned(std.testing.allocator);
    defer checkpoint.deinit();
    var pool = try MachinePool.initForked(std.testing.allocator, &seed, 2);
    defer pool.deinit();

    var branches: [3]BranchId = undefined;
    try std.testing.expectError(error.PoolExhausted, pool.acquireMany(&checkpoint, &branches));
    try std.testing.expectEqual(@as(usize, 0), pool.activeCount());
}
