const std = @import("std");

pub const Bus = struct {
    memory: [0xFFFF + 1]u8, // 64KB Address Space

    pub fn init() Bus {
        return Bus{
            .memory = [_]u8{0} ** 0x10000,
        };
    }

    pub fn read(self: *Bus, addr: u16) u8 {
        return self.memory[addr];
    }

    pub fn write(self: *Bus, addr: u16, val: u8) void {
        self.memory[addr] = val;
    }

    // Helper to load a "ROM" (array of bytes) into memory
    pub fn load_rom(self: *Bus, data: []const u8) void {
        @memcpy(self.memory[0..data.len], data);
    }
};
