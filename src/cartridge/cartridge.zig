const std = @import("std");
const Allocator = std.mem.Allocator;
const Mbc = @import("../memory/mbc.zig").Mbc;
const MbcType = @import("../memory/mbc.zig").MbcType;

const SharedRom = struct {
    allocator: Allocator,
    references: std.atomic.Value(usize) = .init(1),
    data: []u8,

    fn retain(self: *SharedRom) void {
        _ = self.references.fetchAdd(1, .monotonic);
    }

    fn release(self: *SharedRom) void {
        if (self.references.fetchSub(1, .acq_rel) != 1) return;
        const allocator = self.allocator;
        allocator.free(self.data);
        allocator.destroy(self);
    }
};

/// ROM Header information
pub const RomHeader = struct {
    title: [16]u8,
    cartridge_type: u8,
    rom_size: u8,
    ram_size: u8,
    destination_code: u8,
    old_licensee_code: u8,
    mask_rom_version: u8,
    header_checksum: u8,
    global_checksum: u16,

    pub fn parse(data: []const u8) RomHeader {
        var title: [16]u8 = undefined;
        @memcpy(&title, data[0x0134..0x0144]);

        return RomHeader{
            .title = title,
            .cartridge_type = data[0x0147],
            .rom_size = data[0x0148],
            .ram_size = data[0x0149],
            .destination_code = data[0x014A],
            .old_licensee_code = data[0x014B],
            .mask_rom_version = data[0x014C],
            .header_checksum = data[0x014D],
            .global_checksum = @as(u16, data[0x014E]) << 8 | @as(u16, data[0x014F]),
        };
    }

    pub fn getMbcType(self: *const RomHeader) ?MbcType {
        return MbcType.fromCartridgeType(self.cartridge_type);
    }

    pub fn getRomBankCount(self: *const RomHeader) ?u16 {
        return switch (self.rom_size) {
            0x00...0x08 => @as(u16, 2) << @intCast(self.rom_size),
            0x52 => 72,
            0x53 => 80,
            0x54 => 96,
            else => null,
        };
    }

    pub fn getRamSizeBytes(self: *const RomHeader) usize {
        return switch (self.ram_size) {
            0x00 => 0,
            0x01 => 2 * 1024, // 2 KB
            0x02 => 8 * 1024, // 8 KB
            0x03 => 32 * 1024, // 32 KB (4 banks of 8KB)
            0x04 => 128 * 1024, // 128 KB (16 banks of 8KB)
            0x05 => 64 * 1024, // 64 KB (8 banks of 8KB)
            else => 0,
        };
    }

    fn cartridgeTypeName(cart_type: u8) []const u8 {
        return switch (cart_type) {
            0x00 => "ROM ONLY",
            0x01 => "MBC1",
            0x02 => "MBC1+RAM",
            0x03 => "MBC1+RAM+BATTERY",
            0x05 => "MBC2",
            0x06 => "MBC2+BATTERY",
            0x08 => "ROM+RAM",
            0x09 => "ROM+RAM+BATTERY",
            0x0F => "MBC3+TIMER+BATTERY",
            0x10 => "MBC3+TIMER+RAM+BATTERY",
            0x11 => "MBC3",
            0x12 => "MBC3+RAM",
            0x13 => "MBC3+RAM+BATTERY",
            0x19 => "MBC5",
            0x1A => "MBC5+RAM",
            0x1B => "MBC5+RAM+BATTERY",
            else => "Unknown",
        };
    }

    fn ramSizeName(ram_size: u8) []const u8 {
        return switch (ram_size) {
            0x00 => "None",
            0x01 => "2 KB",
            0x02 => "8 KB",
            0x03 => "32 KB",
            0x04 => "128 KB",
            0x05 => "64 KB",
            else => "Unknown",
        };
    }

    pub fn getTitle(self: *const RomHeader) []const u8 {
        var len: usize = 0;
        for (self.title) |c| {
            if (c == 0) break;
            len += 1;
        }
        return self.title[0..len];
    }

    pub fn printInfo(self: *const RomHeader) void {
        std.debug.print("=== ROM Header ===\n", .{});

        // Title
        std.debug.print("Title: ", .{});
        for (self.title) |c| {
            if (c == 0) break;
            if (c >= 0x20 and c < 0x7F) {
                std.debug.print("{c}", .{c});
            }
        }
        std.debug.print("\n", .{});

        // Cartridge type
        std.debug.print("Cartridge Type: 0x{X:0>2} ({s})\n", .{ self.cartridge_type, cartridgeTypeName(self.cartridge_type) });

        // ROM size
        if (self.getRomBankCount()) |banks| {
            std.debug.print("ROM Size: {d} KB ({d} banks)\n", .{ @as(u32, banks) * 16, banks });
        } else {
            std.debug.print("ROM Size: unknown code 0x{X:0>2}\n", .{self.rom_size});
        }

        // RAM size
        std.debug.print("RAM Size: {s}\n", .{ramSizeName(self.ram_size)});

        // MBC type
        if (self.getMbcType()) |mbc_type| {
            std.debug.print("MBC Type: {s}\n", .{@tagName(mbc_type)});
        } else {
            std.debug.print("MBC Type: unsupported\n", .{});
        }
    }
};

/// Cartridge - contains ROM data, RAM, header, and MBC
pub const Cartridge = struct {
    pub const Inspection = struct {
        header: RomHeader,
        rom_bytes: usize,
        ram_bytes: usize,
        mapper: Mbc.Inspection,
    };

    allocator: Allocator,
    shared_rom: *SharedRom,
    rom_data: []const u8,
    ram_data: ?[]u8,
    header: RomHeader,
    mbc: Mbc,

    pub fn load(allocator: Allocator, io: std.Io, path: []const u8) !Cartridge {
        const file = try std.Io.Dir.cwd().openFile(io, path, .{});
        defer file.close(io);

        const stat = try file.stat(io);
        if (stat.size < 0x150) return error.RomTooSmall;
        const rom_size = std.math.cast(usize, stat.size) orelse return error.RomTooLarge;
        const rom_data = try allocator.alloc(u8, rom_size);

        const bytes_read = file.readPositionalAll(io, rom_data, 0) catch |err| {
            allocator.free(rom_data);
            return err;
        };
        if (bytes_read != rom_size) {
            allocator.free(rom_data);
            return error.IncompleteRead;
        }

        return fromOwnedRom(allocator, rom_data);
    }

    /// Construct a cartridge without involving the filesystem. The ROM is
    /// copied so callers may release their input immediately; immutable ROM
    /// sharing between many machines can be layered on top of this boundary.
    pub fn fromRom(allocator: Allocator, data: []const u8) !Cartridge {
        const rom_data = try allocator.dupe(u8, data);
        return fromOwnedRom(allocator, rom_data);
    }

    /// Take ownership of an allocated ROM buffer and validate its header.
    fn fromOwnedRom(allocator: Allocator, rom_data: []u8) !Cartridge {
        errdefer allocator.free(rom_data);
        if (rom_data.len < 0x150) return error.RomTooSmall;

        const header = RomHeader.parse(rom_data);
        const mbc_type = header.getMbcType() orelse return error.UnsupportedCartridgeType;
        const rom_banks = header.getRomBankCount() orelse return error.InvalidRomSizeCode;
        if (rom_data.len < @as(usize, rom_banks) * 0x4000) return error.TruncatedRom;

        // Allocate external RAM if needed
        const ram_size = header.getRamSizeBytes();
        const ram_data: ?[]u8 = if (ram_size > 0) blk: {
            const ram = try allocator.alloc(u8, ram_size);
            @memset(ram, 0);
            break :blk ram;
        } else null;
        errdefer if (ram_data) |ram| allocator.free(ram);

        const shared_rom = try allocator.create(SharedRom);
        shared_rom.* = .{
            .allocator = allocator,
            .data = rom_data,
        };

        return Cartridge{
            .allocator = allocator,
            .shared_rom = shared_rom,
            .rom_data = rom_data,
            .ram_data = ram_data,
            .header = header,
            .mbc = Mbc.init(
                mbc_type,
                rom_data,
                ram_data,
                header.cartridge_type == 0x0F or header.cartridge_type == 0x10,
            ),
        };
    }

    /// Create independent mapper and cartridge-RAM state while retaining the
    /// immutable ROM allocation. This is the ownership primitive used by
    /// deterministic machine forks and multi-instance workloads.
    pub fn cloneForMachine(self: *const Cartridge, allocator: Allocator) !Cartridge {
        const ram_data: ?[]u8 = if (self.ram_data) |ram| blk: {
            const clone = try allocator.alloc(u8, ram.len);
            @memcpy(clone, ram);
            break :blk clone;
        } else null;
        errdefer if (ram_data) |ram| allocator.free(ram);

        self.shared_rom.retain();
        var clone: Cartridge = .{
            .allocator = allocator,
            .shared_rom = self.shared_rom,
            .rom_data = self.shared_rom.data,
            .ram_data = ram_data,
            .header = self.header,
            .mbc = Mbc.init(
                self.header.getMbcType().?,
                self.shared_rom.data,
                ram_data,
                self.header.cartridge_type == 0x0F or self.header.cartridge_type == 0x10,
            ),
        };
        clone.mbc.restore(self.mbc.snapshot());
        return clone;
    }

    pub fn deinit(self: *Cartridge) void {
        if (self.ram_data) |ram| {
            self.allocator.free(ram);
        }
        self.shared_rom.release();
    }

    pub fn printInfo(self: *const Cartridge) void {
        self.header.printInfo();
        std.debug.print("ROM Data Size: {d} bytes\n", .{self.rom_data.len});
        if (self.ram_data) |r| {
            std.debug.print("External RAM: {d} bytes allocated\n", .{r.len});
        }
    }

    pub fn inspect(self: *const Cartridge) Inspection {
        return .{
            .header = self.header,
            .rom_bytes = self.rom_data.len,
            .ram_bytes = if (self.ram_data) |ram| ram.len else 0,
            .mapper = self.mbc.inspect(),
        };
    }
};
