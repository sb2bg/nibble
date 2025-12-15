const std = @import("std");
const Allocator = std.mem.Allocator;

pub const MbcType = enum {
    none, // ROM only
    mbc1,
    mbc2,
    mbc3,
    mbc5,
};

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

    pub fn getMbcType(self: *const RomHeader) MbcType {
        return switch (self.cartridge_type) {
            0x00, 0x08, 0x09 => .none,
            0x01, 0x02, 0x03 => .mbc1,
            0x05, 0x06 => .mbc2,
            0x0F, 0x10, 0x11, 0x12, 0x13 => .mbc3,
            0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E => .mbc5,
            else => .none,
        };
    }

    pub fn getRomBankCount(self: *const RomHeader) u16 {
        return @as(u16, 2) << @intCast(self.rom_size);
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
        std.debug.print("ROM Size: {d} KB ({d} banks)\n", .{ @as(u32, 32) << @intCast(self.rom_size), self.getRomBankCount() });

        // RAM size
        std.debug.print("RAM Size: {s}\n", .{ramSizeName(self.ram_size)});

        // MBC type
        std.debug.print("MBC Type: {s}\n", .{@tagName(self.getMbcType())});
    }
};

pub const Rom = struct {
    allocator: Allocator,
    data: []u8, // Full ROM data
    header: RomHeader,
    mbc_type: MbcType,

    // MBC state
    rom_bank: u8, // Current ROM bank (1-based for switchable bank)
    ram_bank: u8, // Current RAM bank
    ram_enabled: bool,
    banking_mode: u1, // MBC1: 0 = ROM banking, 1 = RAM banking

    // External RAM (cartridge RAM)
    ram: ?[]u8,

    pub fn load(allocator: Allocator, path: []const u8) !Rom {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const stat = try file.stat();
        const data = try allocator.alloc(u8, stat.size);
        errdefer allocator.free(data);

        const bytes_read = try file.readAll(data);
        if (bytes_read != stat.size) {
            return error.IncompleteRead;
        }

        const header = RomHeader.parse(data);
        const mbc_type = header.getMbcType();

        // Allocate external RAM if needed
        const ram_size = header.getRamSizeBytes();
        const ram: ?[]u8 = if (ram_size > 0) try allocator.alloc(u8, ram_size) else null;
        if (ram) |r| {
            @memset(r, 0);
        }

        return Rom{
            .allocator = allocator,
            .data = data,
            .header = header,
            .mbc_type = mbc_type,
            .rom_bank = 1,
            .ram_bank = 0,
            .ram_enabled = false,
            .banking_mode = 0,
            .ram = ram,
        };
    }

    pub fn deinit(self: *Rom) void {
        self.allocator.free(self.data);
        if (self.ram) |r| {
            self.allocator.free(r);
        }
    }

    /// Read from ROM address space (0x0000-0x7FFF)
    pub fn readRom(self: *const Rom, addr: u16) u8 {
        return switch (addr) {
            // Bank 0 (always fixed at 0x0000-0x3FFF)
            0x0000...0x3FFF => self.data[addr],

            // Switchable bank (0x4000-0x7FFF)
            0x4000...0x7FFF => blk: {
                const bank_offset: usize = @as(usize, self.rom_bank) * 0x4000;
                const offset = bank_offset + (addr - 0x4000);
                if (offset < self.data.len) {
                    break :blk self.data[offset];
                } else {
                    break :blk 0xFF;
                }
            },

            else => 0xFF,
        };
    }

    /// Write to ROM address space (MBC register writes)
    pub fn writeRom(self: *Rom, addr: u16, val: u8) void {
        switch (self.mbc_type) {
            .none => {}, // ROM only - writes ignored
            .mbc1 => self.writeMbc1(addr, val),
            .mbc2 => self.writeMbc2(addr, val),
            .mbc3 => self.writeMbc3(addr, val),
            .mbc5 => self.writeMbc5(addr, val),
        }
    }

    /// Read from external RAM (0xA000-0xBFFF)
    pub fn readRam(self: *const Rom, addr: u16) u8 {
        if (!self.ram_enabled) return 0xFF;
        if (self.ram) |r| {
            const offset = (@as(usize, self.ram_bank) * 0x2000) + (addr - 0xA000);
            if (offset < r.len) {
                return r[offset];
            }
        }
        return 0xFF;
    }

    /// Write to external RAM (0xA000-0xBFFF)
    pub fn writeRam(self: *Rom, addr: u16, val: u8) void {
        if (!self.ram_enabled) return;
        if (self.ram) |r| {
            const offset = (@as(usize, self.ram_bank) * 0x2000) + (addr - 0xA000);
            if (offset < r.len) {
                r[offset] = val;
            }
        }
    }

    // MBC1 register writes
    fn writeMbc1(self: *Rom, addr: u16, val: u8) void {
        switch (addr) {
            0x0000...0x1FFF => {
                // RAM enable
                self.ram_enabled = (val & 0x0F) == 0x0A;
            },
            0x2000...0x3FFF => {
                // ROM bank number (lower 5 bits)
                var bank = val & 0x1F;
                if (bank == 0) bank = 1; // Bank 0 maps to bank 1
                self.rom_bank = (self.rom_bank & 0x60) | bank;
            },
            0x4000...0x5FFF => {
                // RAM bank / upper ROM bank bits
                if (self.banking_mode == 0) {
                    // ROM banking mode - upper 2 bits of ROM bank
                    self.rom_bank = (self.rom_bank & 0x1F) | ((val & 0x03) << 5);
                } else {
                    // RAM banking mode
                    self.ram_bank = val & 0x03;
                }
            },
            0x6000...0x7FFF => {
                // Banking mode select
                self.banking_mode = @intCast(val & 0x01);
            },
            else => {},
        }
    }

    // MBC2 register writes
    fn writeMbc2(self: *Rom, addr: u16, val: u8) void {
        switch (addr) {
            0x0000...0x3FFF => {
                if ((addr & 0x0100) == 0) {
                    // RAM enable (bit 8 must be 0)
                    self.ram_enabled = (val & 0x0F) == 0x0A;
                } else {
                    // ROM bank (bit 8 must be 1)
                    var bank = val & 0x0F;
                    if (bank == 0) bank = 1;
                    self.rom_bank = bank;
                }
            },
            else => {},
        }
    }

    // MBC3 register writes
    fn writeMbc3(self: *Rom, addr: u16, val: u8) void {
        switch (addr) {
            0x0000...0x1FFF => {
                self.ram_enabled = (val & 0x0F) == 0x0A;
            },
            0x2000...0x3FFF => {
                var bank = val & 0x7F;
                if (bank == 0) bank = 1;
                self.rom_bank = bank;
            },
            0x4000...0x5FFF => {
                // RAM bank or RTC register select
                self.ram_bank = val & 0x0F;
            },
            0x6000...0x7FFF => {
                // RTC latch - not implemented yet
            },
            else => {},
        }
    }

    // MBC5 register writes
    fn writeMbc5(self: *Rom, addr: u16, val: u8) void {
        switch (addr) {
            0x0000...0x1FFF => {
                self.ram_enabled = (val & 0x0F) == 0x0A;
            },
            0x2000...0x2FFF => {
                // Lower 8 bits of ROM bank
                self.rom_bank = (self.rom_bank & 0x100) | val;
            },
            0x3000...0x3FFF => {
                // Upper 1 bit of ROM bank (9th bit)
                self.rom_bank = (@as(u16, val & 0x01) << 8) | (self.rom_bank & 0xFF);
            },
            0x4000...0x5FFF => {
                self.ram_bank = val & 0x0F;
            },
            else => {},
        }
    }

    pub fn printInfo(self: *const Rom) void {
        self.header.printInfo();
        std.debug.print("ROM Data Size: {d} bytes\n", .{self.data.len});
        if (self.ram) |r| {
            std.debug.print("External RAM: {d} bytes allocated\n", .{r.len});
        }
    }
};
