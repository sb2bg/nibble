const Cartridge = @import("cartridge/cartridge.zig").Cartridge;
const Mbc = @import("memory/mbc.zig").Mbc;

pub fn emptyCartridge(allocator: @import("std").mem.Allocator) !Cartridge {
    const rom = try allocator.alloc(u8, 0x8000);
    @memset(rom, 0);
    return .{
        .allocator = allocator,
        .rom_data = rom,
        .ram_data = null,
        .header = .{
            .title = [_]u8{0} ** 16,
            .cartridge_type = 0,
            .rom_size = 0,
            .ram_size = 0,
            .destination_code = 0,
            .old_licensee_code = 0,
            .mask_rom_version = 0,
            .header_checksum = 0,
            .global_checksum = 0,
        },
        .mbc = Mbc.init(.none, rom, null, false),
    };
}
