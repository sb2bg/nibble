const Cartridge = @import("cartridge/cartridge.zig").Cartridge;

pub fn emptyCartridge(allocator: @import("std").mem.Allocator) !Cartridge {
    var rom: [0x8000]u8 = undefined;
    @memset(rom, 0);
    return Cartridge.fromRom(allocator, &rom);
}
