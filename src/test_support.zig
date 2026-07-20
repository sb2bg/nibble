const Cartridge = @import("cartridge/cartridge.zig").Cartridge;

pub fn emptyCartridge(allocator: @import("std").mem.Allocator) !Cartridge {
    var rom: [0x8000]u8 = undefined;
    @memset(&rom, 0);
    return Cartridge.fromRom(allocator, &rom);
}

pub fn rtcCartridge(allocator: @import("std").mem.Allocator) !Cartridge {
    var rom = [_]u8{0} ** 0x8000;
    rom[0x0147] = 0x10; // MBC3 + timer + RAM + battery
    rom[0x0148] = 0x00; // 32 KiB ROM
    rom[0x0149] = 0x02; // 8 KiB external RAM
    return Cartridge.fromRom(allocator, &rom);
}
