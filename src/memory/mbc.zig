const std = @import("std");

const RTC_CYCLES_PER_SECOND: u32 = 4_194_304;

pub const MbcType = enum {
    none,
    mbc1,
    mbc2,
    mbc3,
    mbc5,

    pub fn fromCartridgeType(cart_type: u8) ?MbcType {
        return switch (cart_type) {
            0x00, 0x08, 0x09 => .none,
            0x01, 0x02, 0x03 => .mbc1,
            0x05, 0x06 => .mbc2,
            0x0F, 0x10, 0x11, 0x12, 0x13 => .mbc3,
            0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E => .mbc5,
            else => null,
        };
    }
};

pub const Rtc = struct {
    seconds: u8 = 0,
    minutes: u8 = 0,
    hours: u8 = 0,
    day: u9 = 0,
    halted: bool = false,
    carry: bool = false,
    cycle_accumulator: u32 = 0,

    latched: [5]u8 = [_]u8{0} ** 5,
    latched_valid: bool = false,
    last_latch_bit: u1 = 0,

    fn tick(self: *Rtc, cycles: u8) void {
        if (self.halted) return;

        self.cycle_accumulator += cycles;
        if (self.cycle_accumulator < RTC_CYCLES_PER_SECOND) return;
        self.cycle_accumulator -= RTC_CYCLES_PER_SECOND;
        self.incrementSecond();
    }

    fn incrementSecond(self: *Rtc) void {
        self.seconds += 1;
        if (self.seconds < 60) return;
        self.seconds = 0;

        self.minutes += 1;
        if (self.minutes < 60) return;
        self.minutes = 0;

        self.hours += 1;
        if (self.hours < 24) return;
        self.hours = 0;

        if (self.day == 511) {
            self.day = 0;
            self.carry = true;
        } else {
            self.day += 1;
        }
    }

    fn latch(self: *Rtc) void {
        self.latched = .{
            self.seconds,
            self.minutes,
            self.hours,
            @truncate(self.day),
            (@as(u8, @truncate(self.day >> 8))) |
                (if (self.halted) @as(u8, 0x40) else 0) |
                (if (self.carry) @as(u8, 0x80) else 0),
        };
        self.latched_valid = true;
    }

    fn read(self: *const Rtc, register: u8) u8 {
        const index = register - 0x08;
        if (self.latched_valid) return self.latched[index];
        return switch (register) {
            0x08 => self.seconds,
            0x09 => self.minutes,
            0x0A => self.hours,
            0x0B => @truncate(self.day),
            0x0C => (@as(u8, @truncate(self.day >> 8))) |
                (if (self.halted) @as(u8, 0x40) else 0) |
                (if (self.carry) @as(u8, 0x80) else 0),
            else => 0xFF,
        };
    }

    fn write(self: *Rtc, register: u8, value: u8) void {
        switch (register) {
            0x08 => self.seconds = value & 0x3F,
            0x09 => self.minutes = value & 0x3F,
            0x0A => self.hours = value & 0x1F,
            0x0B => self.day = (self.day & 0x100) | value,
            0x0C => {
                self.day = (@as(u9, value & 0x01) << 8) | @as(u8, @truncate(self.day));
                self.halted = (value & 0x40) != 0;
                self.carry = (value & 0x80) != 0;
            },
            else => {},
        }
    }
};

/// Memory-bank controller state and address translation.
pub const Mbc = struct {
    pub const Snapshot = struct {
        rom_bank: u16,
        ram_bank: u8,
        ram_enabled: bool,
        banking_mode: u1,
        mbc2_ram: [512]u8,
        rtc: Rtc,
    };

    mbc_type: MbcType,
    has_rtc: bool,

    /// Raw mapper registers. For MBC1, `rom_bank` is the five-bit primary
    /// register and `ram_bank` is always the two-bit secondary register.
    rom_bank: u16,
    ram_bank: u8,
    ram_enabled: bool,
    banking_mode: u1,

    rom_data: []const u8,
    ram_data: ?[]u8,
    mbc2_ram: [512]u8,
    rtc: Rtc,

    pub fn init(
        mbc_type: MbcType,
        rom_data: []const u8,
        ram_data: ?[]u8,
        has_rtc: bool,
    ) Mbc {
        return .{
            .mbc_type = mbc_type,
            .has_rtc = has_rtc,
            .rom_bank = 1,
            .ram_bank = 0,
            .ram_enabled = mbc_type == .none and ram_data != null,
            .banking_mode = 0,
            .rom_data = rom_data,
            .ram_data = ram_data,
            .mbc2_ram = [_]u8{0} ** 512,
            .rtc = .{},
        };
    }

    /// Reset mapper control registers without erasing battery-backed memory or
    /// rewinding the real-time clock.
    pub fn reset(self: *Mbc) void {
        self.rom_bank = 1;
        self.ram_bank = 0;
        self.ram_enabled = self.mbc_type == .none and self.ram_data != null;
        self.banking_mode = 0;
        self.rtc.latched_valid = false;
        self.rtc.last_latch_bit = 0;
    }

    pub fn snapshot(self: *const Mbc) Snapshot {
        return .{
            .rom_bank = self.rom_bank,
            .ram_bank = self.ram_bank,
            .ram_enabled = self.ram_enabled,
            .banking_mode = self.banking_mode,
            .mbc2_ram = self.mbc2_ram,
            .rtc = self.rtc,
        };
    }

    pub fn restore(self: *Mbc, state: Snapshot) void {
        self.rom_bank = state.rom_bank;
        self.ram_bank = state.ram_bank;
        self.ram_enabled = state.ram_enabled;
        self.banking_mode = state.banking_mode;
        self.mbc2_ram = state.mbc2_ram;
        self.rtc = state.rtc;
    }

    pub fn tick(self: *Mbc, cycles: u8) void {
        if (self.mbc_type == .mbc3 and self.has_rtc) self.rtc.tick(cycles);
    }

    pub fn readRom(self: *const Mbc, addr: u16) u8 {
        if (addr > 0x7FFF) return 0xFF;

        const bank = self.effectiveRomBank(addr);
        const offset = @as(usize, bank) * 0x4000 + (addr & 0x3FFF);
        return if (offset < self.rom_data.len) self.rom_data[offset] else 0xFF;
    }

    pub fn writeRom(self: *Mbc, addr: u16, value: u8) void {
        switch (self.mbc_type) {
            .none => {},
            .mbc1 => self.writeMbc1(addr, value),
            .mbc2 => self.writeMbc2(addr, value),
            .mbc3 => self.writeMbc3(addr, value),
            .mbc5 => self.writeMbc5(addr, value),
        }
    }

    pub fn readRam(self: *const Mbc, addr: u16) u8 {
        if (!self.ram_enabled or addr < 0xA000 or addr > 0xBFFF) return 0xFF;

        if (self.mbc_type == .mbc2) {
            return 0xF0 | self.mbc2_ram[(addr - 0xA000) & 0x01FF];
        }
        if (self.mbc_type == .mbc3 and self.ram_bank >= 0x08 and self.ram_bank <= 0x0C) {
            return if (self.has_rtc) self.rtc.read(self.ram_bank) else 0xFF;
        }

        const ram = self.ram_data orelse return 0xFF;
        const offset = self.effectiveRamBank() * 0x2000 + (addr - 0xA000);
        return if (offset < ram.len) ram[offset] else 0xFF;
    }

    pub fn writeRam(self: *Mbc, addr: u16, value: u8) void {
        if (!self.ram_enabled or addr < 0xA000 or addr > 0xBFFF) return;

        if (self.mbc_type == .mbc2) {
            self.mbc2_ram[(addr - 0xA000) & 0x01FF] = value & 0x0F;
            return;
        }
        if (self.mbc_type == .mbc3 and self.ram_bank >= 0x08 and self.ram_bank <= 0x0C) {
            if (self.has_rtc) self.rtc.write(self.ram_bank, value);
            return;
        }

        const ram = self.ram_data orelse return;
        const offset = self.effectiveRamBank() * 0x2000 + (addr - 0xA000);
        if (offset < ram.len) ram[offset] = value;
    }

    fn effectiveRomBank(self: *const Mbc, addr: u16) u16 {
        const raw_bank: u16 = switch (self.mbc_type) {
            .none => if (addr < 0x4000) 0 else 1,
            .mbc1 => if (addr < 0x4000)
                (if (self.banking_mode == 1) @as(u16, self.ram_bank & 0x03) << 5 else 0)
            else
                (@as(u16, self.ram_bank & 0x03) << 5) |
                    (if ((self.rom_bank & 0x1F) == 0) @as(u16, 1) else self.rom_bank & 0x1F),
            .mbc2, .mbc3 => if (addr < 0x4000) 0 else self.rom_bank,
            .mbc5 => if (addr < 0x4000) 0 else self.rom_bank & 0x01FF,
        };

        const bank_count = @max(@as(usize, 1), self.rom_data.len / 0x4000);
        return @intCast(@as(usize, raw_bank) % bank_count);
    }

    fn effectiveRamBank(self: *const Mbc) usize {
        return switch (self.mbc_type) {
            .mbc1 => if (self.banking_mode == 1) self.ram_bank & 0x03 else 0,
            .mbc3 => self.ram_bank & 0x03,
            .mbc5 => self.ram_bank & 0x0F,
            else => 0,
        };
    }

    fn writeMbc1(self: *Mbc, addr: u16, value: u8) void {
        switch (addr) {
            0x0000...0x1FFF => self.ram_enabled = (value & 0x0F) == 0x0A,
            0x2000...0x3FFF => self.rom_bank = value & 0x1F,
            0x4000...0x5FFF => self.ram_bank = value & 0x03,
            0x6000...0x7FFF => self.banking_mode = @intCast(value & 0x01),
            else => {},
        }
    }

    fn writeMbc2(self: *Mbc, addr: u16, value: u8) void {
        if (addr > 0x3FFF) return;
        if ((addr & 0x0100) == 0) {
            self.ram_enabled = (value & 0x0F) == 0x0A;
        } else {
            self.rom_bank = value & 0x0F;
            if (self.rom_bank == 0) self.rom_bank = 1;
        }
    }

    fn writeMbc3(self: *Mbc, addr: u16, value: u8) void {
        switch (addr) {
            0x0000...0x1FFF => self.ram_enabled = (value & 0x0F) == 0x0A,
            0x2000...0x3FFF => {
                self.rom_bank = value & 0x7F;
                if (self.rom_bank == 0) self.rom_bank = 1;
            },
            0x4000...0x5FFF => self.ram_bank = value,
            0x6000...0x7FFF => {
                const latch_bit: u1 = @truncate(value);
                if (self.rtc.last_latch_bit == 0 and latch_bit == 1 and self.has_rtc) {
                    self.rtc.latch();
                }
                self.rtc.last_latch_bit = latch_bit;
            },
            else => {},
        }
    }

    fn writeMbc5(self: *Mbc, addr: u16, value: u8) void {
        switch (addr) {
            0x0000...0x1FFF => self.ram_enabled = (value & 0x0F) == 0x0A,
            0x2000...0x2FFF => self.rom_bank = (self.rom_bank & 0x100) | value,
            0x3000...0x3FFF => self.rom_bank = (@as(u16, value & 0x01) << 8) | (self.rom_bank & 0xFF),
            0x4000...0x5FFF => self.ram_bank = value & 0x0F,
            else => {},
        }
    }
};

test "ROM plus RAM cartridges expose their unbanked RAM" {
    var rom = [_]u8{0} ** 0x8000;
    var ram = [_]u8{0} ** 0x2000;
    var mbc = Mbc.init(.none, &rom, &ram, false);

    mbc.writeRam(0xA123, 0x5A);
    try std.testing.expectEqual(@as(u8, 0x5A), mbc.readRam(0xA123));
}

test "MBC1 advanced mode banks the lower ROM region and RAM" {
    var rom = [_]u8{0} ** (0x4000 * 64);
    for (0..64) |bank| rom[bank * 0x4000] = @intCast(bank);
    var ram = [_]u8{0} ** (0x2000 * 4);
    var mbc = Mbc.init(.mbc1, &rom, &ram, false);

    mbc.writeRom(0x0000, 0x0A);
    mbc.writeRom(0x4000, 1);
    try std.testing.expectEqual(@as(u8, 33), mbc.readRom(0x4000));
    try std.testing.expectEqual(@as(u8, 0), mbc.readRom(0x0000));

    mbc.writeRom(0x6000, 1);
    try std.testing.expectEqual(@as(u8, 32), mbc.readRom(0x0000));
    mbc.writeRam(0xA000, 0x77);
    try std.testing.expectEqual(@as(u8, 0x77), ram[0x2000]);
}

test "MBC2 provides mirrored four-bit internal RAM" {
    var rom = [_]u8{0} ** 0x8000;
    var mbc = Mbc.init(.mbc2, &rom, null, false);

    mbc.writeRom(0x0000, 0x0A);
    mbc.writeRam(0xA123, 0xAB);
    try std.testing.expectEqual(@as(u8, 0xFB), mbc.readRam(0xA123));
    try std.testing.expectEqual(@as(u8, 0xFB), mbc.readRam(0xA323));
}

test "MBC3 RTC latches a stable snapshot" {
    var rom = [_]u8{0} ** 0x8000;
    var mbc = Mbc.init(.mbc3, &rom, null, true);
    mbc.writeRom(0x0000, 0x0A);
    mbc.rtc.seconds = 17;

    mbc.writeRom(0x6000, 0);
    mbc.writeRom(0x6000, 1);
    mbc.writeRom(0x4000, 0x08);
    try std.testing.expectEqual(@as(u8, 17), mbc.readRam(0xA000));

    mbc.rtc.seconds = 18;
    try std.testing.expectEqual(@as(u8, 17), mbc.readRam(0xA000));
}

test "MBC3 RTC advances from emulated cycles and honors halt" {
    var rom = [_]u8{0} ** 0x8000;
    var mbc = Mbc.init(.mbc3, &rom, null, true);
    mbc.rtc.cycle_accumulator = RTC_CYCLES_PER_SECOND - 4;
    mbc.tick(4);
    try std.testing.expectEqual(@as(u8, 1), mbc.rtc.seconds);

    mbc.rtc.halted = true;
    mbc.rtc.cycle_accumulator = RTC_CYCLES_PER_SECOND - 4;
    mbc.tick(4);
    try std.testing.expectEqual(@as(u8, 1), mbc.rtc.seconds);
}
