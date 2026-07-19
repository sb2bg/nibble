const std = @import("std");

/// I/O Register addresses (relative to 0xFF00)
pub const IoReg = enum(u8) {
    // Joypad
    JOYP = 0x00,

    // Serial
    SB = 0x01,
    SC = 0x02,

    // Timer
    DIV = 0x04,
    TIMA = 0x05,
    TMA = 0x06,
    TAC = 0x07,

    // Interrupt Flag
    IF = 0x0F,

    // Sound (APU)
    NR10 = 0x10,
    NR11 = 0x11,
    NR12 = 0x12,
    NR13 = 0x13,
    NR14 = 0x14,
    NR21 = 0x16,
    NR22 = 0x17,
    NR23 = 0x18,
    NR24 = 0x19,
    NR30 = 0x1A,
    NR31 = 0x1B,
    NR32 = 0x1C,
    NR33 = 0x1D,
    NR34 = 0x1E,
    NR41 = 0x20,
    NR42 = 0x21,
    NR43 = 0x22,
    NR44 = 0x23,
    NR50 = 0x24,
    NR51 = 0x25,
    NR52 = 0x26,

    // Wave RAM: 0xFF30-0xFF3F (handled separately)

    // LCD
    LCDC = 0x40,
    STAT = 0x41,
    SCY = 0x42,
    SCX = 0x43,
    LY = 0x44,
    LYC = 0x45,
    DMA = 0x46,
    BGP = 0x47,
    OBP0 = 0x48,
    OBP1 = 0x49,
    WY = 0x4A,
    WX = 0x4B,

    // CGB registers (for future use)
    KEY1 = 0x4D,
    VBK = 0x4F,
    HDMA1 = 0x51,
    HDMA2 = 0x52,
    HDMA3 = 0x53,
    HDMA4 = 0x54,
    HDMA5 = 0x55,
    BCPS = 0x68,
    BCPD = 0x69,
    OCPS = 0x6A,
    OCPD = 0x6B,
    SVBK = 0x70,

    _,
};

/// Interrupt flags bit positions
pub const Interrupt = struct {
    pub const VBLANK: u8 = 0x01;
    pub const LCD_STAT: u8 = 0x02;
    pub const TIMER: u8 = 0x04;
    pub const SERIAL: u8 = 0x08;
    pub const JOYPAD: u8 = 0x10;
};

/// I/O Registers with special read/write behavior
pub const IoRegisters = struct {
    // Raw register storage
    data: [0x80]u8,

    // Joypad state
    joypad_select: u8, // Which buttons are selected (bits 4-5 of JOYP)
    joypad_buttons: u8, // Button state: bits 0-3 = D-pad, bits 4-7 = buttons
    // Bit layout: Start, Select, B, A (bits 4-7), Down, Up, Left, Right (bits 0-3)
    // 0 = pressed, 1 = not pressed

    // Current OAM scan row during mode 2 (0-19)
    oam_scan_row: u8,

    // The PPU's memory ownership changes a few dots before/after its public
    // STAT mode bits at some boundaries, so access control cannot be derived
    // solely from STAT.
    ppu_oam_read_blocked: bool,
    ppu_oam_write_blocked: bool,
    ppu_vram_read_blocked: bool,
    ppu_vram_write_blocked: bool,

    // The four enabled STAT sources are ORed onto one edge-triggered line.
    // Tracking the previous level prevents duplicate interrupts when two
    // sources overlap (commonly called STAT blocking).
    stat_irq_line: bool,

    // Serial output (for test ROMs)
    allocator: std.mem.Allocator,
    serial_output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) IoRegisters {
        var io = IoRegisters{
            .data = [_]u8{0} ** 0x80,
            .joypad_select = 0x30, // Neither selected
            .joypad_buttons = 0xFF, // All buttons released
            .oam_scan_row = 0,
            .ppu_oam_read_blocked = false,
            .ppu_oam_write_blocked = false,
            .ppu_vram_read_blocked = false,
            .ppu_vram_write_blocked = false,
            .stat_irq_line = false,
            .allocator = allocator,
            .serial_output = .empty,
        };

        // Set initial values for some registers (post-boot ROM values)
        io.data[@intFromEnum(IoReg.JOYP)] = 0xCF;
        io.data[@intFromEnum(IoReg.SC)] = 0x7E;
        io.data[@intFromEnum(IoReg.IF)] = 0xE1;
        io.data[@intFromEnum(IoReg.DIV)] = 0xAB;
        io.data[@intFromEnum(IoReg.TAC)] = 0xF8;
        io.data[@intFromEnum(IoReg.LCDC)] = 0x91;
        io.data[@intFromEnum(IoReg.STAT)] = 0x81; // Mode 1 (VBlank) with bit 7 set
        io.data[@intFromEnum(IoReg.LY)] = 0x91; // Post-boot LY value (in VBlank)
        io.data[@intFromEnum(IoReg.BGP)] = 0xFC;
        io.data[@intFromEnum(IoReg.NR52)] = 0xF1;

        return io;
    }

    pub fn deinit(self: *IoRegisters) void {
        self.serial_output.deinit(self.allocator);
    }

    pub fn reset(self: *IoRegisters) void {
        const allocator = self.allocator;
        self.serial_output.clearRetainingCapacity();
        self.* = IoRegisters.init(allocator);
    }

    /// Read from I/O register (addr is 0x00-0x7F, relative to 0xFF00)
    pub fn read(self: *const IoRegisters, addr: u8) u8 {
        if (isUnusedDmgRegister(addr)) return 0xFF;
        const reg: IoReg = @enumFromInt(addr);
        return switch (reg) {
            .JOYP => self.readJoypad(),
            .DIV => self.data[addr],
            .LY => self.data[addr], // Read-only PPU scanline
            .STAT => self.data[addr] | 0x80, // Bit 7 always set
            .IF => self.data[addr] | 0xE0, // Upper 3 bits always set
            .NR10 => self.data[addr] | 0x80,
            .NR30 => self.data[addr] | 0x7F,
            .NR32 => self.data[addr] | 0x9F,
            .NR41 => self.data[addr] | 0xC0,
            .NR44 => self.data[addr] | 0x3F,
            .NR52 => self.data[addr] | 0x70,
            else => self.data[addr],
        };
    }

    /// Write to I/O register (addr is 0x00-0x7F, relative to 0xFF00)
    pub fn write(self: *IoRegisters, addr: u8, val: u8) void {
        if (isUnusedDmgRegister(addr)) return;
        const reg: IoReg = @enumFromInt(addr);
        switch (reg) {
            .JOYP => {
                // Only bits 4-5 are writable (select lines)
                const old_lines = self.readJoypad() & 0x0F;
                self.joypad_select = val & 0x30;
                self.requestJoypadEdge(old_lines);
            },
            .DIV => {
                // The bus normally routes this through Timer so the hidden
                // counter and falling-edge behavior are updated too.
                self.data[addr] = 0;
            },
            .LY => {
                // LY is read-only, writes are ignored
            },
            .STAT => {
                // Use setStat to handle writable bits properly
                self.setStat(val);
            },
            .TAC => {
                // The bus normally routes this through Timer so a change in
                // the selected counter bit can generate a falling edge.
                self.data[addr] = val | 0xF8; // Upper 5 bits always set
            },
            .KEY1 => {
                // Bit 0 arms a CGB speed switch; bit 7 is changed by STOP.
                self.data[addr] = (self.data[addr] & 0x80) | (val & 0x01);
            },
            .LCDC => {
                const was_enabled = (self.data[addr] & 0x80) != 0;
                self.data[addr] = val;
                const is_enabled = (val & 0x80) != 0;
                if (!was_enabled and is_enabled) self.updateCoincidence();
                self.updateStatInterrupt();
            },
            .LYC => {
                self.data[addr] = val;
                // The LY comparator is clock-gated with the PPU. Its existing
                // result is retained while LCDC is off, even as LYC changes.
                if ((self.data[@intFromEnum(IoReg.LCDC)] & 0x80) != 0) {
                    self.updateCoincidence();
                }
                self.updateStatInterrupt();
            },
            .SC => {
                // The bus routes this through Serial so transfer timing and
                // completion interrupts have one owner.
                self.data[addr] = val | 0x7E; // Bits 1-6 always set (DMG)
            },
            .NR52 => {
                // Only bit 7 is writable (sound on/off)
                if (val & 0x80 == 0) {
                    // Sound off - clear all sound registers
                    for (0x10..0x26) |i| {
                        self.data[i] = 0;
                    }
                }
                self.data[addr] = (self.data[addr] & 0x0F) | (val & 0x80);
            },
            else => {
                self.data[addr] = val;
            },
        }
    }

    fn isUnusedDmgRegister(addr: u8) bool {
        return addr == 0x03 or
            (addr >= 0x08 and addr <= 0x0E) or
            addr == 0x15 or
            addr == 0x1F or
            (addr >= 0x27 and addr <= 0x29) or
            addr >= 0x4C;
    }

    /// Read joypad register with proper button masking
    fn readJoypad(self: *const IoRegisters) u8 {
        var result: u8 = 0xCF; // Bits 6-7 always set, bits 0-3 default high

        if (self.joypad_select & 0x10 == 0) {
            // D-pad selected (active low)
            result &= 0xF0 | (self.joypad_buttons & 0x0F);
        }
        if (self.joypad_select & 0x20 == 0) {
            // Buttons selected (active low)
            result &= 0xF0 | ((self.joypad_buttons >> 4) & 0x0F);
        }

        return (result & 0x0F) | self.joypad_select | 0xC0;
    }

    /// Set joypad button state (for input handling)
    /// Buttons: bit 0=Right, 1=Left, 2=Up, 3=Down, 4=A, 5=B, 6=Select, 7=Start
    /// Use 0 for pressed, 1 for released
    pub fn setJoypadState(self: *IoRegisters, state: u8) void {
        const old_lines = self.readJoypad() & 0x0F;
        self.joypad_buttons = state;
        self.requestJoypadEdge(old_lines);
    }

    pub fn getJoypadState(self: *const IoRegisters) u8 {
        return self.joypad_buttons;
    }

    fn requestJoypadEdge(self: *IoRegisters, old_lines: u8) void {
        const new_lines = self.readJoypad() & 0x0F;
        if ((old_lines & ~new_lines) != 0) self.requestInterrupt(Interrupt.JOYPAD);
    }

    /// Request an interrupt by setting a bit in IF
    pub fn requestInterrupt(self: *IoRegisters, interrupt: u8) void {
        self.data[@intFromEnum(IoReg.IF)] |= interrupt;
    }

    /// Clear an interrupt flag
    pub fn clearInterrupt(self: *IoRegisters, interrupt: u8) void {
        self.data[@intFromEnum(IoReg.IF)] &= ~interrupt;
    }

    /// Get pending interrupts (IF & IE)
    pub fn getPendingInterrupts(self: *const IoRegisters, ie: u8) u8 {
        return self.data[@intFromEnum(IoReg.IF)] & ie & 0x1F;
    }

    /// Get serial output buffer (for test ROMs)
    pub fn getSerialOutput(self: *const IoRegisters) []const u8 {
        return self.serial_output.items;
    }

    pub fn captureSerialOutput(self: *IoRegisters, byte: u8) void {
        self.serial_output.append(self.allocator, byte) catch {};
        if (byte >= 0x20 and byte < 0x7F or byte == '\n' or byte == '\r') {
            std.debug.print("{c}", .{byte});
        }
    }

    // PPU register helpers
    pub fn getLcdc(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.LCDC)];
    }

    pub fn getPpuMode(self: *const IoRegisters) u2 {
        return @truncate(self.data[@intFromEnum(IoReg.STAT)] & 0x03);
    }

    pub fn getOamScanRow(self: *const IoRegisters) u8 {
        return self.oam_scan_row;
    }

    pub fn setOamScanRow(self: *IoRegisters, row: u8) void {
        self.oam_scan_row = row;
    }

    pub fn setPpuMemoryBlocked(self: *IoRegisters, oam: bool, vram: bool) void {
        self.setPpuMemoryBlockedDetailed(oam, oam, vram, vram);
    }

    pub fn setPpuMemoryBlockedDetailed(
        self: *IoRegisters,
        oam_read: bool,
        oam_write: bool,
        vram_read: bool,
        vram_write: bool,
    ) void {
        self.ppu_oam_read_blocked = oam_read;
        self.ppu_oam_write_blocked = oam_write;
        self.ppu_vram_read_blocked = vram_read;
        self.ppu_vram_write_blocked = vram_write;
    }

    pub fn getScy(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.SCY)];
    }

    pub fn getScx(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.SCX)];
    }

    pub fn getBgp(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.BGP)];
    }

    pub fn getObp0(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.OBP0)];
    }

    pub fn getObp1(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.OBP1)];
    }

    pub fn getLyc(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.LYC)];
    }

    pub fn getStat(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.STAT)];
    }

    pub fn getWy(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.WY)];
    }

    pub fn getWx(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.WX)];
    }

    pub fn setStat(self: *IoRegisters, stat: u8) void {
        // Bit 7 is unused, bits 0-2 are read-only (mode and LYC flag)
        // Only bits 3-6 are writable
        const writable_bits = stat & 0x78;
        const readonly_bits = self.data[@intFromEnum(IoReg.STAT)] & 0x07;
        self.data[@intFromEnum(IoReg.STAT)] = writable_bits | readonly_bits | 0x80;
        self.updateStatInterrupt();
    }

    pub fn setLy(self: *IoRegisters, ly: u8) void {
        self.data[@intFromEnum(IoReg.LY)] = ly;
        if ((self.data[@intFromEnum(IoReg.LCDC)] & 0x80) != 0) {
            self.updateCoincidence();
        }
        self.updateStatInterrupt();
    }

    /// At the start of a visible DMG scanline, LY changes one dot before the
    /// comparator latches the new line. During that short phase coincidence
    /// reads false even if the new LY equals LYC.
    pub fn beginVisibleLine(self: *IoRegisters, ly: u8) void {
        self.data[@intFromEnum(IoReg.LY)] = ly;
        self.data[@intFromEnum(IoReg.STAT)] &= ~@as(u8, 0x04);
        self.updateStatInterrupt();
    }

    pub fn latchLyCoincidence(self: *IoRegisters) void {
        self.updateCoincidence();
        self.updateStatInterrupt();
    }

    pub fn setPpuMode(self: *IoRegisters, mode: u2) void {
        self.data[@intFromEnum(IoReg.STAT)] =
            (self.data[@intFromEnum(IoReg.STAT)] & 0xFC) | mode | 0x80;
        self.updateStatInterrupt();
    }

    /// On DMG visible lines, the mode-2 STAT source rises one dot before the
    /// public STAT mode bits change from 0 to 2.
    pub fn preassertMode2Stat(self: *IoRegisters) void {
        const mode2_enabled = (self.data[@intFromEnum(IoReg.STAT)] & 0x20) != 0;
        if (mode2_enabled and !self.stat_irq_line) {
            self.requestInterrupt(Interrupt.LCD_STAT);
        }
        self.stat_irq_line = self.stat_irq_line or mode2_enabled;
    }

    fn updateCoincidence(self: *IoRegisters) void {
        const stat_index = @intFromEnum(IoReg.STAT);
        if (self.data[@intFromEnum(IoReg.LY)] == self.data[@intFromEnum(IoReg.LYC)]) {
            self.data[stat_index] |= 0x04;
        } else {
            self.data[stat_index] &= ~@as(u8, 0x04);
        }
    }

    fn updateStatInterrupt(self: *IoRegisters) void {
        const lcd_enabled = (self.data[@intFromEnum(IoReg.LCDC)] & 0x80) != 0;
        const stat = self.data[@intFromEnum(IoReg.STAT)];
        const mode = stat & 0x03;
        // LCD disable gates the mode sources, but the frozen LYC comparator
        // remains connected to the STAT edge detector. Consequently, a true
        // comparison retained across off -> on does not create a second edge.
        const line_high = ((stat & 0x40) != 0 and (stat & 0x04) != 0) or
            (lcd_enabled and (((stat & 0x20) != 0 and mode == 2) or
                ((stat & 0x10) != 0 and mode == 1) or
                ((stat & 0x08) != 0 and mode == 0)));

        if (line_high and !self.stat_irq_line) {
            self.requestInterrupt(Interrupt.LCD_STAT);
        }
        self.stat_irq_line = line_high;
    }
};

test "DMG unused IO bits and registers read high" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();

    for ([_]struct { addr: u8, mask: u8 }{
        .{ .addr = @intFromEnum(IoReg.NR10), .mask = 0x80 },
        .{ .addr = @intFromEnum(IoReg.NR30), .mask = 0x7F },
        .{ .addr = @intFromEnum(IoReg.NR32), .mask = 0x9F },
        .{ .addr = @intFromEnum(IoReg.NR41), .mask = 0xC0 },
        .{ .addr = @intFromEnum(IoReg.NR44), .mask = 0x3F },
        .{ .addr = @intFromEnum(IoReg.NR52), .mask = 0x70 },
    }) |case| {
        io.write(case.addr, 0);
        try std.testing.expectEqual(case.mask, io.read(case.addr) & case.mask);
    }

    for ([_]u8{ 0x03, 0x08, 0x15, 0x1F, 0x27, 0x4C, 0x7F }) |addr| {
        io.write(addr, 0);
        try std.testing.expectEqual(@as(u8, 0xFF), io.read(addr));
    }
}

test "LYC writes update coincidence and request STAT only on a rising edge" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();

    io.setLy(12);
    io.write(@intFromEnum(IoReg.STAT), 0x40);
    io.clearInterrupt(Interrupt.LCD_STAT);

    io.write(@intFromEnum(IoReg.LYC), 12);
    try std.testing.expect((io.getStat() & 0x04) != 0);
    try std.testing.expect((io.data[@intFromEnum(IoReg.IF)] & Interrupt.LCD_STAT) != 0);

    io.clearInterrupt(Interrupt.LCD_STAT);
    io.setPpuMode(2);
    try std.testing.expectEqual(@as(u8, 0), io.data[@intFromEnum(IoReg.IF)] & Interrupt.LCD_STAT);
}

test "LY coincidence freezes while LCD is disabled and restarts on enable" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();

    io.setLy(144);
    io.write(@intFromEnum(IoReg.LYC), 144);
    try std.testing.expect((io.getStat() & 0x04) != 0);

    io.write(@intFromEnum(IoReg.LCDC), 0);
    io.setLy(0);
    io.write(@intFromEnum(IoReg.LYC), 1);
    try std.testing.expect((io.getStat() & 0x04) != 0);

    io.write(@intFromEnum(IoReg.LCDC), 0x80);
    try std.testing.expectEqual(@as(u8, 0), io.getStat() & 0x04);

    io.write(@intFromEnum(IoReg.LCDC), 0);
    io.write(@intFromEnum(IoReg.LYC), 0);
    io.write(@intFromEnum(IoReg.STAT), 0x40);
    io.clearInterrupt(Interrupt.LCD_STAT);
    io.write(@intFromEnum(IoReg.LCDC), 0x80);
    try std.testing.expect((io.getStat() & 0x04) != 0);
    try std.testing.expect((io.data[@intFromEnum(IoReg.IF)] & Interrupt.LCD_STAT) != 0);
}

test "retained LY coincidence does not retrigger when LCD is enabled" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();

    io.setLy(144);
    io.write(@intFromEnum(IoReg.LYC), 144);
    io.write(@intFromEnum(IoReg.STAT), 0x40);
    io.clearInterrupt(Interrupt.LCD_STAT);

    io.write(@intFromEnum(IoReg.LCDC), 0);
    io.setLy(0);
    io.write(@intFromEnum(IoReg.LYC), 0);
    io.clearInterrupt(Interrupt.LCD_STAT);
    io.write(@intFromEnum(IoReg.LCDC), 0x80);

    try std.testing.expect((io.getStat() & 0x04) != 0);
    try std.testing.expectEqual(
        @as(u8, 0),
        io.data[@intFromEnum(IoReg.IF)] & Interrupt.LCD_STAT,
    );
}

test "visible line start delays the new LY comparison" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();

    io.write(@intFromEnum(IoReg.LYC), 1);
    io.beginVisibleLine(1);
    try std.testing.expectEqual(@as(u8, 0), io.getStat() & 0x04);

    io.latchLyCoincidence();
    try std.testing.expect((io.getStat() & 0x04) != 0);
}

test "mode 2 STAT source rises before the public mode changes" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();

    io.write(@intFromEnum(IoReg.STAT), 0x20);
    io.setPpuMode(0);
    io.clearInterrupt(Interrupt.LCD_STAT);
    io.preassertMode2Stat();
    try std.testing.expectEqual(@as(u2, 0), io.getPpuMode());
    try std.testing.expect((io.data[@intFromEnum(IoReg.IF)] & Interrupt.LCD_STAT) != 0);

    io.clearInterrupt(Interrupt.LCD_STAT);
    io.setPpuMode(2);
    try std.testing.expectEqual(@as(u8, 0), io.data[@intFromEnum(IoReg.IF)] & Interrupt.LCD_STAT);
}

test "joypad interrupt follows selected input lines" {
    var io = IoRegisters.init(std.testing.allocator);
    defer io.deinit();
    io.clearInterrupt(Interrupt.JOYPAD);

    // Pressing A while neither group is selected does not pull a JOYP input
    // line low.
    io.setJoypadState(0xEF);
    try std.testing.expectEqual(@as(u8, 0), io.data[@intFromEnum(IoReg.IF)] & Interrupt.JOYPAD);

    // Selecting the already-held button creates the required high-to-low edge.
    io.write(@intFromEnum(IoReg.JOYP), 0x10);
    try std.testing.expect((io.data[@intFromEnum(IoReg.IF)] & Interrupt.JOYPAD) != 0);
}
