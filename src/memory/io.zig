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

    // Timer internal state (DIV is upper 8 bits of 16-bit counter)
    div_counter: u16,

    // Joypad state
    joypad_select: u8, // Which buttons are selected (bits 4-5 of JOYP)
    joypad_buttons: u8, // Button state: bits 0-3 = D-pad, bits 4-7 = buttons
    // Bit layout: Start, Select, B, A (bits 4-7), Down, Up, Left, Right (bits 0-3)
    // 0 = pressed, 1 = not pressed

    // DMA state
    dma_active: bool,
    dma_source: u16,
    dma_offset: u8,
    dma_delay: u8,

    // Serial output (for test ROMs)
    allocator: std.mem.Allocator,
    serial_output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) IoRegisters {
        var io = IoRegisters{
            .data = [_]u8{0} ** 0x80,
            .div_counter = 0,
            .joypad_select = 0x30, // Neither selected
            .joypad_buttons = 0xFF, // All buttons released
            .dma_active = false,
            .dma_source = 0,
            .dma_offset = 0,
            .dma_delay = 0,
            .allocator = allocator,
            .serial_output = std.ArrayList(u8){},
        };

        // Set initial values for some registers (post-boot ROM values)
        io.data[@intFromEnum(IoReg.JOYP)] = 0xCF;
        io.data[@intFromEnum(IoReg.IF)] = 0xE1;
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
        const reg: IoReg = @enumFromInt(addr);
        return switch (reg) {
            .JOYP => self.readJoypad(),
            .DIV => @truncate(self.div_counter >> 8),
            .LY => self.data[addr], // Read-only PPU scanline
            .STAT => self.data[addr] | 0x80, // Bit 7 always set
            .IF => self.data[addr] | 0xE0, // Upper 3 bits always set
            else => self.data[addr],
        };
    }

    /// Write to I/O register (addr is 0x00-0x7F, relative to 0xFF00)
    pub fn write(self: *IoRegisters, addr: u8, val: u8) void {
        const reg: IoReg = @enumFromInt(addr);
        switch (reg) {
            .JOYP => {
                // Only bits 4-5 are writable (select lines)
                self.joypad_select = val & 0x30;
            },
            .DIV => {
                // Any write resets DIV to 0
                self.div_counter = 0;
            },
            .LY => {
                // LY is read-only, writes are ignored
            },
            .STAT => {
                // Use setStat to handle writable bits properly
                self.setStat(val);
            },
            .DMA => {
                // Start DMA transfer
                self.data[addr] = val;
                self.dma_active = true;
                self.dma_source = @as(u16, val) << 8;
                self.dma_offset = 0;
                self.dma_delay = 2; // 2 cycle delay before DMA starts
            },
            .TAC => {
                self.data[addr] = val | 0xF8; // Upper 5 bits always set
            },
            .SC => {
                // Serial control - capture output when transfer is initiated
                if (val == 0x81) {
                    // Transfer requested (internal clock)
                    const byte = self.data[@intFromEnum(IoReg.SB)];
                    self.serial_output.append(self.allocator, byte) catch {};

                    // Print to stderr immediately for test ROM output
                    if (byte >= 0x20 and byte < 0x7F or byte == '\n' or byte == '\r') {
                        std.debug.print("{c}", .{byte});
                    }
                }
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
        self.joypad_buttons = state;
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

    // PPU register helpers
    pub fn getLcdc(self: *const IoRegisters) u8 {
        return self.data[@intFromEnum(IoReg.LCDC)];
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
    }

    pub fn setLy(self: *IoRegisters, ly: u8) void {
        self.data[@intFromEnum(IoReg.LY)] = ly;

        // Check LY=LYC comparison
        const lyc = self.getLyc();
        const stat = self.getStat();

        // Update LYC flag (bit 2) in STAT
        if (ly == lyc) {
            self.data[@intFromEnum(IoReg.STAT)] |= 0x04; // Set LYC=LY flag

            // Trigger STAT interrupt if LYC interrupt is enabled (bit 6)
            if (stat & 0x40 != 0) {
                self.requestInterrupt(Interrupt.LCD_STAT);
            }
        } else {
            self.data[@intFromEnum(IoReg.STAT)] &= ~@as(u8, 0x04); // Clear LYC=LY flag
        }
    }
};
