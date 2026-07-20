const std = @import("std");

pub const MASTER_CLOCK: u32 = 4_194_304;
pub const SAMPLE_RATE: u32 = 48_000;
const SAMPLE_BUFFER_CAPACITY = 256;

pub const StereoSample = extern struct {
    left: i16,
    right: i16,
};

const PulseChannel = struct {
    enabled: bool = false,
    dac_enabled: bool = false,
    length: u8 = 0,
    length_enabled: bool = false,
    timer: u16 = 0,
    duty_step: u3 = 0,
    volume: u4 = 0,
    envelope_timer: u4 = 0,
    envelope_running: bool = false,
};

const WaveChannel = struct {
    enabled: bool = false,
    dac_enabled: bool = false,
    length: u16 = 0,
    length_enabled: bool = false,
    timer: u16 = 0,
    sample_index: u5 = 0,
    sample_buffer: u8 = 0,
    accessed_byte: u4 = 0,
    access_window: bool = false,
};

const NoiseChannel = struct {
    enabled: bool = false,
    dac_enabled: bool = false,
    length: u8 = 0,
    length_enabled: bool = false,
    timer: u32 = 0,
    lfsr: u15 = 0,
    volume: u4 = 0,
    envelope_timer: u4 = 0,
    envelope_running: bool = false,
};

/// Dot-clocked DMG audio processing unit.
///
/// The CPU-visible registers, frame sequencer, channel generators, and sample
/// mixer live together because writes can alter all four in the same T-cycle.
/// SDL only consumes the resulting samples and has no authority over timing.
pub const Apu = struct {
    // FF10-FF3F. FF27-FF2F are physically absent but keeping one contiguous
    // array makes register and wave-RAM addressing explicit.
    regs: [0x30]u8 = [_]u8{0} ** 0x30,
    powered: bool = true,
    frame_step: u3 = 0,

    pulse1: PulseChannel = .{},
    pulse2: PulseChannel = .{},
    wave: WaveChannel = .{},
    noise: NoiseChannel = .{},

    sweep_shadow: u11 = 0,
    sweep_timer: u4 = 0,
    sweep_enabled: bool = false,
    sweep_negate_used: bool = false,

    sample_accumulator: u32 = 0,
    high_pass_left: f32 = 0,
    high_pass_right: f32 = 0,
    samples: [SAMPLE_BUFFER_CAPACITY]StereoSample =
        [_]StereoSample{.{ .left = 0, .right = 0 }} ** SAMPLE_BUFFER_CAPACITY,
    sample_count: u16 = 0,

    pub fn init() Apu {
        var apu: Apu = .{};
        apu.loadPostBootState();
        return apu;
    }

    pub fn reset(self: *Apu) void {
        self.* = init();
    }

    pub fn isRegister(addr: u8) bool {
        return addr >= 0x10 and addr <= 0x3F;
    }

    pub fn read(self: *const Apu, addr: u8) u8 {
        std.debug.assert(isRegister(addr));
        if (addr >= 0x30) return self.readWave(addr);
        if (addr >= 0x27) return 0xFF;
        if (addr == 0x26) {
            return 0x70 |
                (@as(u8, @intFromBool(self.powered)) << 7) |
                @as(u8, @intFromBool(self.pulse1.enabled)) |
                (@as(u8, @intFromBool(self.pulse2.enabled)) << 1) |
                (@as(u8, @intFromBool(self.wave.enabled)) << 2) |
                (@as(u8, @intFromBool(self.noise.enabled)) << 3);
        }
        return self.regs[regIndex(addr)] | readMask(addr);
    }

    pub fn write(self: *Apu, addr: u8, value: u8) void {
        std.debug.assert(isRegister(addr));

        if (addr >= 0x30) {
            self.writeWave(addr, value);
            return;
        }
        if (addr == 0x26) {
            self.writePower(value);
            return;
        }
        if (addr >= 0x27) return;

        if (!self.powered) {
            // On monochrome hardware, length counters remain writable while
            // powered down even though the register latches stay cleared.
            switch (addr) {
                0x11 => self.pulse1.length = 64 -% (value & 0x3F),
                0x16 => self.pulse2.length = 64 -% (value & 0x3F),
                0x1B => self.wave.length = 256 -% @as(u16, value),
                0x20 => self.noise.length = 64 -% (value & 0x3F),
                else => {},
            }
            return;
        }

        switch (addr) {
            0x10 => self.writeSweep(value),
            0x11 => {
                self.regs[regIndex(addr)] = value;
                self.pulse1.length = 64 -% (value & 0x3F);
            },
            0x12 => self.writePulseEnvelope(1, value),
            0x13 => self.regs[regIndex(addr)] = value,
            0x14 => self.writePulseControl(1, value),
            0x15 => {},
            0x16 => {
                self.regs[regIndex(addr)] = value;
                self.pulse2.length = 64 -% (value & 0x3F);
            },
            0x17 => self.writePulseEnvelope(2, value),
            0x18 => self.regs[regIndex(addr)] = value,
            0x19 => self.writePulseControl(2, value),
            0x1A => {
                self.regs[regIndex(addr)] = value & 0x80;
                self.wave.dac_enabled = (value & 0x80) != 0;
                if (!self.wave.dac_enabled) self.wave.enabled = false;
            },
            0x1B => {
                self.regs[regIndex(addr)] = value;
                self.wave.length = 256 -% @as(u16, value);
            },
            0x1C => self.regs[regIndex(addr)] = value & 0x60,
            0x1D => self.regs[regIndex(addr)] = value,
            0x1E => self.writeWaveControl(value),
            0x1F => {},
            0x20 => {
                self.regs[regIndex(addr)] = value & 0x3F;
                self.noise.length = 64 -% (value & 0x3F);
            },
            0x21 => self.writeNoiseEnvelope(value),
            0x22 => self.regs[regIndex(addr)] = value,
            0x23 => self.writeNoiseControl(value),
            0x24, 0x25 => self.regs[regIndex(addr)] = value,
            else => unreachable,
        }
    }

    /// Advance generators and the DIV-APU frame sequencer from the system
    /// counter value that was visible before this batch.
    pub fn tick(self: *Apu, cycles: u8, divider_start: u16) void {
        var divider = divider_start;
        var remaining = cycles;
        while (remaining > 0) : (remaining -= 1) {
            self.wave.access_window = false;

            if (self.powered) {
                self.tickPulse(&self.pulse1, self.pulseFrequency(1));
                self.tickPulse(&self.pulse2, self.pulseFrequency(2));
                self.tickWave();
                self.tickNoise();
            }

            const old_div_apu = (divider & 0x1000) != 0;
            divider +%= 1;
            if (old_div_apu and (divider & 0x1000) == 0) self.clockFrameSequencer();

            self.sample_accumulator += SAMPLE_RATE;
            if (self.sample_accumulator >= MASTER_CLOCK) {
                self.sample_accumulator -= MASTER_CLOCK;
                self.emitSample();
            }
        }
    }

    /// Resetting DIV clocks DIV-APU immediately if its input was high.
    pub fn dividerReset(self: *Apu, old_divider: u16) void {
        if ((old_divider & 0x1000) != 0) self.clockFrameSequencer();
    }

    pub fn pendingSamples(self: *const Apu) []const StereoSample {
        return self.samples[0..self.sample_count];
    }

    pub fn discardSamples(self: *Apu) void {
        self.sample_count = 0;
    }

    fn loadPostBootState(self: *Apu) void {
        self.powered = true;
        self.regs[regIndex(0x10)] = 0x00;
        self.regs[regIndex(0x11)] = 0x80;
        self.regs[regIndex(0x12)] = 0xF3;
        self.regs[regIndex(0x24)] = 0x77;
        self.regs[regIndex(0x25)] = 0xF3;
        self.pulse1.enabled = true;
        self.pulse1.dac_enabled = true;
        self.pulse1.volume = 0x0F;
    }

    fn writePower(self: *Apu, value: u8) void {
        const enable = (value & 0x80) != 0;
        if (!enable and self.powered) {
            const pulse1_length = self.pulse1.length;
            const pulse2_length = self.pulse2.length;
            const wave_length = self.wave.length;
            const noise_length = self.noise.length;
            self.powered = false;
            @memset(self.regs[0..regIndex(0x26)], 0);
            // Power loss resets every channel latch except the four DMG
            // length counters, which remain clocked and writable.
            self.pulse1 = .{ .length = pulse1_length };
            self.pulse2 = .{ .length = pulse2_length };
            self.wave = .{ .length = wave_length };
            self.noise = .{ .length = noise_length };
            self.sweep_enabled = false;
            self.sweep_negate_used = false;
            self.high_pass_left = 0;
            self.high_pass_right = 0;
        } else if (enable and !self.powered) {
            self.powered = true;
            self.frame_step = 0;
            self.sweep_timer = 0;
        }
    }

    fn writeSweep(self: *Apu, value: u8) void {
        const old = self.regs[regIndex(0x10)];
        self.regs[regIndex(0x10)] = value & 0x7F;
        if ((old & 0x08) != 0 and (value & 0x08) == 0 and self.sweep_negate_used) {
            self.pulse1.enabled = false;
        }
    }

    fn writePulseEnvelope(self: *Apu, comptime channel: u2, value: u8) void {
        self.regs[regIndex(if (channel == 1) 0x12 else 0x17)] = value;
        const pulse = if (channel == 1) &self.pulse1 else &self.pulse2;
        pulse.dac_enabled = (value & 0xF8) != 0;
        if (!pulse.dac_enabled) pulse.enabled = false;
    }

    fn writeNoiseEnvelope(self: *Apu, value: u8) void {
        self.regs[regIndex(0x21)] = value;
        self.noise.dac_enabled = (value & 0xF8) != 0;
        if (!self.noise.dac_enabled) self.noise.enabled = false;
    }

    fn writePulseControl(self: *Apu, comptime channel: u2, value: u8) void {
        const addr: u8 = if (channel == 1) 0x14 else 0x19;
        const pulse = if (channel == 1) &self.pulse1 else &self.pulse2;
        const old_length_enabled = pulse.length_enabled;
        pulse.length_enabled = (value & 0x40) != 0;
        self.regs[regIndex(addr)] = value & 0x47;

        self.applyLengthWrite(
            &pulse.length,
            &pulse.enabled,
            old_length_enabled,
            pulse.length_enabled,
            (value & 0x80) != 0,
            64,
        );
        if ((value & 0x80) != 0) self.triggerPulse(channel);
    }

    fn writeWaveControl(self: *Apu, value: u8) void {
        const was_active = self.wave.enabled;
        const old_length_enabled = self.wave.length_enabled;
        self.wave.length_enabled = (value & 0x40) != 0;
        self.regs[regIndex(0x1E)] = value & 0x47;

        self.applyLengthWrite(
            &self.wave.length,
            &self.wave.enabled,
            old_length_enabled,
            self.wave.length_enabled,
            (value & 0x80) != 0,
            256,
        );
        if ((value & 0x80) != 0) {
            // Retrigger corruption is sampled two dots before the next wave
            // fetch, not during the CPU wave-RAM access window.
            if (was_active and self.wave.timer == 2) self.corruptWaveOnRetrigger();
            self.triggerWave();
        }
    }

    fn writeNoiseControl(self: *Apu, value: u8) void {
        const old_length_enabled = self.noise.length_enabled;
        self.noise.length_enabled = (value & 0x40) != 0;
        self.regs[regIndex(0x23)] = value & 0x40;

        self.applyLengthWrite(
            &self.noise.length,
            &self.noise.enabled,
            old_length_enabled,
            self.noise.length_enabled,
            (value & 0x80) != 0,
            64,
        );
        if ((value & 0x80) != 0) self.triggerNoise();
    }

    fn applyLengthWrite(
        self: *Apu,
        length: anytype,
        enabled: *bool,
        old_length_enabled: bool,
        new_length_enabled: bool,
        trigger: bool,
        comptime maximum: comptime_int,
    ) void {
        const extra_clock = !self.nextStepClocksLength();
        if (!old_length_enabled and new_length_enabled and extra_clock and length.* != 0) {
            length.* -= 1;
            if (length.* == 0) enabled.* = false;
        }
        if (trigger and length.* == 0) {
            length.* = maximum;
            if (new_length_enabled and extra_clock) length.* -= 1;
        }
    }

    fn triggerPulse(self: *Apu, comptime channel: u2) void {
        const pulse = if (channel == 1) &self.pulse1 else &self.pulse2;
        const envelope = self.regs[regIndex(if (channel == 1) 0x12 else 0x17)];
        pulse.enabled = pulse.dac_enabled;
        pulse.timer = pulsePeriod(self.pulseFrequency(channel));
        pulse.volume = @truncate(envelope >> 4);
        pulse.envelope_timer = envelopePeriod(envelope);
        if (self.frame_step == 7) pulse.envelope_timer += 1;
        pulse.envelope_running = true;

        if (channel == 1) {
            self.sweep_shadow = self.pulseFrequency(1);
            const sweep = self.regs[regIndex(0x10)];
            self.sweep_timer = sweepPeriod(sweep);
            self.sweep_enabled = (sweep & 0x77) != 0;
            self.sweep_negate_used = false;
            if ((sweep & 0x07) != 0) _ = self.calculateSweep(true);
        }
    }

    fn triggerWave(self: *Apu) void {
        self.wave.enabled = self.wave.dac_enabled;
        // Channel 3's trigger pipeline delays the first byte fetch by six
        // dots in addition to the normal frequency period.
        self.wave.timer = wavePeriod(self.waveFrequency()) + 6;
        self.wave.sample_index = 0;
    }

    fn triggerNoise(self: *Apu) void {
        const envelope = self.regs[regIndex(0x21)];
        self.noise.enabled = self.noise.dac_enabled;
        self.noise.timer = noisePeriod(self.regs[regIndex(0x22)]);
        self.noise.lfsr = 0;
        self.noise.volume = @truncate(envelope >> 4);
        self.noise.envelope_timer = envelopePeriod(envelope);
        if (self.frame_step == 7) self.noise.envelope_timer += 1;
        self.noise.envelope_running = true;
    }

    fn clockFrameSequencer(self: *Apu) void {
        if (!self.powered) return;
        const step = self.frame_step;
        if ((step & 1) == 0) self.clockLengths();
        if (step == 2 or step == 6) self.clockSweep();
        if (step == 7) {
            self.clockEnvelope(&self.pulse1, self.regs[regIndex(0x12)]);
            self.clockEnvelope(&self.pulse2, self.regs[regIndex(0x17)]);
            self.clockNoiseEnvelope();
        }
        self.frame_step +%= 1;
    }

    fn nextStepClocksLength(self: *const Apu) bool {
        return (self.frame_step & 1) == 0;
    }

    fn clockLengths(self: *Apu) void {
        clockLength(&self.pulse1.length, self.pulse1.length_enabled, &self.pulse1.enabled);
        clockLength(&self.pulse2.length, self.pulse2.length_enabled, &self.pulse2.enabled);
        clockLength(&self.wave.length, self.wave.length_enabled, &self.wave.enabled);
        clockLength(&self.noise.length, self.noise.length_enabled, &self.noise.enabled);
    }

    fn clockLength(length: anytype, length_enabled: bool, enabled: *bool) void {
        if (!length_enabled or length.* == 0) return;
        length.* -= 1;
        if (length.* == 0) enabled.* = false;
    }

    fn clockEnvelope(self: *Apu, pulse: *PulseChannel, envelope: u8) void {
        _ = self;
        if (!pulse.envelope_running) return;
        pulse.envelope_timer -|= 1;
        if (pulse.envelope_timer != 0) return;
        pulse.envelope_timer = envelopePeriod(envelope);
        if ((envelope & 0x07) == 0) return;
        if ((envelope & 0x08) != 0) {
            if (pulse.volume == 15) {
                pulse.envelope_running = false;
            } else {
                pulse.volume += 1;
            }
        } else if (pulse.volume == 0) {
            pulse.envelope_running = false;
        } else {
            pulse.volume -= 1;
        }
    }

    fn clockNoiseEnvelope(self: *Apu) void {
        const envelope = self.regs[regIndex(0x21)];
        if (!self.noise.envelope_running) return;
        self.noise.envelope_timer -|= 1;
        if (self.noise.envelope_timer != 0) return;
        self.noise.envelope_timer = envelopePeriod(envelope);
        if ((envelope & 0x07) == 0) return;
        if ((envelope & 0x08) != 0) {
            if (self.noise.volume == 15) {
                self.noise.envelope_running = false;
            } else {
                self.noise.volume += 1;
            }
        } else if (self.noise.volume == 0) {
            self.noise.envelope_running = false;
        } else {
            self.noise.volume -= 1;
        }
    }

    fn clockSweep(self: *Apu) void {
        self.sweep_timer -|= 1;
        if (self.sweep_timer != 0) return;
        const sweep = self.regs[regIndex(0x10)];
        self.sweep_timer = sweepPeriod(sweep);
        if (!self.sweep_enabled or (sweep & 0x70) == 0) return;
        const frequency = self.calculateSweep(true) orelse return;
        if ((sweep & 0x07) != 0) {
            self.sweep_shadow = frequency;
            self.setPulse1Frequency(frequency);
        }
        _ = self.calculateSweep(true);
    }

    fn calculateSweep(self: *Apu, disable_on_overflow: bool) ?u11 {
        const sweep = self.regs[regIndex(0x10)];
        const shift: u3 = @truncate(sweep & 0x07);
        const delta = self.sweep_shadow >> shift;
        const subtract = (sweep & 0x08) != 0;
        const result: u12 = if (subtract) blk: {
            self.sweep_negate_used = true;
            break :blk self.sweep_shadow - delta;
        } else @as(u12, self.sweep_shadow) + delta;
        if (result > 0x7FF) {
            if (disable_on_overflow) self.pulse1.enabled = false;
            return null;
        }
        return @truncate(result);
    }

    fn tickPulse(self: *Apu, pulse: *PulseChannel, frequency: u11) void {
        _ = self;
        if (pulse.timer > 1) {
            pulse.timer -= 1;
            return;
        }
        pulse.timer = pulsePeriod(frequency);
        pulse.duty_step +%= 1;
    }

    fn tickWave(self: *Apu) void {
        if (self.wave.timer > 1) {
            self.wave.timer -= 1;
            return;
        }
        self.wave.timer = wavePeriod(self.waveFrequency());
        self.wave.sample_index +%= 1;
        self.wave.accessed_byte = @truncate(self.wave.sample_index >> 1);
        self.wave.sample_buffer = self.regs[regIndex(0x30) + self.wave.accessed_byte];
        self.wave.access_window = true;
    }

    fn tickNoise(self: *Apu) void {
        if (self.noise.timer > 1) {
            self.noise.timer -= 1;
            return;
        }
        self.noise.timer = noisePeriod(self.regs[regIndex(0x22)]);
        const feedback: u15 = @intFromBool((self.noise.lfsr & 1) == ((self.noise.lfsr >> 1) & 1));
        self.noise.lfsr = (self.noise.lfsr >> 1) | (feedback << 14);
        if ((self.regs[regIndex(0x22)] & 0x08) != 0) {
            self.noise.lfsr = (self.noise.lfsr & ~@as(u15, 0x40)) | (feedback << 6);
        }
    }

    fn emitSample(self: *Apu) void {
        if (self.sample_count == SAMPLE_BUFFER_CAPACITY) return;

        const outputs = [_]f32{
            self.channelAnalog(self.pulseOutput(1), self.pulse1.dac_enabled),
            self.channelAnalog(self.pulseOutput(2), self.pulse2.dac_enabled),
            self.channelAnalog(self.waveOutput(), self.wave.dac_enabled),
            self.channelAnalog(self.noiseOutput(), self.noise.dac_enabled),
        };
        const nr50 = self.regs[regIndex(0x24)];
        const nr51 = self.regs[regIndex(0x25)];
        var left: f32 = 0;
        var right: f32 = 0;
        for (outputs, 0..) |output, channel| {
            if ((nr51 & (@as(u8, 1) << @intCast(channel))) != 0) right += output;
            if ((nr51 & (@as(u8, 0x10) << @intCast(channel))) != 0) left += output;
        }
        left *= @as(f32, @floatFromInt(((nr50 >> 4) & 7) + 1)) / 32.0;
        right *= @as(f32, @floatFromInt((nr50 & 7) + 1)) / 32.0;

        const any_dac = self.pulse1.dac_enabled or self.pulse2.dac_enabled or
            self.wave.dac_enabled or self.noise.dac_enabled;
        if (any_dac) {
            const filtered_left = left - self.high_pass_left;
            const filtered_right = right - self.high_pass_right;
            self.high_pass_left = left - filtered_left * 0.996;
            self.high_pass_right = right - filtered_right * 0.996;
            left = filtered_left;
            right = filtered_right;
        } else {
            left = 0;
            right = 0;
            self.high_pass_left = 0;
            self.high_pass_right = 0;
        }

        self.samples[self.sample_count] = .{
            .left = floatToSample(left),
            .right = floatToSample(right),
        };
        self.sample_count += 1;
    }

    fn pulseOutput(self: *const Apu, comptime channel: u2) u4 {
        const pulse = if (channel == 1) &self.pulse1 else &self.pulse2;
        if (!pulse.enabled) return 0;
        const duty_reg = self.regs[regIndex(if (channel == 1) 0x11 else 0x16)];
        const duty: u2 = @truncate(duty_reg >> 6);
        const patterns = [_]u8{ 0b0000_0001, 0b1000_0001, 0b1000_0111, 0b0111_1110 };
        return if ((patterns[duty] & (@as(u8, 1) << pulse.duty_step)) != 0) pulse.volume else 0;
    }

    fn waveOutput(self: *const Apu) u4 {
        if (!self.wave.enabled) return 0;
        const raw: u4 = if ((self.wave.sample_index & 1) == 0)
            @truncate(self.wave.sample_buffer >> 4)
        else
            @truncate(self.wave.sample_buffer);
        return switch ((self.regs[regIndex(0x1C)] >> 5) & 3) {
            0 => 0,
            1 => raw,
            2 => raw >> 1,
            3 => raw >> 2,
            else => unreachable,
        };
    }

    fn noiseOutput(self: *const Apu) u4 {
        if (!self.noise.enabled) return 0;
        return if ((self.noise.lfsr & 1) != 0) self.noise.volume else 0;
    }

    fn channelAnalog(self: *const Apu, digital: u4, dac_enabled: bool) f32 {
        _ = self;
        if (!dac_enabled) return 0;
        return (7.5 - @as(f32, @floatFromInt(digital))) / 7.5;
    }

    fn readWave(self: *const Apu, addr: u8) u8 {
        if (!self.wave.enabled) return self.regs[regIndex(addr)];
        if (!self.wave.access_window) return 0xFF;
        return self.regs[regIndex(0x30) + self.wave.accessed_byte];
    }

    fn writeWave(self: *Apu, addr: u8, value: u8) void {
        if (!self.wave.enabled) {
            self.regs[regIndex(addr)] = value;
        } else if (self.wave.access_window) {
            self.regs[regIndex(0x30) + self.wave.accessed_byte] = value;
        }
    }

    fn corruptWaveOnRetrigger(self: *Apu) void {
        const current: u4 = @truncate(((self.wave.sample_index +% 1) >> 1) & 0x0F);
        if (current < 4) {
            self.regs[regIndex(0x30)] = self.regs[regIndex(0x30) + current];
            return;
        }
        const source = current & 0x0C;
        var copy: [4]u8 = undefined;
        @memcpy(&copy, self.regs[regIndex(0x30) + source ..][0..4]);
        @memcpy(self.regs[regIndex(0x30)..][0..4], &copy);
    }

    fn pulseFrequency(self: *const Apu, comptime channel: u2) u11 {
        const low_addr: u8 = if (channel == 1) 0x13 else 0x18;
        const high_addr: u8 = if (channel == 1) 0x14 else 0x19;
        return @truncate(
            @as(u16, self.regs[regIndex(low_addr)]) |
                (@as(u16, self.regs[regIndex(high_addr)] & 7) << 8),
        );
    }

    fn setPulse1Frequency(self: *Apu, frequency: u11) void {
        self.regs[regIndex(0x13)] = @truncate(frequency);
        self.regs[regIndex(0x14)] =
            (self.regs[regIndex(0x14)] & 0x40) | @as(u8, @truncate(frequency >> 8));
    }

    fn waveFrequency(self: *const Apu) u11 {
        return @truncate(
            @as(u16, self.regs[regIndex(0x1D)]) |
                (@as(u16, self.regs[regIndex(0x1E)] & 7) << 8),
        );
    }
};

inline fn regIndex(addr: u8) usize {
    return addr - 0x10;
}

fn readMask(addr: u8) u8 {
    return switch (addr) {
        0x10 => 0x80,
        0x11 => 0x3F,
        0x12 => 0x00,
        0x13 => 0xFF,
        0x14 => 0xBF,
        0x15 => 0xFF,
        0x16 => 0x3F,
        0x17 => 0x00,
        0x18 => 0xFF,
        0x19 => 0xBF,
        0x1A => 0x7F,
        0x1B => 0xFF,
        0x1C => 0x9F,
        0x1D => 0xFF,
        0x1E => 0xBF,
        0x1F, 0x20 => 0xFF,
        0x21, 0x22 => 0x00,
        0x23 => 0xBF,
        0x24, 0x25 => 0x00,
        else => unreachable,
    };
}

inline fn pulsePeriod(frequency: u11) u16 {
    return (@as(u16, 2048) - frequency) * 4;
}

inline fn wavePeriod(frequency: u11) u16 {
    return (@as(u16, 2048) - frequency) * 2;
}

fn noisePeriod(nr43: u8) u32 {
    const divisor_code = nr43 & 7;
    const divisor: u32 = if (divisor_code == 0) 8 else @as(u32, divisor_code) * 16;
    return divisor << @intCast(nr43 >> 4);
}

inline fn envelopePeriod(envelope: u8) u4 {
    const period: u4 = @truncate(envelope & 7);
    return if (period == 0) 8 else period;
}

inline fn sweepPeriod(sweep: u8) u4 {
    const period: u4 = @truncate((sweep >> 4) & 7);
    return if (period == 0) 8 else period;
}

fn floatToSample(value: f32) i16 {
    const scaled = std.math.clamp(value, -1.0, 1.0) * 32767.0;
    return @intFromFloat(scaled);
}

test "DMG audio registers expose masks and power gating" {
    var apu = Apu.init();
    apu.write(0x26, 0);
    try std.testing.expectEqual(@as(u8, 0x70), apu.read(0x26));

    apu.write(0x12, 0xF3);
    apu.write(0x11, 0xFF);
    apu.write(0x30, 0xA5);
    apu.write(0x26, 0x80);
    try std.testing.expectEqual(@as(u8, 0x3F), apu.read(0x11));
    try std.testing.expectEqual(@as(u8, 0x00), apu.read(0x12));
    try std.testing.expectEqual(@as(u8, 0xA5), apu.read(0x30));
    try std.testing.expectEqual(@as(u8, 0xF0), apu.read(0x26));
}

test "length expiration clears the channel status bit" {
    var apu = Apu.init();
    apu.write(0x26, 0);
    apu.write(0x26, 0x80);
    apu.write(0x12, 0x08);
    apu.write(0x11, 0x3F);
    apu.write(0x14, 0xC0);
    try std.testing.expect((apu.read(0x26) & 1) != 0);

    apu.clockFrameSequencer();
    try std.testing.expectEqual(@as(u8, 0), apu.read(0x26) & 1);
}

test "sweep overflow disables pulse channel one on trigger" {
    var apu = Apu.init();
    apu.write(0x26, 0);
    apu.write(0x26, 0x80);
    apu.write(0x12, 0x08);
    apu.write(0x10, 0x01);
    apu.write(0x13, 0xFF);
    apu.write(0x14, 0x87);
    try std.testing.expectEqual(@as(u8, 0), apu.read(0x26) & 1);
}

test "sample clock emits 48 kHz stereo frames" {
    var apu = Apu.init();
    apu.discardSamples();
    apu.tick(255, 0);
    try std.testing.expectEqual(@as(usize, 2), apu.pendingSamples().len);
}
