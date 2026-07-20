const std = @import("std");
const sdl = @import("../sdl.zig");

const first_codepoint = 32;
const glyph_count = 95;
const atlas_width = 512;
const atlas_height = 512;
const baked_height: f32 = 36;
const embedded_font = @embedFile("debugger_font");

const BakedGlyph = extern struct {
    x0: u16,
    y0: u16,
    x1: u16,
    y1: u16,
    x_offset: f32,
    y_offset: f32,
    x_advance: f32,
};

comptime {
    if (@sizeOf(BakedGlyph) != 20) @compileError("stbtt_bakedchar ABI changed");
}

extern fn stbtt_BakeFontBitmap(
    data: [*c]const u8,
    offset: c_int,
    pixel_height: f32,
    pixels: [*c]u8,
    width: c_int,
    height: c_int,
    first_character: c_int,
    character_count: c_int,
    glyphs: [*c]BakedGlyph,
) c_int;

/// An immutable, antialiased ASCII atlas built once for the debugger window.
/// The only accepted font data is the trusted font embedded at build time.
pub const TextRenderer = struct {
    texture: *sdl.Texture,
    glyphs: [glyph_count]BakedGlyph,

    pub fn init(renderer: *sdl.Renderer) !TextRenderer {
        const allocator = std.heap.page_allocator;
        const pixel_count = atlas_width * atlas_height;
        const atlas = try allocator.alloc(u32, pixel_count);
        defer allocator.free(atlas);

        const atlas_bytes = std.mem.sliceAsBytes(atlas);
        @memset(atlas_bytes, 0);

        var glyphs: [glyph_count]BakedGlyph = undefined;
        const used_rows = stbtt_BakeFontBitmap(
            embedded_font.ptr,
            0,
            baked_height,
            atlas_bytes.ptr,
            atlas_width,
            atlas_height,
            first_codepoint,
            glyph_count,
            &glyphs,
        );
        if (used_rows <= 0) return error.FontAtlasBakeFailed;

        // Expand the one-channel atlas in place, from back to front, into the
        // native ARGB8888 pixels SDL expects. Backward expansion preserves the
        // unread alpha bytes at the front of the allocation.
        var index: usize = pixel_count;
        while (index > 0) {
            index -= 1;
            const alpha = atlas_bytes[index];
            atlas[index] = (@as(u32, alpha) << 24) | 0x00FF_FFFF;
        }

        const texture = try sdl.createTexture(
            renderer,
            sdl.PIXELFORMAT_ARGB8888,
            sdl.TEXTUREACCESS_STATIC,
            atlas_width,
            atlas_height,
        );
        errdefer sdl.destroyTexture(texture);
        try sdl.setTextureBlendMode(texture, sdl.BLENDMODE_BLEND);
        try sdl.setTextureScaleMode(texture, sdl.SCALEMODE_LINEAR);
        try sdl.updateTexture(texture, null, std.mem.sliceAsBytes(atlas), atlas_width * @sizeOf(u32));

        return .{ .texture = texture, .glyphs = glyphs };
    }

    pub fn deinit(self: *TextRenderer) void {
        sdl.destroyTexture(self.texture);
    }

    /// Draw text with `baseline_y` expressed in the debugger's logical pixels.
    pub fn draw(
        self: *const TextRenderer,
        renderer: *sdl.Renderer,
        text: []const u8,
        start_x: c_int,
        baseline_y: c_int,
        pixel_height: f32,
        rgb: u32,
        max_x: c_int,
    ) void {
        sdl.setTextureColor(self.texture, rgb) catch return;

        const scale = pixel_height / baked_height;
        var pen_x: f32 = @floatFromInt(start_x);
        const baseline: f32 = @floatFromInt(baseline_y);

        for (text) |raw| {
            const codepoint = if (raw >= first_codepoint and raw < first_codepoint + glyph_count)
                raw
            else
                '?';
            const glyph = self.glyphs[codepoint - first_codepoint];
            const left = pen_x + glyph.x_offset * scale;
            if (left >= @as(f32, @floatFromInt(max_x))) return;

            const width = @as(f32, @floatFromInt(glyph.x1 - glyph.x0)) * scale;
            const height = @as(f32, @floatFromInt(glyph.y1 - glyph.y0)) * scale;
            if (width > 0 and height > 0) {
                const source: sdl.Rect = .{
                    .x = @intCast(glyph.x0),
                    .y = @intCast(glyph.y0),
                    .w = @intCast(glyph.x1 - glyph.x0),
                    .h = @intCast(glyph.y1 - glyph.y0),
                };
                const destination: sdl.FRect = .{
                    .x = left,
                    .y = baseline + glyph.y_offset * scale,
                    .w = width,
                    .h = height,
                };
                sdl.renderCopyF(renderer, self.texture, &source, &destination) catch return;
            }
            pen_x += glyph.x_advance * scale;
        }
    }
};
