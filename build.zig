const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});
    const clap = b.dependency("clap", .{});
    const stb = b.dependency("stb", .{});
    const jetbrains_mono = b.dependency("jetbrains_mono", .{});

    // Public frontend-free package module for embedding, automation, and
    // future C/Python bindings.
    const core_module = b.addModule("nibble", .{
        .root_source_file = b.path("src/nibble.zig"),
        .target = target,
        .optimize = optimize,
    });

    // This creates an executable module.
    const exe = b.addExecutable(.{
        .name = "nibble",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    exe.root_module.addImport("clap", clap.module("clap"));
    configureFrontend(b, exe.root_module, stb, jetbrains_mono, target);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);
    b.getInstallStep().dependOn(&b.addInstallFile(
        stb.path("LICENSE"),
        "share/nibble/licenses/stb.txt",
    ).step);
    b.getInstallStep().dependOn(&b.addInstallFile(
        jetbrains_mono.path("OFL.txt"),
        "share/nibble/licenses/JetBrainsMono-OFL.txt",
    ).step);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const benchmark = b.addExecutable(.{
        .name = "nibble-bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/benchmark.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    benchmark.root_module.addImport("nibble", core_module);
    benchmark.root_module.addImport("clap", clap.module("clap"));
    const run_benchmark = b.addRunArtifact(benchmark);
    if (b.args) |args| run_benchmark.addArgs(args);
    const benchmark_step = b.step("bench", "Benchmark the frontend-free simulation core");
    benchmark_step.dependOn(&run_benchmark.step);

    const agent_benchmark = b.addExecutable(.{
        .name = "nibble-agent-bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/agent_benchmark.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    agent_benchmark.root_module.addImport("nibble", core_module);
    agent_benchmark.root_module.addImport("clap", clap.module("clap"));
    const run_agent_benchmark = b.addRunArtifact(agent_benchmark);
    if (b.args) |args| run_agent_benchmark.addArgs(args);
    const agent_benchmark_step = b.step("agent-bench", "Benchmark the complete agent workload");
    agent_benchmark_step.dependOn(&run_agent_benchmark.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const exe_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    exe_unit_tests.root_module.addImport("clap", clap.module("clap"));
    configureFrontend(b, exe_unit_tests.root_module, stb, jetbrains_mono, target);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const benchmark_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/benchmark.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    benchmark_unit_tests.root_module.addImport("nibble", core_module);
    benchmark_unit_tests.root_module.addImport("clap", clap.module("clap"));
    const run_benchmark_unit_tests = b.addRunArtifact(benchmark_unit_tests);

    const core_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/nibble.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_core_unit_tests = b.addRunArtifact(core_unit_tests);

    const agent_benchmark_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/agent_benchmark.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    agent_benchmark_unit_tests.root_module.addImport("nibble", core_module);
    agent_benchmark_unit_tests.root_module.addImport("clap", clap.module("clap"));
    const run_agent_benchmark_unit_tests = b.addRunArtifact(agent_benchmark_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
    test_step.dependOn(&run_benchmark_unit_tests.step);
    test_step.dependOn(&run_core_unit_tests.step);
    test_step.dependOn(&run_agent_benchmark_unit_tests.step);

    // SDL dependency for graphics and input
    // Use system compiler to avoid Zig's C frontend issues with ARM NEON headers
    exe.root_module.link_libc = true;
    exe.root_module.linkSystemLibrary("sdl2", .{ .use_pkg_config = .force });
}

fn configureFrontend(
    b: *std.Build,
    module: *std.Build.Module,
    stb: *std.Build.Dependency,
    jetbrains_mono: *std.Build.Dependency,
    target: std.Build.ResolvedTarget,
) void {
    module.link_libc = true;
    if (target.result.os.tag != .windows) module.linkSystemLibrary("m", .{});
    module.addIncludePath(stb.path("."));
    module.addCSourceFile(.{ .file = b.path("src/frontend/stb_truetype_impl.c") });
    module.addAnonymousImport("debugger_font", .{
        .root_source_file = jetbrains_mono.path("fonts/ttf/JetBrainsMono-Regular.ttf"),
    });
}
