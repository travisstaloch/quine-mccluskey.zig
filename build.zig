const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const lib = b.addStaticLibrary(.{
        .name = "quine-mccluskey",
        .root_source_file = .{ .path = "src/quine-mccluskey.zig" },
        .target = target,
        .optimize = optimize,
    });
    lib.install();

    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/tests.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    const simplify_exe = b.addExecutable(.{
        .name = "simplify",
        .root_source_file = .{ .path = "simplify-c-defines-zig/simplify.zig" },
        .target = target,
        .optimize = optimize,
    });
    simplify_exe.install();
    simplify_exe.addAnonymousModule(
        "quine-mccluskey",
        .{ .source_file = .{ .path = "src/quine-mccluskey.zig" } },
    );

    const simplify_tests = b.addTest(.{
        .root_source_file = .{ .path = "simplify-c-defines-zig/test_simplify.zig" },
        .target = target,
        .optimize = optimize,
    });
    simplify_tests.step.dependOn(b.getInstallStep());

    const test_simplify_step = b.step("test-simplify", "Run simplify-c-defines-zig/test_simplify.zig tests\n                               NOTE: you must run `zig build` first otherwise zig-out/bin/simplify is missing");
    test_simplify_step.dependOn(&simplify_tests.step);
}
