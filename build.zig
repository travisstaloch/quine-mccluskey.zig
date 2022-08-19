const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("quine-mccluskey", "src/quine-mccluskey.zig");
    lib.setBuildMode(mode);
    lib.install();

    const main_tests = b.addTest("src/tests.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    const simplify_exe = b.addExecutable("simplify", "simplify-c-defines-zig/simplify.zig");
    simplify_exe.setBuildMode(mode);
    simplify_exe.install();
    simplify_exe.addPackagePath("quine-mccluskey", "src/quine-mccluskey.zig");

    const simplify_tests = b.addTest("simplify-c-defines-zig/test_simplify.zig");
    simplify_tests.setBuildMode(mode);

    const test_simplify_step = b.step("test-simplify", "Run simplify-c-defines-zig/test_simplify.zig tests\n                               NOTE: you must run `zig build` first otherwise zig-out/bin/simplify is missing");
    test_simplify_step.dependOn(&simplify_tests.step);
    test_simplify_step.dependOn(&simplify_exe.step);
}
