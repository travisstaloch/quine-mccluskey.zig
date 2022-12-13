const std = @import("std");

test "simplify compare" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allr = arena.allocator();

    const exepath = if (@import("builtin").os.tag == .windows) "zig-out/bin/simplify.exe" else "zig-out/bin/simplify";
    const programtxt_path = "simplify-c-defines/program.txt";

    // run zig-out/bin/simplify simplify-c-defines/program.txt
    const zig_simplify_result = std.ChildProcess.exec(.{ .allocator = allr, .argv = &.{ exepath, programtxt_path } }) catch |err| switch (err) {
        error.FileNotFound => {
            std.debug.print("\n\nerror - FileNotFound: you must run `zig build` first otherwise " ++ exepath ++ " is missing\n\n", .{});
            return err;
        },
        else => return err,
    };
    // run python simplify-c-defines/simplify.py simplify-c-defines/program.txt
    const py_simplify_result = try std.ChildProcess.exec(.{ .allocator = allr, .argv = &.{ "python", "simplify-c-defines/simplify.py", programtxt_path } });
    // compare output
    try std.testing.expectEqualStrings(std.mem.trim(u8, py_simplify_result.stdout, &std.ascii.whitespace), std.mem.trim(u8, zig_simplify_result.stdout, &std.ascii.whitespace));
}
