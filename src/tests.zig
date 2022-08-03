const std = @import("std");
const Allocator = std.mem.Allocator;

const qmc = @import("quine-mccluskey.zig");
const QuineMcCluskey = qmc.QuineMcCluskey;

const allr = std.testing.allocator;
const ABCD: []const []const u8 = &.{ "A", "B", "C", "D" };

fn testReductions(comptime QM: type) !void {
    // i've been using this online reduction tool to find expected reductions:
    //   https://www.emathhelp.net/calculators/discrete-mathematics/boolean-algebra-calculator/
    // note: default is different syntax for NOT operator: A' -> ~A
    //   so have to replace trailing apostrophes with leading tildes as shown in comments below
    const Test = struct {
        input: []const u8,
        ones: []const QM.T = &.{},
        dontcares: []const QM.T = &.{},
        result: []const u8,
    };

    const tests = [_]Test{
        .{
            // ~A~B~C~D + A~B~C~D + AB~C~D + ABC~D + ~A~B~CD + A~B~CD + AB~CD + ABCD
            .input = "A'B'C'D' + AB'C'D' + ABC'D' + ABCD' + A'B'C'D + AB'C'D + ABC'D + ABCD",
            .ones = &.{ 0, 1, 8, 9, 12, 13, 14, 15 },
            .result = "B'C' + AB",
        },
        .{
            // ~A~B~C~D + ~A~B~CD + ~A~BCD + ~ABCD + A~B~C~D + A~B~CD + A~BCD + ABCD
            .input = "A'B'C'D' + A'B'C'D + A'B'CD + A'BCD + AB'C'D' + AB'C'D + AB'CD + ABCD",
            .ones = &.{ 0, 1, 3, 7, 8, 9, 11, 15 },
            .result = "B'C' + CD",
        },
        .{
            // ~AB~C~D + A~B~C~D + A~B~CD + A~BC~D + A~BCD + AB~C~D + ABC~D + ABCD
            .input = "A'BC'D' + AB'C'D' + AB'C'D + AB'CD' + AB'CD + ABC'D' + ABCD' + ABCD",
            .ones = &.{ 4, 8, 9, 10, 11, 12, 14, 15 },
            .result = "AB' + BC'D' + AC",
        },
    };
    const delimiter = " + ";
    for (tests) |tst| {
        const terms = try QM.parseTerms(allr, tst.input, delimiter, ABCD);
        defer allr.free(terms);

        try std.testing.expectEqualSlices(QM.T, tst.ones, terms);
        var qm = try QM.simplify(allr, terms, &.{}, ABCD);
        defer qm.deinit();
        var output = std.ArrayList(u8).init(allr);
        defer output.deinit();
        try qm.printEssentialTerms(output.writer(), " + ");

        // FIXME: why does T >= u32 produce different ordering? the following test should succeed
        //   ie "B'C' + AB" turns into "AB + B'C"
        // try std.testing.expectEqualStrings(tst.result, output.items);

        // workaround for FIXME: parse terms into sets and compare
        var expecteds = try parseIntoSet(QM, allr, tst.result, delimiter, ABCD);
        defer expecteds.deinit();
        var actuals = try parseIntoSet(QM, allr, output.items, delimiter, ABCD);
        defer actuals.deinit();
        try std.testing.expect(setsEqual(@TypeOf(expecteds), expecteds, actuals));
    }
}

fn setsEqual(comptime Set: type, a: Set, b: Set) bool {
    if (a.count() != b.count()) return false;
    var iter = a.iterator();
    while (iter.next()) |it| {
        const k = it.key_ptr.*;
        if (!b.contains(k)) return false;
    }
    return true;
}

fn parseIntoSet(comptime QM: type, allocator: Allocator, input: []const u8, delimiter: []const u8, variables: []const []const u8) !std.AutoHashMap(QM.T, void) {
    var result = std.AutoHashMap(QM.T, void).init(allocator);
    const terms = try QM.parseTerms(allocator, input, delimiter, variables);
    defer allocator.free(terms);
    for (terms) |term| try result.put(term, {});
    return result;
}

// const QMu2048 = QuineMcCluskey(u2048);
// const QMu1024 = QuineMcCluskey(u1024);
// const QMu512 = QuineMcCluskey(u512);
const QMu256 = QuineMcCluskey(u256);
const QMu128 = QuineMcCluskey(u128);
const QMu64 = QuineMcCluskey(u64);
const QMu32 = QuineMcCluskey(u32);
const QMu16 = QuineMcCluskey(u16);
const QMu8 = QuineMcCluskey(u8);
const QMu4 = QuineMcCluskey(u4);

test "basic2" {
    // try testReductions(QMu256); // <- LLVM ERROR: Unsupported library call operation!
    try testReductions(QMu128);
    try testReductions(QMu64);
    try testReductions(QMu32);
    try testReductions(QMu16);
    try testReductions(QMu8);
    try testReductions(QMu4);
}

test {
    const Test = struct {
        res: []const u8,
        ons: []const QMu32.T = &.{},
        dnc: []const QMu32.T = &.{},
    };

    const common_tests = [_]Test{
        .{ .res = "----", .ons = &.{}, .dnc = &.{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 } },
        .{ .res = "----", .ons = &.{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 } },
        .{ .res = "----", .ons = &.{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, .dnc = &.{ 10, 11, 12, 13, 14, 15 } },
        .{ .res = "----", .ons = &.{ 1, 3, 5, 7, 9, 11, 13, 15 }, .dnc = &.{ 0, 2, 4, 6, 8, 10, 12, 14 } },
    };
    const noxor_tests = [_]Test{
        .{ .res = "010- + 0-11 + 111- + 1-01", .ons = &.{ 3, 4, 5, 7, 9, 13, 14, 15 } },
    };

    inline for (common_tests ++ noxor_tests) |t| {
        var qm = try QMu32.simplify(allr, t.ons, t.dnc, ABCD);
        defer qm.deinit();
        var list = std.ArrayList(u8).init(allr);
        defer list.deinit();
        const writer = list.writer();
        const delimiter = " + ";
        try qm.printEssentialTermsBin(writer, delimiter);
        try std.testing.expectEqualStrings(t.res, list.items);
    }

    // const xor_tests = [_]Test{
    //     .{ .res = &.{"--^^"} },
    //     .{ .res = &.{"1--^^"} },
    //     .{ .res = &.{"-10"}, .ons = &.{2}, .dnc = &.{ 4, 5, 6, 7 } },
    //     .{ .res = &.{ "--1--11-", "00000001", "10001000" } },
    //     .{ .res = &.{"--^^"}, .ons = &.{ 1, 2, 5, 6, 9, 10, 13, 14 } },
    //     .{ .res = &.{"^^^^"}, .ons = &.{ 1, 7, 8, 14 }, .dnc = &.{ 2, 4, 5, 6, 9, 10, 11, 13 } },
    //     .{ .res = &.{"-------1"} },
    //     .{ .res = &.{"------^^"} },
    //     .{ .res = &.{"-----^^^"} },
    //     .{ .res = &.{"0^^^"} },
    //     .{ .res = &.{"0~~~"} },
    //     .{ .res = &.{"^^^^^^^^"} },
    //     .{ .res = &.{ "^^^0", "100-" } },
    //     .{ .res = &.{ "00^-0^^0", "01000001", "10001000" } },
    //     .{ .res = &.{ "^^^00", "111^^" } },
    //     .{ .res = &.{"---00000^^^^^^^"} },
    // };
}

// test "xxx" {
//     var x: u256 = 0b110101;
//     var y: u256 = 0b010111;
//     var result = x << 20;
//     _ = y;
//     std.debug.print("{}\n", .{result});
// }
