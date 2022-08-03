const std = @import("std");
const Allocator = std.mem.Allocator;

const qmc = @import("quine-mccluskey.zig");
const QuineMcCluskey = qmc.QuineMcCluskey;

const allr = std.testing.allocator;
const ABCD: []const []const u8 = &.{ "A", "B", "C", "D" };

fn testReduce(comptime QM: type) !void {
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
        .{
            // ~A~BC~D + ~ABC~D + A~B~C~D + A~B~CD + A~BC~D + A~BCD + ABC~D + ABCD
            .input = "A'B'CD' + A'BCD' + AB'C'D' + AB'C'D + AB'CD' + AB'CD + ABCD' + ABCD",
            .ones = &.{ 2, 6, 8, 9, 10, 11, 14, 15 },
            .result = "CD' + AB' + AC",
        },
    };
    const delimiter = " + ";
    for (tests) |tst| {
        const terms = try QM.parseTerms(allr, tst.input, delimiter, ABCD);
        defer allr.free(terms);

        // workaround: can't use std.testing.expextEqualSlices or std.testing.expextEqual
        // with T > u128 due to LLVM ERROR.  i guess the error happens when trying to print.
        for (tst.ones) |x, i| {
            if (x != terms[i])
                if (!test_large) // don't try to print large integers
                    std.debug.print("index {} expected {} != {} actual\n", .{ i, x, terms[i] });
            try std.testing.expect(x == terms[i]);
        }

        var qm = try QM.reduce(allr, terms, &.{}, ABCD);
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
        const equal = setsEqual(@TypeOf(expecteds), expecteds, actuals);
        if (!equal)
            std.debug.print("expected {s}\nactual  {s}\n", .{ tst.result, output.items });
        try std.testing.expect(equal);
    }
}

fn setsEqual(comptime Set: type, a: Set, b: Set) bool {
    if (a.count() != b.count()) return false;
    var iter = a.iterator();
    while (iter.next()) |it| {
        if (!b.contains(it.key_ptr.*)) return false;
    }
    return true;
}

fn parseIntoSet(
    comptime QM: type,
    allocator: Allocator,
    input: []const u8,
    delimiter: []const u8,
    variables: []const []const u8,
) !std.AutoHashMap(QM.T, void) {
    var result = std.AutoHashMap(QM.T, void).init(allocator);
    const terms = try QM.parseTerms(allocator, input, delimiter, variables);
    defer allocator.free(terms);
    for (terms) |term| try result.put(term, {});
    return result;
}

const QMu8192 = QuineMcCluskey(u8192);
const QMu4096 = QuineMcCluskey(u4096);
const QMu2048 = QuineMcCluskey(u2048);
const QMu1024 = QuineMcCluskey(u1024);
const QMu512 = QuineMcCluskey(u512);
const QMu256 = QuineMcCluskey(u256);
const QMu128 = QuineMcCluskey(u128);
const QMu64 = QuineMcCluskey(u64);
const QMu32 = QuineMcCluskey(u32);
const QMu16 = QuineMcCluskey(u16);
const QMu8 = QuineMcCluskey(u8);
const QMu4 = QuineMcCluskey(u4);

const test_large_integers = true;
const test_large = @hasDecl(@This(), "test_large_integers");
test "basic2" {
    if (test_large) {
        try testReduce(QMu8192);
        try testReduce(QMu4096);
        try testReduce(QMu2048);
        try testReduce(QMu1024);
        try testReduce(QMu512);
        try testReduce(QMu256);
    }
    try testReduce(QMu128);
    try testReduce(QMu64);
    try testReduce(QMu32);
    try testReduce(QMu16);
    try testReduce(QMu8);
    try testReduce(QMu4);
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
        var qm = try QMu32.reduce(allr, t.ons, t.dnc, ABCD);
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
