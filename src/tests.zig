const std = @import("std");
const Allocator = std.mem.Allocator;

const qmc = @import("quine-mccluskey.zig");
const p = @import("parsing.zig");
const QuineMcCluskey = qmc.QuineMcCluskey;

const allr = std.testing.allocator;

const ABCD: []const []const u8 = &.{ "A", "B", "C", "D" };
const AB_CD: []const []const u8 = &.{ "AB", "CD" };
const ABCDEFGH: []const []const u8 = &.{ "A", "B", "C", "D", "E", "F", "G", "H" };
const wxyz: []const []const u8 = &.{ "w", "x", "y", "z" };

fn testReduce(comptime QM: type) !void {
    // i've been using this online tool to find expected reductions:
    //   https://www.emathhelp.net/calculators/discrete-mathematics/boolean-algebra-calculator/
    // note: default is different syntax for NOT operator: A' -> ~A
    //   so have to replace trailing apostrophes with leading tildes as shown in comments below
    const Test = struct {
        ones: []const QM.T = &.{},
        result: []const u8,
    };

    const tests = [_]Test{
        .{ // 0
            // ~A~B~C~D + A~B~C~D + AB~C~D + ABC~D + ~A~B~CD + A~B~CD + AB~CD + ABCD
            // .input = "A'B'C'D' + AB'C'D' + ABC'D' + ABCD' + A'B'C'D + AB'C'D + ABC'D + ABCD",
            .ones = &.{ 0, 1, 8, 9, 12, 13, 14, 15 },
            // .result = "B'C' + AB",
            .result = "-00- + 11--",
        },
        .{ // 1
            // ~A~B~C~D + ~A~B~CD + ~A~BCD + ~ABCD + A~B~C~D + A~B~CD + A~BCD + ABCD
            // .input = "A'B'C'D' + A'B'C'D + A'B'CD + A'BCD + AB'C'D' + AB'C'D + AB'CD + ABCD",
            .ones = &.{ 0, 1, 3, 7, 8, 9, 11, 15 },
            // .result = "B'C' + CD",
            .result = "-00- + --11",
        },
        .{ // 2
            // ~AB~C~D + A~B~C~D + A~B~CD + A~BC~D + A~BCD + AB~C~D + ABC~D + ABCD
            // .input = "A'BC'D' + AB'C'D' + AB'C'D + AB'CD' + AB'CD + ABC'D' + ABCD' + ABCD",
            .ones = &.{ 4, 8, 9, 10, 11, 12, 14, 15 },
            // .result = "AB' + BC'D' + AC",
            .result = "1-1- + -100 + 10--",
        },
        .{ // 3
            // ~A~BC~D + ~ABC~D + A~B~C~D + A~B~CD + A~BC~D + A~BCD + ABC~D + ABCD
            // .input = "A'B'CD' + A'BCD' + AB'C'D' + AB'C'D + AB'CD' + AB'CD + ABCD' + ABCD",
            .ones = &.{ 2, 6, 8, 9, 10, 11, 14, 15 },
            // .result = "CD' + AB' + AC",
            .result = "1-1- + --10 + 10--",
        },
        .{ // 4
            // ~AB~C~D + ~AB~CD + ~ABC~D + ~ABCD + A~B~C~D
            // .input = "A'BC'D' + A'BC'D + A'BCD' + A'BCD + AB'C'D'",
            .ones = &.{ 4, 5, 6, 7, 8 },
            // .result = "A'B + AB'C'D'",
            .result = "1000 + 01--",
        },
        .{ // 5
            // ~F~GH + ~FG~H + ~FGH + F~G~H + F~GH + FG~H + FGH
            // .input = "F'G'H + F'GH' + F'GH + FG'H' + FG'H + FGH' + FGH",
            .ones = &.{ 1, 2, 3, 4, 5, 6, 7 },
            // .result = "A + B + C",
            .result = "--1 + 1-- + -1-",
        },
        .{ // 6
            // variable names longer than 1
            // .input = "ABCD' + ABCD",
            .ones = &.{ 2, 3 },
            // .result = "AB",
            .result = "1-",
        },
        .{ // 7
            // .input = "A'BC + A'B + AB'C'D' + ABC'D'",
            .ones = &.{ 4, 6, 8, 12 },
            // .result = "A'BC' + AC'D'",
            .result = "1-00 + 01-0",
        },
        // from https://uweb.engr.arizona.edu/~ece474a/uploads/Main/lecture_logicopt.pdf
        // example 2
        .{ // 8
            // .input = "w'x'y'z' + w'x'yz + w'x'yz' + w'xy'z' + w'xyz + w'xyz' + wxy'z + wxyz + wx'y'z + wx'yz",
            .ones = &.{ 0, 3, 2, 4, 7, 6, 13, 15, 9, 11 },
            // .result = "w'z' + wz + yz",
            .result = "1--1 + --11 + 0--0",
        },
    };
    const delimiter = " + ";
    for (tests) |tst, tsti| {
        var qm = try QM.initAndReduce(allr, tst.ones, &.{}, .{});
        defer qm.deinit();
        var output = std.ArrayList(u8).init(allr);
        defer output.deinit();
        try p.printEssentialTermsBin(QM, qm, output.writer(), " + ");

        try expectEqualStringSets(QM, tst.result, output.items, delimiter, tsti);
    }
}

fn expectEqualStringSets(comptime QM: type, expected: []const u8, actual: []const u8, delimiter: []const u8, tsti: usize) !void {
    var expecteds = try p.parseIntoStringSet(allr, expected, delimiter);
    defer expecteds.deinit();
    var actuals = try p.parseIntoStringSet(allr, actual, delimiter);
    defer actuals.deinit();
    const equal = QM.setsEqual(@TypeOf(expecteds), expecteds, actuals);
    if (!equal)
        std.debug.print("test{} expected {s}\nactual  {s}\n", .{ tsti, expected, actual });
    try std.testing.expect(equal);
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

// TODO: add tests with dontcares

test "reduce" {
    try testReduce(QMu8);
    try testReduce(QMu16);
    try testReduce(QMu32);
    try testReduce(QMu64);
    if (test_large) {
        try testReduce(QMu128);
        try testReduce(QMu256);
        try testReduce(QMu512);
        // try testReduce(QMu1024);
        // try testReduce(QMu2048);
        // try testReduce(QMu4096);
        // try testReduce(QMu8192);
    }
}

test "reduce binary" {
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
        .{ .res = "010- + 111- + 0-11 + 1-01", .ons = &.{ 3, 4, 5, 7, 9, 13, 14, 15 } },
    };

    inline for (common_tests ++ noxor_tests) |t, tsti| {
        var qm = try QMu32.initAndReduce(allr, t.ons, t.dnc, .{});
        defer qm.deinit();
        var list = std.ArrayList(u8).init(allr);
        defer list.deinit();
        const writer = list.writer();
        const delimiter = " + ";
        try p.printEssentialTermsBin(QMu32, qm, writer, delimiter);
        try expectEqualStringSets(QMu32, t.res, list.items, delimiter, tsti);
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

test "not reducible" {
    // example from https://citeseerx.ist.psu.edu/viewdoc/download?rep=rep1&type=pdf&doi=10.1.1.213.287
    // ^ look for table II and table III
    const vars: []const []const u8 = &.{ "x", "y", "z", "w" };
    // ~xyzw + x~yzw + xy~zw + ~xyz~w + x~yz~w + xy~z~w + ~x~yzw + x~y~zw + ~x~yz~w
    const input =
        \\x'yzw + xy'zw + xyz'w + 
        \\x'yzw' + xy'zw' + xyz'w' + x'y'zw + xy'z'w + 
        \\x'y'zw'
    ;
    const delimiter = " + ";
    const ones = try p.parseTerms(QMu8, allr, input, delimiter, vars, .reverse);
    defer allr.free(ones);
    var qm = QMu8.init(allr, ones, &.{}, .{});
    defer qm.deinit();
    var output = std.ArrayList(u8).init(allr);
    defer output.deinit();
    try qm.reduce();
    try p.printEssentialTerms(QMu8, qm, output.writer(), delimiter, vars);
    try expectEqualStringSets(QMu8, "xyz' + xz'w + y'z + x'z", output.items, delimiter, 0);
    // TODO: this ^ is actually incorrect. there are 2 minimal disjunctive forms:
    //   note the final terms on each line
    // This can be allomplished by deleting columns of
    // essential prime implicants and rows that contain their symbols
    // x'z + y'z + xyz' + xy'w
    // x'z + y'z + xyz' + xz'w
}
