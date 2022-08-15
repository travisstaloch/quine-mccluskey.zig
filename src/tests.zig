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

const Qm8192 = QuineMcCluskey(u8192);
const Qm4096 = QuineMcCluskey(u4096);
const Qm2048 = QuineMcCluskey(u2048);
const Qm1024 = QuineMcCluskey(u1024);
const Qm512 = QuineMcCluskey(u512);
const Qm256 = QuineMcCluskey(u256);
const Qm128 = QuineMcCluskey(u128);
const Qm64 = QuineMcCluskey(u64);
const Qm32 = QuineMcCluskey(u32);
const Qm16 = QuineMcCluskey(u16);
const Qm8 = QuineMcCluskey(u8);
const Qm4 = QuineMcCluskey(u4);

const test_large_integers = true;
const test_large = @hasDecl(@This(), "test_large_integers");

// TODO: add tests with dontcares

test "reduce" {
    try testReduce(Qm8);
    try testReduce(Qm16);
    try testReduce(Qm32);
    try testReduce(Qm64);
    if (test_large) {
        try testReduce(Qm128);
        try testReduce(Qm256);
        try testReduce(Qm512);
        // try testReduce(QMu1024);
        // try testReduce(QMu2048);
        // try testReduce(QMu4096);
        // try testReduce(QMu8192);
    }
}

test "reduce binary" {
    const Test = struct {
        res: []const u8,
        ons: []const Qm32.T = &.{},
        dnc: []const Qm32.T = &.{},
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
        var qm = try Qm32.initAndReduce(allr, t.ons, t.dnc, .{});
        defer qm.deinit();
        var list = std.ArrayList(u8).init(allr);
        defer list.deinit();
        const writer = list.writer();
        const delimiter = " + ";
        try p.printEssentialTermsBin(Qm32, qm, writer, delimiter);
        try expectEqualStringSets(Qm32, t.res, list.items, delimiter, tsti);
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
    const ones = try p.parseTerms(Qm8, allr, input, delimiter, vars, .reverse);
    defer allr.free(ones);
    var qm = Qm8.init(allr, ones, &.{}, .{});
    defer qm.deinit();
    var output = std.ArrayList(u8).init(allr);
    defer output.deinit();
    try qm.reduce();
    try p.printEssentialTerms(Qm8, qm, output.writer(), delimiter, vars);
    try expectEqualStringSets(Qm8, "xyz' + xz'w + y'z + x'z", output.items, delimiter, 0);
    // TODO: this ^ is actually incorrect. there are 2 minimal disjunctive forms:
    //   note the final terms on each line
    // This can be allomplished by deleting columns of
    // essential prime implicants and rows that contain their symbols
    // x'z + y'z + xyz' + xy'w
    // x'z + y'z + xyz' + xz'w
}

fn doTest(ones: []const Qm32.T, dontcares: []const Qm32.T, expected: []const u8) !void {
    var q = Qm32.init(allr, ones, dontcares, .{});
    try q.reduce();
    defer q.deinit();
    const s = try std.fmt.allocPrint(allr, "{}", .{Qm32.TermSetFmt.init(q.reduced_implicants, Qm32.comma_delim, q.bitcount)});
    defer allr.free(s);
    const equal = try p.testEqualStringSets(Qm32, allr, expected, s, Qm32.comma_delim);
    // std.debug.print("reduced_implicants.len {}\n", .{q.reduced_implicants.count()});
    try std.testing.expect(equal);
}

test "basic" {
    try doTest(&.{ 2, 6, 10, 14, 15, 8, 9 }, &.{}, "--10, 111-, 100-");
    try doTest(&.{ 4, 8, 6, 12 }, &.{}, "1-00, 01-0");
    try doTest(&.{ 1, 2, 3, 6 }, &.{}, "0-1, -10");
    try doTest(
        &.{ 0, 1, 2, 4, 8, 64, 3, 5, 6, 9, 12, 20, 48, 66, 144, 7, 13, 14, 26, 42, 50, 52, 74, 133, 15, 29, 30, 51, 75, 89, 101, 114, 177, 31, 47, 55, 59, 143, 185, 248, 126 },
        &.{},
        "-0000101, 0000--0-, 01100101, 11111000, 1011-001, 0-110010, 0-0000-0, 00101010, 000-11-1, 0011-011, 00110-00, 00000---, -0001111, 0000-1--, 10010000, 00-01111, 01111110, 01011001, 00110-11, 0100101-, 00011-10, 00-10100",
    );
    try doTest(
        &.{ 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 142, 399, 20, 21, 22, 279, 24, 25, 23, 29, 31, 38, 39, 44, 45, 46, 179, 51, 53, 54, 185, 60, 62, 196, 211, 87, 89, 227, 101, 235, 238, 495, 239, 242, 243, 244, 118, 120, 508, 125 },
        &.{},
        "000-0011-, 001111000, 000-10101, 0111-0011, 00-010111, 00000--1-, 0000--1-1, 111111100, 0-0110011, 011-10011, 010111001, 011000100, 011101-11, -11101111, 001111101, 01110111-, 0000101--, 000-0110-, 0001-11-0, 00-011001, 0000-100-, 00-110110, 0-0001110, 001100101, 01111001-, 110001111, 00000---1, 011110100, -00010111",
    );
}

test "readme" {
    const allocator = std.testing.allocator;
    const qm = @import("quine-mccluskey.zig");
    const Qm = qm.QuineMcCluskey(u32);
    const ones = &.{ 0, 1, 8, 9, 12, 13, 14, 15 };
    var q = try Qm.initAndReduce(allocator, ones, &.{}, .{});
    defer q.deinit();
    const stdout = std.io.getStdOut().writer();
    try qm.parsing.printEssentialTermsBin(Qm, q, stdout, " + ");
    // outputs "-00- + 11--"
    const variables = .{ "A", "B", "C", "D" };
    try qm.parsing.printEssentialTerms(Qm, q, stdout, " + ", &variables);
    // outputs "B'C' + AB"
}
