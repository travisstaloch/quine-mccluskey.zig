const std = @import("std");
const Allocator = std.mem.Allocator;

const qmc = @import("quine-mccluskey.zig");
const p = @import("parsing.zig");
const QuineMcCluskey = qmc.QuineMcCluskey;

const allr = std.testing.allocator;

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
            //"A'B'C'D' + AB'C'D' + ABC'D' + ABCD' + A'B'C'D + AB'C'D + ABC'D + ABCD",
            .ones = &.{ 0, 1, 8, 9, 12, 13, 14, 15 },
            //        "B'C' + AB",
            .result = "-00- + 11--",
        },
        .{ // 1
            // ~A~B~C~D + ~A~B~CD + ~A~BCD + ~ABCD + A~B~C~D + A~B~CD + A~BCD + ABCD
            //"A'B'C'D' + A'B'C'D + A'B'CD + A'BCD + AB'C'D' + AB'C'D + AB'CD + ABCD",
            .ones = &.{ 0, 1, 3, 7, 8, 9, 11, 15 },
            //        "B'C' + CD",
            .result = "-00- + --11",
        },
        .{ // 2
            // ~AB~C~D + A~B~C~D + A~B~CD + A~BC~D + A~BCD + AB~C~D + ABC~D + ABCD
            //"A'BC'D' + AB'C'D' + AB'C'D + AB'CD' + AB'CD + ABC'D' + ABCD' + ABCD",
            .ones = &.{ 4, 8, 9, 10, 11, 12, 14, 15 },
            //        "AB' + BC'D' + AC",
            .result = "1-1- + -100 + 10--",
        },
        .{ // 3
            // ~A~BC~D + ~ABC~D + A~B~C~D + A~B~CD + A~BC~D + A~BCD + ABC~D + ABCD
            //"A'B'CD' + A'BCD' + AB'C'D' + AB'C'D + AB'CD' + AB'CD + ABCD' + ABCD",
            .ones = &.{ 2, 6, 8, 9, 10, 11, 14, 15 },
            //        "CD' + AB' + AC",
            .result = "1-1- + --10 + 10--",
        },
        .{ // 4
            // ~AB~C~D + ~AB~CD + ~ABC~D + ~ABCD + A~B~C~D
            //"A'BC'D' + A'BC'D + A'BCD' + A'BCD + AB'C'D'",
            .ones = &.{ 4, 5, 6, 7, 8 },
            //        "A'B + AB'C'D'",
            .result = "1000 + 01--",
        },
        .{ // 5
            // ~F~GH + ~FG~H + ~FGH + F~G~H + F~GH + FG~H + FGH
            //"F'G'H + F'GH' + F'GH + FG'H' + FG'H + FGH' + FGH",
            .ones = &.{ 1, 2, 3, 4, 5, 6, 7 },
            //        "A + B + C",
            .result = "--1 + 1-- + -1-",
        },
        .{ // 6
            // variable names longer than 1
            //"ABCD' + ABCD",
            .ones = &.{ 2, 3 },
            //        "AB",
            .result = "1-",
        },
        .{ // 7
            //"A'BC + A'B + AB'C'D' + ABC'D'",
            .ones = &.{ 4, 6, 8, 12 },
            //        "A'BC' + AC'D'",
            .result = "1-00 + 01-0",
        },
        // from https://uweb.engr.arizona.edu/~ece474a/uploads/Main/lecture_logicopt.pdf
        // example 2
        .{ // 8
            //"w'x'y'z' + w'x'yz + w'x'yz' + w'xy'z' + w'xyz + w'xyz' + wxy'z + wxyz + wx'y'z + wx'yz",
            .ones = &.{ 0, 3, 2, 4, 7, 6, 13, 15, 9, 11 },
            //        "w'z' + wz + yz",
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
    // TODO: this ^ is technically incorrect. there are 2 minimal disjunctive forms:
    //   note the final terms on each line
    // This can be allomplished by deleting columns of
    // essential prime implicants and rows that contain their symbols
    // x'z + y'z + xyz' + xy'w
    // x'z + y'z + xyz' + xz'w
}

fn doTest(ones: []const Qm32.T, dontcares: []const Qm32.T, expected: []const u8, delimiter: []const u8) !void {
    var q = Qm32.init(allr, ones, dontcares, .{});
    try q.reduce();
    defer q.deinit();
    // std.debug.print("reduced_implicants.len {}\n", .{q.reduced_implicants.count()});
    var expecteds = try parseIntoTermSet(Qm32, expected, delimiter, q.bitcount);
    // std.debug.print("expected reduced_implicants {}\n", .{Qm32.TermSetFmt.init(expecteds, Qm32.comma_delim, q.bitcount)});
    // for (expecteds.keys()) |k|
    //     std.debug.print("{} ({})\n", .{ Qm32.TermFmt.init(k, q.bitcount), std.fmt.fmtSliceHexLower(k) });
    var expecteds_rev: Qm32.TermSet = .{};
    for (expecteds.keys()) |k| {
        const k2 = try allr.dupe(u8, k);
        try expecteds_rev.put(allr, k2, {});
    }
    // std.debug.print("e0 {}\n", .{std.fmt.fmtSliceHexLower(expecteds.keys()[0])});
    // std.debug.print("a0 {}\n", .{std.fmt.fmtSliceHexLower(q.reduced_implicants.keys()[0])});
    defer q.deinitTermSet(&expecteds);
    defer q.deinitTermSet(&expecteds_rev);
    try testTermSetsEqual(Qm32, expecteds_rev, q.reduced_implicants, expected, delimiter, q.bitcount, true);
}

test "basic" {
    try doTest(&.{ 2, 6, 10, 14, 15, 8, 9 }, &.{}, "--10, 111-, 100-", Qm32.comma_delim);
    try doTest(&.{ 4, 8, 6, 12 }, &.{}, "1-00, 01-0", Qm32.comma_delim);
    try doTest(&.{ 1, 2, 3, 6 }, &.{}, "0-1, -10", Qm32.comma_delim);
    try doTest(
        &.{ 0, 1, 2, 4, 8, 64, 3, 5, 6, 9, 12, 20, 48, 66, 144, 7, 13, 14, 26, 42, 50, 52, 74, 133, 15, 29, 30, 51, 75, 89, 101, 114, 177, 31, 47, 55, 59, 143, 185, 248, 126 },
        &.{},
        "-0000101, 0000--0-, 01100101, 11111000, 1011-001, 0-110010, 0-0000-0, 00101010, 000-11-1, 0011-011, 00110-00, 00000---, -0001111, 0000-1--, 10010000, 00-01111, 01111110, 01011001, 00110-11, 0100101-, 00011-10, 00-10100",
        Qm32.comma_delim,
    );
    // the following test case was previously failing because the output from qm.py is inconsistent between runs.
    // see https://github.com/tpircher/quine-mccluskey/issues/8
    // the output from the larger set (29 reduced implicants) was deemed incorrect and the smaller set (28)
    // was deemed correct.  i am not yet convinced this is actually correct.
    try doTest(
        &.{ 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 142, 399, 20, 21, 22, 279, 24, 25, 23, 29, 31, 38, 39, 44, 45, 46, 179, 51, 53, 54, 185, 60, 62, 196, 211, 87, 89, 227, 101, 235, 238, 495, 239, 242, 243, 244, 118, 120, 508, 125 },
        &.{},
        "0000-100-, 001111000, 0001-11-0, 000-10101, 00000---1, 000-0110-, 00-011001, 01110111-, 00000--1-, 011-10011, 01110-011, 0000101--, 0-0001110, 0000--1-1, 010111001, 001100101, -11101111, 110001111, -00010111, 0-0110011, 00-010111, 111111100, 001111101, 011110100, 01111001-, 011000100, 00-110110, 000-0011-",
        Qm32.comma_delim,
    );
    try doTest(
        &.{ 1, 2, 5, 6, 7 },
        &.{},
        "-01, -10, 1-1",
        Qm32.comma_delim,
    );
    try doTest(
        &.{ 1, 2, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 19 },
        &.{},
        "001-1, 010--, 0--10, 01--0, 0--01, 10011",
        Qm32.comma_delim,
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

test "dontcares" {
    try doTest(
        &.{ 5, 7, 11, 12, 27, 29 },
        &.{ 14, 20, 21, 22, 23 },
        // B'CE A'BCE' BC'DE ACD'E
        "-01-1, 011-0, -1011, 1-101",
        Qm32.comma_delim,
    );
}

fn testTermSetsEqual(comptime QM: type, expecteds: QM.TermSet, actuals: QM.TermSet, expected_input: []const u8, delimiter: []const u8, bitcount: QM.TLen, debug: bool) !void {
    const equal = QM.setsEqual(QM.TermSet, expecteds, actuals);
    if (!equal and debug) {
        {
            var iter = std.mem.split(u8, expected_input, delimiter);
            var i: usize = 0;
            while (iter.next()) |expected_bytes| : (i += 1) {
                const exterm = try QM.bytesToTerm(allr, expected_bytes, bitcount);
                defer allr.free(exterm);
                const missing = !actuals.contains(exterm);
                if (missing) {
                    std.debug.print("ERROR: expected not found at index {: >3}: {s} ({})\n", .{ i, QM.TermFmt.init(exterm, bitcount), std.fmt.fmtSliceHexLower(exterm) });
                }
            }
        }

        for (actuals.keys()) |actual, i| {
            const extra = !expecteds.contains(actual);
            if (extra) {
                std.debug.print("ERROR: unexpected item at index    {: >3}: {s} ({})\n", .{ i, QM.TermFmt.init(actual, bitcount), std.fmt.fmtSliceHexLower(actual) });
            }
        }
    }
    try std.testing.expect(equal);
}

fn parseIntoTermSet(comptime QM: type, input: []const u8, delimiter: []const u8, bitcount: QM.TLen) !QM.TermSet {
    var result: QM.TermSet = .{};

    var spliter = std.mem.split(u8, input, delimiter);
    while (spliter.next()) |termbytes| {
        // std.debug.print("termbytes {}:{s} bitcount {}\n", .{ termbytes.len, termbytes, bitcount });
        var buf: [QM.TBitSize]u8 = undefined;
        var term = try QM.bytesToTermBuf(&buf, std.mem.trim(u8, termbytes, &std.ascii.spaces), bitcount);
        // std.debug.print("term {} bitcount {}\n", .{ Qm32.TermFmt.init(term, bitcount), bitcount });
        if (!result.contains(term))
            try result.putNoClobber(allr, try allr.dupe(u8, term), {});
    }
    return result;
}

fn permTest(bytes: []const u8, expecteds: []const u8, delimiter: []const u8, debug: bool) !void {
    const bitcount = @intCast(Qm32.TLen, bytes.len);
    const term = try Qm32.bytesToTerm(allr, bytes, bitcount);
    defer allr.free(term);
    // std.debug.print("term.len {} term {}\n", .{ term.len, Qm32.TermFmt.init(term, bitcount) });
    var termset: Qm32.TermSet = .{};
    var q = Qm32.init(allr, &.{}, &.{}, .{});
    defer q.deinitTermSet(&termset);
    try Qm32.collectPerms(allr, term, &termset, .{}, bitcount);
    // std.debug.print("{}\n", .{Qm32.TermSetFmt.init(termset, ",", bitcount)});
    var termset_expected = try parseIntoTermSet(Qm32, expecteds, delimiter, bitcount);
    defer q.deinitTermSet(&termset_expected);
    // for (termset_expected.keys()) |k|
    //     std.debug.print("expected {}\n", .{std.fmt.fmtSliceHexLower(k)});
    // for (termset.keys()) |k|
    //     std.debug.print("actual   {}\n", .{std.fmt.fmtSliceHexLower(k)});
    try testTermSetsEqual(Qm32, termset_expected, termset, expecteds, delimiter, bitcount, debug);
}

test "permutations" {
    try permTest("-", "1, 0", ", ", true);
    try permTest("1-", "11, 10", ", ", true);
    try permTest("11-", "111, 110", ", ", true);
    try permTest("1--", "100, 101, 110, 111", ", ", true);
    try permTest("0000-011-", "000000110, 000000111, 000010110, 000010111", ", ", true);
    try permTest("000001---", "000001110, 000001001, 000001100, 000001000, 000001010, 000001101, 000001011, 000001111", ", ", true);
    try permTest("001110111-", "0011101110, 0011101111", ", ", true);
}

test "round trip parsing" {
    var i: usize = 0;
    var prng = std.rand.DefaultPrng.init(0);
    const rand = prng.random();
    while (i < 100) : (i += 1) {
        // start with random t
        const expected_t = rand.int(u32);
        const expected_bytes = try std.fmt.allocPrint(allr, "{b}", .{expected_t});
        defer allr.free(expected_bytes);
        const bitcount = 32 - @clz(u32, expected_t);
        // convert t to packed term
        const term = try Qm32.tToTerm(allr, expected_t, bitcount);
        defer allr.free(term);
        // compare binary string representations
        const bytes = try std.fmt.allocPrint(allr, "{}", .{Qm32.TermFmt.init(term, bitcount)});
        defer allr.free(bytes);
        try std.testing.expectEqualStrings(expected_bytes, bytes);
        // convert from binary string to packed term and compare
        const term2 = try Qm32.bytesToTerm(allr, bytes, bitcount);
        defer allr.free(term2);
        // std.debug.print("bitcount {}\nexpected_bytes {s}\n         bytes {s}\nterm  {}\nterm2 {}", .{
        //     bitcount,
        //     expected_bytes,
        //     bytes,
        //     std.fmt.fmtSliceHexLower(term),
        //     std.fmt.fmtSliceHexLower(term2),
        // });
        try std.testing.expectEqualStrings(term, term2);
        // convert from packed term to t and compare
        const t = try Qm32.termToT(term2, bitcount);
        try std.testing.expectEqual(expected_t, t);
    }
}
