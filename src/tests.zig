const std = @import("std");
const Allocator = std.mem.Allocator;

const qmc = @import("quine-mccluskey.zig");
const QuineMcCluskey = qmc.QuineMcCluskey;

const allr = std.testing.allocator;

test "sanity checks" {
    try std.testing.expectEqual(@as(u8, 0b10101010), QMu8.dashes);
    try std.testing.expectEqual(@as(u8, 0b01010101), QMu8.dashes_complement);
}

test "widen" {
    const ones = [_]u8{ 2, 6, 10, 12, 3, 9, 7, 11, 13 };
    var ones_wide: [ones.len]u16 = undefined;
    for (ones) |o, i| ones_wide[i] = QMu16.widenT(o);
    try std.testing.expectEqualSlices(u16, &.{ 4, 20, 68, 80, 5, 65, 21, 69, 81 }, &ones_wide);
}

test "widen compress" {
    const one = 0b110011;
    const one_wide = QMu32.widenT(one);
    try std.testing.expectEqual(@as(u32, 0b0000010100000101), one_wide);
    try std.testing.expectEqual(@as(u32, one), QMu32.compressT(one_wide, 1));
}

const ABCD: []const []const u8 = &.{ "A", "B", "C", "D" };
const AB_CD: []const []const u8 = &.{ "AB", "CD" };
const ABCDEFGH: []const []const u8 = &.{ "A", "B", "C", "D", "E", "F", "G", "H" };

fn testReduce(comptime QM: type) !void {
    // i've been using this online tool to find expected reductions:
    //   https://www.emathhelp.net/calculators/discrete-mathematics/boolean-algebra-calculator/
    // note: default is different syntax for NOT operator: A' -> ~A
    //   so have to replace trailing apostrophes with leading tildes as shown in comments below
    const Test = struct {
        input: []const u8,
        ones: []const QM.THalf = &.{},
        result: []const u8,
        variables: []const []const u8,
    };

    const tests = [_]Test{
        .{ // 0
            // ~A~B~C~D + A~B~C~D + AB~C~D + ABC~D + ~A~B~CD + A~B~CD + AB~CD + ABCD
            .input = "A'B'C'D' + AB'C'D' + ABC'D' + ABCD' + A'B'C'D + AB'C'D + ABC'D + ABCD",
            .ones = &.{ 0, 1, 8, 9, 12, 13, 14, 15 },
            .result = "B'C' + AB",
            .variables = ABCD,
        },
        .{ // 1
            // ~A~B~C~D + ~A~B~CD + ~A~BCD + ~ABCD + A~B~C~D + A~B~CD + A~BCD + ABCD
            .input = "A'B'C'D' + A'B'C'D + A'B'CD + A'BCD + AB'C'D' + AB'C'D + AB'CD + ABCD",
            .ones = &.{ 0, 1, 3, 7, 8, 9, 11, 15 },
            .result = "B'C' + CD",
            .variables = ABCD,
        },
        .{ // 2
            // ~AB~C~D + A~B~C~D + A~B~CD + A~BC~D + A~BCD + AB~C~D + ABC~D + ABCD
            .input = "A'BC'D' + AB'C'D' + AB'C'D + AB'CD' + AB'CD + ABC'D' + ABCD' + ABCD",
            .ones = &.{ 4, 8, 9, 10, 11, 12, 14, 15 },
            .result = "AB' + BC'D' + AC",
            .variables = ABCD,
        },
        .{ // 3
            // ~A~BC~D + ~ABC~D + A~B~C~D + A~B~CD + A~BC~D + A~BCD + ABC~D + ABCD
            .input = "A'B'CD' + A'BCD' + AB'C'D' + AB'C'D + AB'CD' + AB'CD + ABCD' + ABCD",
            .ones = &.{ 2, 6, 8, 9, 10, 11, 14, 15 },
            .result = "CD' + AB' + AC",
            .variables = ABCD,
        },
        .{ // 4
            // ~AB~C~D + ~AB~CD + ~ABC~D + ~ABCD + A~B~C~D
            .input = "A'BC'D' + A'BC'D + A'BCD' + A'BCD + AB'C'D'",
            .ones = &.{ 4, 5, 6, 7, 8 },
            .result = "A'B + AB'C'D'",
            .variables = ABCD,
        },
        .{ // 5
            // ~F~GH + ~FG~H + ~FGH + F~G~H + F~GH + FG~H + FGH
            .input = "F'G'H + F'GH' + F'GH + FG'H' + FG'H + FGH' + FGH",
            .ones = &.{ 1, 2, 3, 4, 5, 6, 7 },
            .result = "A'B'C'D'E'H + A'B'C'D'E'G + A'B'C'D'E'F",
            .variables = ABCDEFGH,
        },
        .{ // 6
            // variable names longer than 1
            .input = "ABCD' + ABCD",
            .ones = &.{ 2, 3 },
            .result = "AB",
            .variables = AB_CD,
        },
        // .{ // 7
        //     .input = "A'BC + A'B + AB'C'D' + ABC'D'",
        //     .ones = &.{ 4, 6, 8, 12 },
        //     .result = "BC' + AC'",
        //     .variables = ABCD,
        // },
    };
    const delimiter = " + ";
    for (tests) |tst, tsti| {
        if (@bitSizeOf(QM.TLog2) < tst.variables.len) continue;
        const terms = try QM.parseTerms(allr, tst.input, delimiter, tst.variables);
        defer allr.free(terms);

        // workaround: can't use std.testing{expextEqualSlices, expectEqual}
        // with T > u128 due to LLVM ERROR.  the error only happens when trying
        // to print T > u128
        try std.testing.expectEqual(tst.ones.len, terms.len);
        std.sort.sort(QM.THalf, terms, {}, comptime QM.lessThan(QM.THalf));
        for (tst.ones) |x, i| {
            if (x != terms[i])
                if (!test_large) // don't try to print large integers
                    std.debug.print("test{} index {} expected {}:{b} != {}:{b} actual\n", .{ tsti, i, x, x, terms[i], terms[i] });
            try std.testing.expect(x == terms[i]);
        }
        // std.debug.print("\nterms {b}\n", .{terms});
        var qm = try QM.initAndReduce(allr, tst.variables, terms, &.{});
        defer qm.deinit();
        var output = std.ArrayList(u8).init(allr);
        defer output.deinit();
        try qm.printEssentialTerms(output.writer(), " + ");

        // FIXME: why does T >= u32 produce different ordering? the following test should succeed
        //   ie "B'C' + AB" turns into "AB + B'C"
        // try std.testing.expectEqualStrings(tst.result, output.items);
        // if (!test_large and !std.mem.eql(u8, tst.result, output.items)) {
        //     std.debug.print("\ntst.result   {s} !=\n", .{tst.result});
        //     std.debug.print("output.items {s}\n", .{output.items});
        // }
        // workaround for FIXME: parse terms into sets and compare
        var expecteds = try parseIntoSet(QM, allr, tst.result, delimiter, tst.variables);
        defer expecteds.deinit();
        var actuals = try parseIntoSet(QM, allr, output.items, delimiter, tst.variables);
        defer actuals.deinit();
        const equal = setsEqual(@TypeOf(expecteds), expecteds, actuals);
        if (!equal)
            std.debug.print("test{} expected {s}\nactual  {s}\n", .{ tsti, tst.result, output.items });
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

fn isSubSet(comptime Set: type, set: Set, subset: []const @TypeOf(@as(Set.KV, undefined).key)) bool {
    if (subset.len > set.count()) return false;
    for (subset) |key| {
        if (!set.contains(key)) return false;
    }
    return true;
}

fn setsEqual2(comptime Set: type, a: Set, keys: []const @TypeOf(@as(Set.KV, undefined).key)) bool {
    if (keys.len != a.count()) return false;
    for (keys) |key| {
        if (!a.contains(key)) return false;
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
    const terms = try QM.parseTerms2(allocator, input, delimiter, variables);
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

test "reduce" {

    // try testReduce(QMu4);
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
        ons: []const QMu32.THalf = &.{},
        dnc: []const QMu32.THalf = &.{},
    };

    const common_tests = [_]Test{
        .{ .res = "----", .ons = &.{}, .dnc = &.{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 } },
        .{ .res = "----", .ons = &.{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 } },
        .{ .res = "----", .ons = &.{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, .dnc = &.{ 10, 11, 12, 13, 14, 15 } },
        .{ .res = "----", .ons = &.{ 1, 3, 5, 7, 9, 11, 13, 15 }, .dnc = &.{ 0, 2, 4, 6, 8, 10, 12, 14 } },
    };
    const noxor_tests = [_]Test{
        .{ .res = "-1-1 + 111- + 0-11 + 1-01 + 010-", .ons = &.{ 3, 4, 5, 7, 9, 13, 14, 15 } },
    };

    inline for (common_tests ++ noxor_tests) |t| {
        var qm = try QMu32.initAndReduce(allr, ABCD, t.ons, t.dnc);
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

fn testPerms(imp: QMu32.ImplTs, expecteds: []const @TypeOf(@as(QMu32.TSet.KV, undefined).key), comptime fmt: []const u8) !void {
    var q = QMu32.init(allr, &.{});
    defer q.deinit();
    var perms = try q.permutations(imp);
    defer perms.deinit();
    const eq = setsEqual2(QMu32.TSet, perms, expecteds);
    if (!eq)
        std.debug.print("expected " ++ fmt ++ " actual " ++ fmt ++ "\n", .{ expecteds, perms.keys() });
    try std.testing.expect(eq);
}

// test "permutations" {
//     const fmt = "{b:0>4}";
//     try testPerms(
//         .{ .number = 0b0011, .dashes = 0b1100 },
//         &.{ 0b0011, 0b0111, 0b1011, 0b1111 },
//         fmt,
//     );
//     try testPerms(
//         .{ .number = 0b0000, .dashes = 0b1100 },
//         &.{ 0b0000, 0b0100, 0b1000, 0b1100 },
//         fmt,
//     );
//     try testPerms(
//         .{ .number = 0b0000, .dashes = 0b1000 },
//         &.{ 0b0000, 0b1000 },
//         fmt,
//     );
//     try testPerms(
//         .{ .number = 0b0000, .dashes = 0b1110 },
//         &.{ 0b0000, 0b0010, 0b0100, 0b0110, 0b1000, 0b1010, 0b1100, 0b1110 },
//         fmt,
//     );

//     {
//         // this block exercises a bug which happens when iterating over set keys while adding to the set.
//         // the bug only manifests when set.keys() grows big enough that a reallocation happens.
//         // this is the old buggy code from premutations:
//         //   for (set.keys()) |k|
//         //     try set.put(k | mask, {});
//         var q = QMu32.init(allr, &.{});
//         defer q.deinit();
//         // -00--------
//         var perms = try q.permutations(.{ .number = 0, .dashes = 0b10011111111 });
//         defer perms.deinit();
//         // -11--------
//         var perms2 = try q.permutations(.{ .number = 0b01100000000, .dashes = 0b10011111111 });
//         for (perms2.keys()) |k| try perms.put(k, {});
//         defer perms2.deinit();
//         try std.testing.expectEqual(@as(usize, 1024), perms.count());
//     }
// }

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
    const ones = try QMu8.parseTerms(allr, input, " + ", vars);
    defer allr.free(ones);
    var qm = QMu8.init(allr, vars, ones, &.{});
    defer qm.deinit();
    var output = std.ArrayList(u8).init(allr);
    defer output.deinit();
    try qm.reduce();
    try qm.printEssentialTerms(output.writer(), " + ");
    try std.testing.expectEqualStrings("x'z + xyz' + xy'w + y'z", output.items);
    // TODO: this ^ is actually incorrect. there are 2 minimal disjunctive forms:
    //   note the final terms on each line
    // This can be allomplished by deleting columns of
    // essential prime implicants and rows that contain their symbols
    // x'z + y'z + xyz' + xy'w
    // x'z + y'z + xyz' + xz'w
}
