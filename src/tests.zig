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

// zig fmt: off
test "dontcares" {
    try doTest(
        &.{ 5, 7, 11, 12, 27, 29 },
        &.{ 14, 20, 21, 22, 23 },
        // B'CE A'BCE' BC'DE ACD'E
        "-01-1, 011-0, -1011, 1-101",
        Qm32.comma_delim,
    );
    try doTest(
        &.{ 1648,1904,1656,1912,1652,1908,1660,1916,1650,1906,1658,1914,1654,1910,1662,1918,1649,1905,1657,1913,1653,1909,1661,1917,1651,1907,1659,1915,1655,1911,1663,1919,1392,1904,1400,1912,1396,1908,1404,1916,1394,1906,1402,1914,1398,1910,1406,1918,1393,1905,1401,1913,1397,1909,1405,1917,1395,1907,1403,1915,1399,1911,1407,1919 },
        &.{ 0,1024,128,1152,64,1088,192,1216,32,1056,160,1184,96,1120,224,1248,16,1040,144,1168,80,1104,208,1232,48,1072,176,1200,112,1136,240,1264,8,1032,136,1160,72,1096,200,1224,40,1064,168,1192,104,1128,232,1256,24,1048,152,1176,88,1112,216,1240,56,1080,184,1208,120,1144,248,1272,4,1028,132,1156,68,1092,196,1220,36,1060,164,1188,100,1124,228,1252,20,1044,148,1172,84,1108,212,1236,52,1076,180,1204,116,1140,244,1268,12,1036,140,1164,76,1100,204,1228,44,1068,172,1196,108,1132,236,1260,28,1052,156,1180,92,1116,220,1244,60,1084,188,1212,124,1148,252,1276,2,1026,130,1154,66,1090,194,1218,34,1058,162,1186,98,1122,226,1250,18,1042,146,1170,82,1106,210,1234,50,1074,178,1202,114,1138,242,1266,10,1034,138,1162,74,1098,202,1226,42,1066,170,1194,106,1130,234,1258,26,1050,154,1178,90,1114,218,1242,58,1082,186,1210,122,1146,250,1274,6,1030,134,1158,70,1094,198,1222,38,1062,166,1190,102,1126,230,1254,22,1046,150,1174,86,1110,214,1238,54,1078,182,1206,118,1142,246,1270,14,1038,142,1166,78,1102,206,1230,46,1070,174,1198,110,1134,238,1262,30,1054,158,1182,94,1118,222,1246,62,1086,190,1214,126,1150,254,1278,1,1025,129,1153,65,1089,193,1217,33,1057,161,1185,97,1121,225,1249,17,1041,145,1169,81,1105,209,1233,49,1073,177,1201,113,1137,241,1265,9,1033,137,1161,73,1097,201,1225,41,1065,169,1193,105,1129,233,1257,25,1049,153,1177,89,1113,217,1241,57,1081,185,1209,121,1145,249,1273,5,1029,133,1157,69,1093,197,1221,37,1061,165,1189,101,1125,229,1253,21,1045,149,1173,85,1109,213,1237,53,1077,181,1205,117,1141,245,1269,13,1037,141,1165,77,1101,205,1229,45,1069,173,1197,109,1133,237,1261,29,1053,157,1181,93,1117,221,1245,61,1085,189,1213,125,1149,253,1277,3,1027,131,1155,67,1091,195,1219,35,1059,163,1187,99,1123,227,1251,19,1043,147,1171,83,1107,211,1235,51,1075,179,1203,115,1139,243,1267,11,1035,139,1163,75,1099,203,1227,43,1067,171,1195,107,1131,235,1259,27,1051,155,1179,91,1115,219,1243,59,1083,187,1211,123,1147,251,1275,7,1031,135,1159,71,1095,199,1223,39,1063,167,1191,103,1127,231,1255,23,1047,151,1175,87,1111,215,1239,55,1079,183,1207,119,1143,247,1271,15,1039,143,1167,79,1103,207,1231,47,1071,175,1199,111,1135,239,1263,31,1055,159,1183,95,1119,223,1247,63,1087,191,1215,127,1151,255,1279,768,1792,896,1920,832,1856,960,1984,800,1824,928,1952,864,1888,992,2016,784,1808,912,1936,848,1872,976,2000,816,1840,944,1968,880,1904,1008,2032,776,1800,904,1928,840,1864,968,1992,808,1832,936,1960,872,1896,1000,2024,792,1816,920,1944,856,1880,984,2008,824,1848,952,1976,888,1912,1016,2040,772,1796,900,1924,836,1860,964,1988,804,1828,932,1956,868,1892,996,2020,788,1812,916,1940,852,1876,980,2004,820,1844,948,1972,884,1908,1012,2036,780,1804,908,1932,844,1868,972,1996,812,1836,940,1964,876,1900,1004,2028,796,1820,924,1948,860,1884,988,2012,828,1852,956,1980,892,1916,1020,2044,770,1794,898,1922,834,1858,962,1986,802,1826,930,1954,866,1890,994,2018,786,1810,914,1938,850,1874,978,2002,818,1842,946,1970,882,1906,1010,2034,778,1802,906,1930,842,1866,970,1994,810,1834,938,1962,874,1898,1002,2026,794,1818,922,1946,858,1882,986,2010,826,1850,954,1978,890,1914,1018,2042,774,1798,902,1926,838,1862,966,1990,806,1830,934,1958,870,1894,998,2022,790,1814,918,1942,854,1878,982,2006,822,1846,950,1974,886,1910,1014,2038,782,1806,910,1934,846,1870,974,1998,814,1838,942,1966,878,1902,1006,2030,798,1822,926,1950,862,1886,990,2014,830,1854,958,1982,894,1918,1022,2046,769,1793,897,1921,833,1857,961,1985,801,1825,929,1953,865,1889,993,2017,785,1809,913,1937,849,1873,977,2001,817,1841,945,1969,881,1905,1009,2033,777,1801,905,1929,841,1865,969,1993,809,1833,937,1961,873,1897,1001,2025,793,1817,921,1945,857,1881,985,2009,825,1849,953,1977,889,1913,1017,2041,773,1797,901,1925,837,1861,965,1989,805,1829,933,1957,869,1893,997,2021,789,1813,917,1941,853,1877,981,2005,821,1845,949,1973,885,1909,1013,2037,781,1805,909,1933,845,1869,973,1997,813,1837,941,1965,877,1901,1005,2029,797,1821,925,1949,861,1885,989,2013,829,1853,957,1981,893,1917,1021,2045,771,1795,899,1923,835,1859,963,1987,803,1827,931,1955,867,1891,995,2019,787,1811,915,1939,851,1875,979,2003,819,1843,947,1971,883,1907,1011,2035,779,1803,907,1931,843,1867,971,1995,811,1835,939,1963,875,1899,1003,2027,795,1819,923,1947,859,1883,987,2011,827,1851,955,1979,891,1915,1019,2043,775,1799,903,1927,839,1863,967,1991,807,1831,935,1959,871,1895,999,2023,791,1815,919,1943,855,1879,983,2007,823,1847,951,1975,887,1911,1015,2039,783,1807,911,1935,847,1871,975,1999,815,1839,943,1967,879,1903,1007,2031,799,1823,927,1951,863,1887,991,2015,831,1855,959,1983,895,1919,1023,2047 },
        "1--0111----",
        Qm32.comma_delim,
    );
    
}
// zig fmt: on

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
        var term = try QM.bytesToTermBuf(&buf, std.mem.trim(u8, termbytes, &std.ascii.whitespace), bitcount);
        // std.debug.print("term {} bitcount {}\n", .{ Qm32.TermFmt.init(term, bitcount), bitcount });
        if (!result.contains(term))
            try result.putNoClobber(allr, try allr.dupe(u8, term), {});
    }
    return result;
}

fn permTestIter(bytes: []const u8, expecteds: []const u8, delimiter: []const u8, debug: bool) !void {
    const bitcount = @intCast(Qm32.TLen, bytes.len);
    const term = try Qm32.bytesToTerm(allr, bytes, bitcount);
    defer allr.free(term);
    var q = Qm32.init(allr, &.{}, &.{}, .{});
    var termset_expected = try parseIntoTermSet(Qm32, expecteds, delimiter, bitcount);
    defer q.deinitTermSet(&termset_expected);
    var iter = Qm32.PermutationsIter.init(term, .{}, bitcount);
    _ = debug;
    while (iter.next()) |perm| {
        try std.testing.expect(termset_expected.contains(perm));
    }
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

    try permTestIter(bytes, expecteds, delimiter, debug);
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
        const bitcount = 32 - @clz(expected_t);
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
