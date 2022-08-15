const std = @import("std");
const Allocator = std.mem.Allocator;

/// 1. given `variables`={"A", "B"} and `delimiter`=" + "
///   - AB'         => (A and !B)
///   - AB' + C'D   => (A and !B) or (!C and D)
/// 2. given `variables`={"AB", "A", "B"} and `delimiter`=" + "
///   - AB'         => (!AB)
///     - if "A" were before "AB" in `variables` then the result would be the same as 1.
pub fn parseTerms(comptime Qm: type, allocator: Allocator, input: []const u8, delimiter: []const u8, variables: []const []const u8, comptime mode: enum { reverse, noreverse }) ![]Qm.T {
    const m = @intCast(Qm.TLen, variables.len - 1);
    var iter = std.mem.split(u8, input, delimiter);
    var terms: Qm.TSet = .{};
    defer terms.deinit(allocator);
    while (iter.next()) |_term| {
        const term = std.mem.trim(u8, _term, &std.ascii.spaces);
        var termval: Qm.T = 0;
        var i: usize = 0;
        while (i < term.len) {
            const idx = for (variables) |v, j| {
                if (std.mem.startsWith(u8, term[i..], v))
                    break @intCast(Qm.TLen, j);
            } else return error.ParseError;
            i += variables[idx].len;
            const not = @boolToInt(i < term.len and term[i] == '\'');
            // this is just a branchless equivalent of:
            //   if(!not) termval |= (1 << (m-idx)) else i += 1;
            if (mode == .reverse) {
                termval |= (@as(Qm.T, not +% 1) << @intCast(Qm.TLog2, m - idx));
            } else if (mode == .noreverse) {
                termval |= (@as(Qm.T, not +% 1) << @intCast(Qm.TLog2, idx));
            } else unreachable;
            i += not;
        }
        try terms.put(allocator, termval, {});
    }

    var result: Qm.TList = .{};
    var kiter = terms.keyIterator();
    while (kiter.next()) |k| {
        try result.append(allocator, k.*);
    }
    // std.sort.sort(Qm.T, result, {}, comptime ltByPopCount(T));
    return result.toOwnedSlice(allocator);
}

pub fn parseIntoSet(
    comptime QM: type,
    comptime Set: type,
    allocator: Allocator,
    input: []const u8,
    delimiter: []const u8,
    variables: []const []const u8,
) !Set {
    var result: Set = .{};
    const terms = try parseTerms(QM, allocator, input, delimiter, variables, .reverse);
    defer allocator.free(terms);
    for (terms) |term| try result.put(allocator, term, {});
    return result;
}

/// use when each item in input is a dual
pub fn parseOneTermDual(comptime QM: type, input_duals: []const u8, term: *std.ArrayList(u8)) !void {
    var i: usize = 0;
    term.clearRetainingCapacity();
    while (i < input_duals.len) {
        var dual: QM.Dual = undefined;
        dual.a = QM.byteToNibble(input_duals[i]);
        i += 1;
        if (i >= input_duals.len) {
            dual.b = QM.invalid;
            try term.append(QM.fromDual(dual));
            break;
        }
        dual.b = QM.byteToNibble(input_duals[i]);
        i += 1;
        try term.append(QM.fromDual(dual));
    }
}

/// use when input is packed terms (duals)
pub fn parseIntoTermSet(
    comptime QM: type,
    allocator: Allocator,
    input: []const u8,
    delimiter: []const u8,
) !QM.TermSet {
    var result: QM.TermSet = .{};

    var iter = std.mem.split(u8, input, delimiter);
    var term = std.ArrayList(u8).init(allocator);
    while (iter.next()) |termraw| {
        try parseOneTermDual(QM, termraw, &term);
        try result.put(allocator, try allocator.dupe(u8, term.items), {});
    }
    return result;
}

pub fn parseIntoStringSet(
    allocator: Allocator,
    input: []const u8,
    delimiter: []const u8,
) !std.StringArrayHashMap(void) {
    var result = std.StringArrayHashMap(void).init(allocator);
    var iter = std.mem.split(u8, input, delimiter);
    while (iter.next()) |s| try result.put(s, {});
    return result;
}

pub fn printEssentialTerms(comptime Qm: type, self: Qm, writer: anytype, delimiter: []const u8, variables: []const []const u8) !void {
    for (self.reduced_implicants.keys()) |term, i| {
        if (i != 0) _ = try writer.write(delimiter);
        const skiplen = std.math.sub(usize, term.len * 2, self.bitcount) catch continue;
        var j: usize = 0;
        for (term) |c| {
            const dual = Qm.toDual(c);
            if (j >= skiplen) {
                if (dual.a == Qm.zero)
                    try writer.print("{s}'", .{variables[j - skiplen]})
                else if (dual.a == Qm.one)
                    try writer.print("{s}", .{variables[j - skiplen]});
            }
            j += 1;
            if (j >= skiplen) {
                if (dual.b == Qm.zero)
                    try writer.print("{s}'", .{variables[j - skiplen]})
                else if (dual.b == Qm.one)
                    try writer.print("{s}", .{variables[j - skiplen]});
            }
            j += 1;
        }
    }
    if (self.reduced_implicants.keys().len == 0)
        try writer.writeByteNTimes('-', variables.len);
}

pub fn printEssentialTermsBin(comptime Qm: type, self: Qm, writer: anytype, delimiter: []const u8) !void {
    for (self.reduced_implicants.keys()) |term, i| {
        if (i != 0) _ = try writer.write(delimiter);
        try writer.print("{}", .{Qm.TermFmt.init(term, self.bitcount)});
    }
    if (self.reduced_implicants.keys().len == 0)
        try writer.writeByteNTimes('-', self.bitcount);
}

pub fn LenInt(comptime U: type) type {
    return std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(U)) + 1);
}

/// reverse only lower `len` in 2-bit chunks
pub fn bitReverse(
    comptime U: type,
    num: U,
    len: LenInt(U),
    comptime chunk_size: u8,
) U {
    var reverse_num: U = 0;
    var i: LenInt(U) = 0;
    var n = num;
    const I = std.meta.Int(.unsigned, chunk_size);
    while (i < len) : (i += 1) {
        reverse_num <<= chunk_size;
        reverse_num |= @truncate(I, n);
        n >>= chunk_size;
    }
    return reverse_num;
}

pub fn lessThan(comptime U: type) fn (void, U, U) bool {
    return struct {
        fn func(_: void, lhs: U, rhs: U) bool {
            return lhs < rhs;
        }
    }.func;
}

pub fn ltByPopCount(comptime U: type) fn (void, U, U) bool {
    return struct {
        fn func(_: void, lhs: U, rhs: U) bool {
            const lpc = @popCount(U, lhs);
            const rpc = @popCount(U, rhs);
            if (lpc == rpc) return lhs < rhs;
            return lpc < rpc;
        }
    }.func;
}

pub fn testEqualStringSets(comptime QM: type, allr: Allocator, expected: []const u8, actual: []const u8, delimiter: []const u8) !bool {
    var expecteds = try parseIntoStringSet(allr, expected, delimiter);
    defer expecteds.deinit();
    var actuals = try parseIntoStringSet(allr, actual, delimiter);
    defer actuals.deinit();
    const equal = QM.setsEqual(@TypeOf(expecteds), expecteds, actuals);
    if (!equal) {
        {
            var iter = std.mem.split(u8, expected, delimiter);
            var i: usize = 0;
            while (iter.next()) |t| : (i += 1) {
                if (!actuals.contains(t)) {
                    std.debug.print("ERROR: missing item at index {}: {s}\n", .{ i, t });
                }
            }
        }
        {
            var iter = std.mem.split(u8, actual, delimiter);
            var i: usize = 0;
            while (iter.next()) |t| : (i += 1) {
                if (!expecteds.contains(t)) {
                    std.debug.print("ERROR: extra item at index {}: {s}\n", .{ i, t });
                }
            }
        }
    }
    return equal;
}

pub const SetEquality = struct { missing: usize = 0, extra: usize = 0 };
pub fn testEqualSets(comptime QM: type, allr: Allocator, expected: []const u8, actual: []const u8, delimiter: []const u8, debug: bool) !SetEquality {
    var expecteds = try parseIntoTermSet(QM, allr, expected, delimiter);
    defer expecteds.deinit(allr);
    var actuals = try parseIntoTermSet(QM, allr, actual, delimiter);
    defer actuals.deinit(allr);
    const equal = QM.setsEqual(@TypeOf(expecteds), expecteds, actuals);
    var result: SetEquality = .{};
    if (!equal) {
        {
            var iter = std.mem.split(u8, expected, delimiter);
            var term = std.ArrayList(u8).init(allr);
            var i: usize = 0;
            while (iter.next()) |t| : (i += 1) {
                try parseOneTermDual(QM, t, &term);
                const missing = !actuals.contains(term.items);
                if (debug and missing) {
                    std.debug.print("ERROR: missing item at index {}: {s}\n", .{ i, t });
                }
                result.missing += @boolToInt(missing);
            }
        }
        {
            var iter = std.mem.split(u8, actual, delimiter);
            var term = std.ArrayList(u8).init(allr);
            var i: usize = 0;
            while (iter.next()) |t| : (i += 1) {
                try parseOneTermDual(QM, t, &term);
                const extra = !expecteds.contains(term.items);
                if (debug and extra) {
                    std.debug.print("ERROR: extra item at index {}: {s}\n", .{ i, t });
                }
                result.extra += @boolToInt(extra);
            }
        }
    }
    return result;
}
