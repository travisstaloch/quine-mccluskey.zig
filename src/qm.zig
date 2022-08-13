//! adapted from https://github.com/tpircher/quine-mccluskey/

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

/// this imlementation packs 2 elements per byte and calls this representation a 'dual'
const QuineMcCluskey = struct {
    allocator: Allocator,
    bitcount: TLen,
    ones: []const T,
    dontcares: []const T,
    terms: []Term,
    flags: std.enums.EnumSet(enum { use_xor }),

    comptime {
        assert(std.math.isPowerOfTwo(TBitSize));
    }
    pub const T = u32;

    pub const Dual = packed struct {
        a: u4,
        b: u4,
        pub const invalid = @enumToInt(Element.invalid);
        pub fn initInvalid() Dual {
            return .{ .a = invalid, .b = invalid };
        }
    };
    pub const Element = enum(u4) { zero, one, xnor, xor, dash, invalid };
    pub const Term = []const u8;
    pub const TBitSize = @bitSizeOf(T);
    pub const TLog2 = std.math.Log2Int(T);

    pub const TLen = std.meta.Int(.unsigned, std.math.log2_int(T, TBitSize) + 1);
    pub const TLog2Signed = std.meta.Int(.signed, std.math.log2_int(TLen, TBitSize) + 1);

    pub fn toDual(te: u8) Dual {
        return @bitCast(Dual, te);
    }
    pub fn toDualPtr(te: *u8) *Dual {
        return @ptrCast(*Dual, te);
    }
    pub fn fromDual(dual: Dual) u8 {
        return @bitCast(u8, dual);
    }

    pub const TSet = std.AutoHashMapUnmanaged(T, void);
    pub const TermSet = std.StringHashMapUnmanaged(void);
    pub const GroupKey = [3]u8;
    pub const TermMap = std.AutoHashMapUnmanaged(GroupKey, TermSet);
    pub const TermMapOrdered = std.ArrayHashMapUnmanaged(u16, TermSet, std.array_hash_map.AutoContext(u16), true);
    pub const PermMap = std.StringHashMapUnmanaged(TermSet);

    const Self = @This();

    pub fn init(allocator: Allocator, ones: []const T, dontcares: []const T, options: struct { bitcount: ?TLen = null }) Self {
        return .{
            .allocator = allocator,
            .ones = ones,
            .dontcares = dontcares,
            .bitcount = if (options.bitcount) |bc| bc else 0,
            .flags = .{},
            .terms = &.{},
        };
    }

    pub fn freeTerms(self: *Self, terms: []Term) void {
        for (terms) |term| self.allocator.free(term);
        self.allocator.free(terms);
    }
    pub fn freeTermSetKeys(self: *Self, termset: *TermSet) void {
        var iter = termset.keyIterator();
        while (iter.next()) |k|
            self.allocator.free(k.*);
    }
    pub fn deinitTermSet(self: *Self, termset: *TermSet) void {
        self.freeTermSetKeys(termset);
        termset.deinit(self.allocator);
    }

    fn toTerm(allocator: Allocator, t: T, bitcount: TLen) !Term {
        var terms = try allocator.alloc(u8, bitcount);
        return toTermBuf(terms, t, bitcount);
    }

    fn toTermBuf(buf: []u8, t: T, bitcount: TLen) Term {
        var mutt = t;

        var i = bitcount - 1;
        while (true) : (i -= 1) {
            const dual = toDualPtr(&buf[i]);
            dual.a = @truncate(u1, mutt);
            mutt >>= 1;
            dual.b = @truncate(u1, mutt);
            if (i == 0) break;
            mutt >>= 1;
        }
        return buf[0..bitcount];
    }

    fn elementToByte(e: Element) u8 {
        return switch (e) {
            .zero => '0',
            .one => '1',
            .xor => '^',
            .xnor => '~',
            .dash => '-',
        };
    }

    fn nibbleToByte1(e: u4) u8 {
        return switch (e) {
            @enumToInt(Element.zero) => '0',
            @enumToInt(Element.one) => '1',
            @enumToInt(Element.xor) => '^',
            @enumToInt(Element.xnor) => '~',
            @enumToInt(Element.dash) => '-',
            else => std.debug.panic("invalid TermElement {}", .{e}),
        };
    }

    fn nibblesToBytes(e: u8) [2]u8 {
        const dual = toDual(e);
        return .{ nibbleToByte1(dual.a), nibbleToByte1(dual.b) };
    }

    pub const TermFmt = struct {
        term: Term,
        pub fn init(term: Term) TermFmt {
            return .{ .term = term };
        }

        pub fn format(self: TermFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            for (self.term) |e| {
                const bytes = nibblesToBytes(e);
                try writer.writeByte(bytes[1]);
                try writer.writeByte(bytes[0]);
            }
        }
    };
    pub const TermsFmt = struct {
        terms: []const Term,
        pub fn init(terms: []const Term) TermsFmt {
            return .{ .terms = terms };
        }

        pub fn format(self: TermsFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            for (self.terms) |term, i| {
                if (i != 0) _ = try writer.write(", ");
                try writer.print("{}", .{TermFmt.init(term)});
            }
        }
    };

    pub const TermSetFmt = struct {
        termset: TermSet,
        pub fn init(termset: TermSet) TermSetFmt {
            return .{ .termset = termset };
        }

        pub fn format(self: TermSetFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            var iter = self.termset.keyIterator();
            var i: usize = 0;
            while (iter.next()) |k| : (i += 1) {
                if (i != 0) _ = try writer.write(", ");
                try writer.print("{}", .{TermFmt.init(k.*)});
            }
        }
    };

    pub fn simplify(self: *Self) !TermSet {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        const arenaallr = arena.allocator();
        defer arena.deinit();

        var ones = std.ArrayList(Term).init(arenaallr);
        var dontcares = std.ArrayList(Term).init(arenaallr);

        // find max bitcount if not provided
        if (self.bitcount == 0) {
            for (self.ones) |x|
                self.bitcount = std.math.max(self.bitcount, std.math.log2_int_ceil(T, x));
            for (self.dontcares) |x|
                self.bitcount = std.math.max(self.bitcount, std.math.log2_int_ceil(T, x));
        }

        for (self.ones) |x|
            try ones.append(try toTerm(arenaallr, x, self.bitcount / 2));
        for (self.dontcares) |x|
            try dontcares.append(try toTerm(arenaallr, x, self.bitcount / 2));

        return try self.simplifyLos(ones.items, dontcares.items, arenaallr);
    }

    pub fn simplifyLos(self: *Self, ones: []Term, dontcares: []Term, arena: Allocator) !TermSet {
        var terms: TermSet = .{};
        for (ones) |x| try terms.put(arena, try arena.dupe(u8, x), {});
        for (dontcares) |x| try terms.put(arena, try arena.dupe(u8, x), {});

        if (terms.count() == 0) return TermSet{};

        var prime_implicants = try self.getPrimeImplicants(arena, &terms);
        defer self.deinitTermSet(&prime_implicants);
        trace("prime_implicants {}\n", .{TermSetFmt.init(prime_implicants)});
        var dcset: TSet = .{};
        for (self.dontcares) |t| try dcset.put(arena, t, {});
        var essential_implicants = try self.getEssentialImplicants(arena, &prime_implicants, dcset);
        trace("essential_implicants {}\n", .{TermSetFmt.init(essential_implicants)});
        return essential_implicants;
    }

    fn termCount(t: Term, te: Element) TLog2 {
        var r: TLog2 = 0;
        for (t) |e| {
            const dual = toDual(e);
            r += @boolToInt(dual.a == @enumToInt(te));
            r += @boolToInt(dual.b == @enumToInt(te));
        }
        return r;
    }

    pub fn getPrimeImplicants(self: *Self, arena: Allocator, terms: *TermSet) !TermSet {
        const ngroups = self.bitcount + 1;

        if (false) {
            var groups = try arena.alloc(TermSet, ngroups);
            for (groups) |*g| g.* = .{};
            {
                var kiter = terms.keyIterator();
                while (kiter.next()) |t| {
                    const bitcount = termCount(t.*, .one);
                    try groups[bitcount].put(arena, t.*, {});
                }
            }
            if (self.flags.contains(.use_xor))
                todo("use_xor");
        }

        var groups: TermMap = .{};
        var used: TermSet = .{};
        var marked: TermSet = .{};
        var t2 = try arena.alloc(u8, self.bitcount / 2);

        while (true) {
            groups.clearRetainingCapacity();
            {
                var termsiter = terms.keyIterator();
                while (termsiter.next()) |k| {
                    const t = k.*;
                    const key = GroupKey{ termCount(t, .one), termCount(t, .xor), termCount(t, .xnor) };
                    assert(key[1] == 0 or key[2] == 0);
                    const gop = try groups.getOrPut(arena, key);
                    if (!gop.found_existing) gop.value_ptr.* = .{};
                    try gop.value_ptr.put(arena, t, {});
                }
            }

            {
                var groupsiter = groups.iterator();
                while (groupsiter.next()) |it| {
                    trace("group{any}: {}\n", .{ it.key_ptr.*, TermSetFmt.init(it.value_ptr.*) });
                }
            }

            used.clearRetainingCapacity();
            terms.clearRetainingCapacity();

            // Find prime implicants
            var groupsiter = groups.iterator();
            while (groupsiter.next()) |it| {
                var key_next = it.key_ptr.*;
                key_next[0] += 1;
                if (groups.get(key_next)) |group_next| {
                    const ts1 = it.value_ptr.*;
                    var ts1iter = ts1.keyIterator();
                    while (ts1iter.next()) |t1p| {
                        const t1 = t1p.*;
                        // trace("t1 {}\n", .{TermFmt.init(t1)});
                        for (t1) |cs1, i| {
                            const dual = toDual(cs1);
                            // check both nibbles
                            inline for (comptime std.meta.fieldNames(Dual)) |f| {
                                const c1 = @field(dual, f);
                                if (c1 == @enumToInt(Element.zero)) {
                                    std.mem.copy(u8, t2, t1);
                                    var tep2 = toDual(t2[i]);
                                    @field(tep2, f) = @enumToInt(Element.one);
                                    t2[i] = fromDual(tep2);
                                    // trace("t2 {}\n", .{TermFmt.init(t2)});
                                    if (group_next.contains(t2)) {
                                        const t12 = try arena.dupe(u8, t1);
                                        var tep12 = toDual(t12[i]);
                                        @field(tep12, f) = @enumToInt(Element.dash);
                                        t12[i] = fromDual(tep12);
                                        trace("{} {} {}\n", .{ TermFmt.init(t1), TermFmt.init(t2), TermFmt.init(t12) });
                                        try used.put(arena, t1, {});
                                        try used.put(arena, try arena.dupe(u8, t2), {});
                                        try terms.put(arena, t12, {});
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // TODO; Find XOR combinations

            // TODO; Find XNOR combinations

            // Add the unused terms to the list of marked terms
            // for g in list(groups.values()):
            //     marked |= g - used
            var gvaluesiter = groups.valueIterator();
            while (gvaluesiter.next()) |g| {
                var gkiter = g.keyIterator();
                while (gkiter.next()) |k| {
                    const t = k.*;
                    if (!used.contains(t))
                        try marked.put(arena, t, {});
                }
            }
            trace("groups.len {} used.len {} marked.len {}\n", .{ groups.count(), used.count(), marked.count() });
            if (used.count() == 0) break;
        }

        trace("groups.len {} used.len {} marked.len {}\n", .{ groups.count(), used.count(), marked.count() });

        // Prepare the list of prime implicants
        var result: TermSet = .{};
        var markediter = marked.keyIterator();
        while (markediter.next()) |k| {
            try result.put(self.allocator, try self.allocator.dupe(u8, k.*), {});
        }

        var gvaluesiter = groups.valueIterator();
        while (gvaluesiter.next()) |g| {
            var gkiter = g.keyIterator();
            while (gkiter.next()) |k| {
                const dupe = try self.allocator.dupe(u8, k.*);
                const gop = try result.getOrPut(self.allocator, dupe);
                if (gop.found_existing) self.allocator.free(dupe);
            }
        }
        return result;
    }

    fn termRank(t: u4) u16 {
        return if (t == 0 or t == @enumToInt(Element.invalid)) 0 else @as(u16, 1) << t - 1;
    }

    fn getTermRank(term: Term, term_range: u16) !u16 {
        comptime {
            assert(5 == @enumToInt(Element.invalid));
            assert(4 == @enumToInt(Element.dash));
            assert(3 == @enumToInt(Element.xor));
            assert(2 == @enumToInt(Element.xnor));
            assert(1 == @enumToInt(Element.one));
            assert(0 == @enumToInt(Element.zero));
            assert(8 == termRank(@enumToInt(Element.dash)));
            assert(4 == termRank(@enumToInt(Element.xor)));
            assert(2 == termRank(@enumToInt(Element.xnor)));
            assert(1 == termRank(@enumToInt(Element.one)));
            assert(0 == termRank(@enumToInt(Element.zero)));
            assert(0 == termRank(@enumToInt(Element.invalid)));
        }
        var n: u16 = 0;
        for (term) |te| {
            const dual = toDual(te);
            n += termRank(dual.a);
            n += termRank(dual.b);
        }
        return 4 * term_range + n;
    }

    fn isSubSet(comptime Set: type, maybe_subset: Set, set: Set) bool {
        if (maybe_subset.count() > set.count()) return false;
        var kiter = maybe_subset.keyIterator();
        while (kiter.next()) |k| {
            // for (subset) |key| {
            if (!set.contains(k.*)) return false;
        }
        return true;
    }

    pub const Permutations = struct {
        value: Term,
        n_bits: TLog2,
        n_xor: TLog2,
        xor_value: u8 = 0,
        seen_xors: u8 = 0,
        res: A = [1]u8{0} ** (TBitSize / 2),
        i: TLog2Signed = 0,
        direction: i2 = 1,
        exclude: TSet,

        pub const A = [TBitSize / 2]u8;
        pub const V = @Vector(TBitSize / 2, u8);

        pub fn init(term: Term, exclude: TSet) Permutations {
            var result = Permutations{
                .n_bits = @intCast(TLog2, term.len),
                .n_xor = termCount(term, .xor) + termCount(term, .xnor),
                .exclude = exclude,
                .value = term,
            };
            return result;
        }

        fn setTeField(te: *u8, comptime field_name: []const u8, field_value: u4) void {
            const dual = toDualPtr(te);
            @field(dual, field_name) = field_value;
        }

        fn getTeField(te: u8, comptime field_name: []const u8) u4 {
            const dual = toDual(te);
            return @field(dual, field_name);
        }

        pub fn next(self: *Permutations) Term {
            const trace_this = false;
            if (trace_this) trace("value {} \n", .{TermFmt.init(self.value[0..self.n_bits])});

            // unlike the python version, in this one, self.i ranges from 0..(n_bits * 2)
            // (rather than 0...n_bits).  this is because each byte stores two elements.
            while (self.i >= 0) {
                {
                    const i = @intCast(TLen, @divTrunc(self.i, 2));
                    if (trace_this) trace("next()  i {} direction {} value[i] {c} res[i] {c}\n", .{
                        i,
                        self.direction,
                        nibblesToBytes(self.value[i]),
                        nibblesToBytes(self.res[i]),
                    });
                    // on even indices, use low nibble 'a' otherwise high nibble 'b'
                    const dual = toDual(self.value[i]);
                    const is_even = @truncate(i1, self.i) == 0;
                    const ele = if (is_even) dual.a else dual.b;
                    switch (ele) {
                        @enumToInt(Element.zero), @enumToInt(Element.one) => {
                            // self.res[i] = self.value[i];
                            if (is_even)
                                setTeField(&self.res[i], "a", getTeField(self.value[i], "a"))
                            else
                                setTeField(&self.res[i], "b", getTeField(self.value[i], "b"));
                        },
                        @enumToInt(Element.dash) => if (self.direction == 1) {
                            // self.res[i] = @enumToInt(TermElementE.zero);
                            if (is_even)
                                setTeField(&self.res[i], "a", @enumToInt(Element.zero))
                            else
                                setTeField(&self.res[i], "b", @enumToInt(Element.zero));
                            // } else if (self.res[i] == @enumToInt(TermElementE.zero)) {
                        } else if ((if (is_even) getTeField(self.res[i], "a") else getTeField(self.res[i], "b")) == @enumToInt(Element.zero)) {

                            // self.res[i] = @enumToInt(TermElementE.one);
                            if (is_even)
                                setTeField(&self.res[i], "a", @enumToInt(Element.one))
                            else
                                setTeField(&self.res[i], "b", @enumToInt(Element.one));
                            self.direction = 1;
                        },
                        @enumToInt(Element.xor) => todo("xor"),
                        @enumToInt(Element.xnor) => todo("xnor"),
                        @enumToInt(Element.invalid) => todo("invalid"),
                        else => std.debug.panic("invalid TermElement {}", .{self.value[i]}),
                    }
                    if (trace_this and i < self.value.len)
                        trace("next()2 i {} direction {} value[i] {c} res[i] {c}\n", .{
                            i,
                            self.direction,
                            nibblesToBytes(self.value[i]),
                            nibblesToBytes(self.res[i]),
                        });
                }

                self.i += self.direction;
                {
                    const i = std.math.cast(TLen, self.i) orelse continue;
                    if (i == self.n_bits * 2) {
                        self.direction = -1;
                        self.i = self.n_bits * 2 - 1;
                        const ones = @splat(TBitSize / 2, fromDual(.{ .a = @enumToInt(Element.one), .b = @enumToInt(Element.one) }));
                        const cmp = @as(V, self.res) == ones;
                        const t = @ptrCast(*align(2) const T, &cmp).*; // TODO: audit this.
                        //                  ^ i'm not sure this alignment is correct although its needed to compile
                        if (trace_this) {
                            var buf: A = undefined;
                            const tterm = toTermBuf(&buf, t, self.n_bits);
                            trace(
                                "t {b:0>8} tterm {} self.res {} self.exclude.contains(t) {}\n",
                                .{ t, TermFmt.init(tterm), TermFmt.init(@as(A, self.res)[0..self.n_bits]), self.exclude.contains(t) },
                            );
                        }
                        if (!self.exclude.contains(t)) {
                            return @as(A, self.res)[0..self.n_bits];
                        }
                    }
                }
            }
            return &[0]u8{};
        }
    };

    pub fn getEssentialImplicants(self: *Self, arena: Allocator, terms: *TermSet, dcset: TSet) !TermSet {

        // Create all permutations for each term in terms.
        // perms = {}
        // for t in terms:
        //     perms[t] = set(p for p in self.permutations(t) if p not in dc)
        var perms: PermMap = .{};
        {
            var iter = terms.keyIterator();
            while (iter.next()) |k| {
                const term = k.*;

                const gop = try perms.getOrPut(arena, term);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                trace("t {}\n", .{TermFmt.init(term)});
                var permsiter = Permutations.init(term, dcset);

                while (true) {
                    const p = permsiter.next();
                    if (p.len == 0) break;
                    trace("p {}\n", .{TermFmt.init(p)});
                    if (!gop.value_ptr.contains(p))
                        try gop.value_ptr.putNoClobber(arena, try arena.dupe(u8, p), {});
                }
            }
        }
        trace("perms.len {}\n", .{perms.count()});
        // Now group the remaining terms and see if any term can be covered
        // by a combination of terms.
        // ei_range = set()
        // groups = dict()
        var ei: TermSet = .{};
        var ei_range: TermSet = .{};
        var groups: TermMapOrdered = .{};
        {
            // for t in terms:
            //     n = self.__get_term_rank(t, len(perms[t]))
            //     if n not in groups:
            //         groups[n] = set()
            //     groups[n].add(t)
            var iter = terms.keyIterator();
            while (iter.next()) |k| {
                const t = k.*;
                const permset = perms.get(t).?;
                const permcount = permset.count();
                // var psetiter = permset.keyIterator();
                // while (psetiter.next()) |p|
                //     trace("p {}\n", .{TermFmt.init(p.*)});
                trace("permset.len {} perms[t] {}\n", .{ permcount, TermSetFmt.init(permset) });
                const n = try getTermRank(t, @intCast(u16, permcount));
                const gop = try groups.getOrPut(arena, n);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                try gop.value_ptr.putNoClobber(arena, t, {});
            }
        }
        trace("groups.len {}\n", .{groups.count()});
        // for t in sorted(list(groups.keys()), reverse=True):
        //     for g in groups[t]:
        //         if not perms[g] <= ei_range:
        //             ei.add(g)
        //             ei_range |= perms[g]
        // if len(ei) == 0:
        //     ei = set(['-' * self.n_bits])
        groups.sort(struct {
            pub fn lessThan(_: @This(), aidx: usize, bidx: usize) bool {
                trace("TODO: verify this sort. aidx {} bidx {}\n", .{ aidx, bidx });
                return aidx > bidx;
            }
        }{});
        {
            for (groups.keys()) |t| {
                trace("t {}\n", .{t});
                const gs = groups.get(t).?;
                var giter = gs.keyIterator();
                while (giter.next()) |gk| {
                    const g = gk.*;
                    const gperms = perms.get(g).?;
                    const issubset = isSubSet(TermSet, gperms, ei_range);
                    trace("perms[g] {}\n", .{TermSetFmt.init(gperms)});
                    trace("ei_range {}\n", .{TermSetFmt.init(ei_range)});
                    trace("not perms[g] <= ei_range {}\n", .{!issubset});
                    if (!issubset) {
                        try ei.put(self.allocator, try self.allocator.dupe(u8, g), {});
                        var gpermsiter = gperms.keyIterator();
                        while (gpermsiter.next()) |gpk|
                            try ei_range.put(arena, gpk.*, {});
                    }
                }
            }
            if (ei.count() == 0) {
                const x = try self.allocator.alloc(u8, self.bitcount / 2);
                std.mem.set(u8, x, @enumToInt(Element.dash));
                try ei.put(self.allocator, x, {});
            }
        }
        return ei;
    }
};

fn todo(msg: []const u8) noreturn {
    @panic(msg);
}

const show_trace = true;
pub fn trace(comptime fmt: []const u8, args: anytype) void {
    if (@hasDecl(@This(), "show_trace"))
        std.debug.print(fmt, args);
}

const allr = std.testing.allocator;
test "basic" {
    var q = QuineMcCluskey.init(allr, &.{ 2, 6, 10, 14 }, &.{}, .{});
    var terms = try q.simplify();
    defer q.deinitTermSet(&terms);
    const s = try std.fmt.allocPrint(allr, "{}", .{QuineMcCluskey.TermSetFmt.init(terms)});
    defer allr.free(s);
    try std.testing.expectEqualStrings("--10", s);

    // std.debug.print("terms {}\n", .{QuineMcCluskey.TermSetFmt.init(terms)});
}
