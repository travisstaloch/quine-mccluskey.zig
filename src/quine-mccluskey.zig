//! adapted from https://github.com/tpircher/quine-mccluskey/
//! TODO: add support for xor/xnor reductions

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

pub const parsing = @import("parsing.zig");

/// this imlementation packs 2 elements per byte.
/// this packed representation is called a 'dual' here.
/// this means that each byte holds 2 'Element' where
/// Element is an enum(u4){zero,one,xor,xnor,dash}.
pub fn QuineMcCluskey(comptime _T: type) type {
    return struct {
        allocator: Allocator,
        bitcount: TLen,
        ones: []const T,
        dontcares: []const T,
        terms: []Term,
        flags: std.enums.EnumSet(enum { use_xor }),
        reduced_implicants: TermSet = .{},

        comptime {
            assert(std.math.isPowerOfTwo(TBitSize));
            assert(fromDual(toDual(0x10)) == fromDual(Dual.init(1, 0)));
        }
        pub const T = _T;

        /// b is intentionally before a so that the byte 0x10 is
        /// equivalent to Dual{.a = 1, .b = 0}
        pub const Dual = packed struct {
            b: u4,
            a: u4,

            pub fn init(a: u4, b: u4) Dual {
                return .{ .a = a, .b = b };
            }
        };

        pub const Element = enum(u4) { zero, one, xnor, xor, dash, invalid };
        pub const Term = []const u8;
        pub const TBitSize = @bitSizeOf(T);
        pub const TLog2 = std.math.Log2Int(T);

        pub const TLen = std.meta.Int(.unsigned, std.math.log2_int(T, TBitSize) + 1);
        pub const TLog2Signed = std.meta.Int(.signed, std.math.log2_int(T, TBitSize) + 1);

        pub inline fn toDual(c: u8) Dual {
            return @bitCast(Dual, c);
        }
        pub inline fn toDualPtr(c: *u8) *Dual {
            return @ptrCast(*Dual, c);
        }
        pub inline fn fromDual(dual: Dual) u8 {
            return @bitCast(u8, dual);
        }

        pub const TSet = std.AutoHashMapUnmanaged(T, void);
        pub const TList = std.ArrayListUnmanaged(T);
        pub const TermSet = std.StringArrayHashMapUnmanaged(void);
        pub const GroupKey = [3]u8;
        pub const TermList = std.ArrayListUnmanaged(Term);
        pub const TermMap = std.AutoHashMapUnmanaged(GroupKey, TermSet);
        pub const TermMapOrdered = std.ArrayHashMapUnmanaged(u16, TermSet, std.array_hash_map.AutoContext(u16), true);
        pub const PermMap = std.StringHashMapUnmanaged(TermSet);

        pub const zero = @enumToInt(Element.zero);
        pub const one = @enumToInt(Element.one);
        pub const xor = @enumToInt(Element.xor);
        pub const xnor = @enumToInt(Element.xnor);
        pub const dash = @enumToInt(Element.dash);
        pub const invalid = @enumToInt(Element.invalid);

        /// used by printing methods
        pub var comma_delim = ", ";

        const Self = @This();
        const Options = struct { bitcount: ?TLen = null };
        pub fn init(allocator: Allocator, ones: []const T, dontcares: []const T, options: Options) Self {
            return .{
                .allocator = allocator,
                .ones = ones,
                .dontcares = dontcares,
                .bitcount = if (options.bitcount) |bc| bc else 0,
                .flags = .{},
                .terms = &.{},
            };
        }

        pub fn initAndReduce(allocator: Allocator, ones: []const T, dontcares: []const T, options: Options) !Self {
            var qm = init(allocator, ones, dontcares, options);
            try qm.reduce();
            return qm;
        }

        pub fn deinit(self: *Self) void {
            self.deinitTermSet(&self.reduced_implicants);
        }

        pub fn freeTerms(self: *Self, terms: []Term) void {
            for (terms) |term| self.allocator.free(term);
            self.allocator.free(terms);
        }
        pub fn freeTermSetKeys(self: *Self, termset: *TermSet) void {
            for (termset.keys()) |k|
                self.allocator.free(k);
        }
        pub fn deinitTermSet(self: *Self, termset: *TermSet) void {
            self.freeTermSetKeys(termset);
            termset.deinit(self.allocator);
        }

        // allocate a slice and pack t into its bytes
        pub fn toTerm(allocator: Allocator, t: T, bitcount: TLen) !Term {
            var terms = try allocator.alloc(u8, bitcount);
            return toTermBuf(terms, t, bitcount);
        }
        // pack t into buf
        pub fn toTermBuf(buf: []u8, t: T, bitcount: TLen) Term {
            // trace("toTermBuf t {} bitcount {}\n", .{ t, bitcount });
            var mutt = t;

            var i = bitcount - 1;
            while (true) : (i -= 1) {
                const dual = toDualPtr(&buf[i]);
                dual.b = @truncate(u1, mutt);
                mutt >>= 1;
                dual.a = @truncate(u1, mutt);
                if (i == 0) break;
                mutt >>= 1;
            }
            return buf[0..bitcount];
        }
        /// widen e into a byte wide human readable representation
        pub fn elementToByte(e: Element) u8 {
            return switch (e) {
                .zero => '0',
                .one => '1',
                .xor => '^',
                .xnor => '~',
                .dash => '-',
            };
        }

        /// widen e into a byte wide human readable representation
        pub fn nibbleToByte(e: u4) u8 {
            return switch (e) {
                zero => '0',
                one => '1',
                xor => '^',
                xnor => '~',
                dash => '-',
                else => std.debug.panic("invalid nibble {}", .{e}),
            };
        }

        /// widen e's nibbles into a 2-byte wide human readable representation
        pub fn nibblesToBytes(e: u8) [2]u8 {
            const dual = toDual(e);
            return .{ nibbleToByte(dual.a), nibbleToByte(dual.b) };
        }
        /// pack a human readable byte into nibble
        pub fn byteToNibble(b: u8) u4 {
            return switch (b) {
                '0' => zero,
                '1' => one,
                '^' => xor,
                '~' => xnor,
                '-' => dash,
                else => std.debug.panic("invalid byte {}", .{b}),
            };
        }

        /// pack a human readable bytes into single byte
        pub fn bytesToNibbles(bytes: [2]u8) u8 {
            return fromDual(.{ .a = byteToNibble(bytes[0]), .b = byteToNibble(bytes[1]) });
        }

        /// wrapper for printing Terms, can be passed to print functions like this:
        ///   std.debug.print("{}", .{TermFmt.init(term, bitcount)});
        pub const TermFmt = struct {
            term: Term,
            bitcount: TLen,
            pub fn init(term: Term, bitcount: TLen) TermFmt {
                return .{ .term = term, .bitcount = bitcount };
            }

            pub fn format(self: TermFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                // sometimes have to skip leading nibble so that extra nibbles aren't shown in output.
                // for instance when bitcount == 3, 2 bytes/4 nibbles are required, skiplen will equal 1
                const skiplen = std.math.sub(usize, self.term.len * 2, self.bitcount) catch return;
                for (self.term) |e, i| {
                    const bytes = nibblesToBytes(e);
                    if (i * 2 >= skiplen)
                        try writer.writeByte(bytes[0]);
                    if (i * 2 + 1 >= skiplen)
                        try writer.writeByte(bytes[1]);
                }
            }
        };

        /// wrapper for printing slice of Terms, can be passed to print functions like this:
        ///   std.debug.print("{}", .{TermsFmt.init(terms, delimiter, bitcount)});
        pub const TermsFmt = struct {
            terms: []const Term,
            delimiter: []const u8,
            bitcount: TLen,
            pub fn init(terms: []const Term, delimiter: []const u8, bitcount: TLen) TermsFmt {
                return .{ .terms = terms, .delimiter = delimiter, .bitcount = bitcount };
            }

            pub fn format(self: TermsFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                for (self.terms) |term, i| {
                    if (i != 0) _ = try writer.write(self.delimiter);
                    try writer.print("{}", .{TermFmt.init(term, self.bitcount)});
                }
            }
        };

        /// wrapper for printing TermSets, can be passed to print functions like this:
        ///   std.debug.print("{}", .{TermSetFmt.init(termset, delimiter, bitcount)});
        pub const TermSetFmt = struct {
            termset: TermSet,
            delimiter: []const u8,
            bitcount: TLen,
            pub fn init(termset: TermSet, delimiter: []const u8, bitcount: TLen) TermSetFmt {
                return .{ .termset = termset, .delimiter = delimiter, .bitcount = bitcount };
            }

            pub fn format(self: TermSetFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                for (self.termset.keys()) |k, i| {
                    if (i != 0) _ = try writer.write(self.delimiter);
                    try writer.print("{}", .{TermFmt.init(k, self.bitcount)});
                }
            }
        };

        /// allocates self.reduced_implicants which can be freed by calling self.deinit().
        /// reduced_implicants may be printed out like this:
        ///   std.debug.print("{}\n", .{TermsFmt.init(self.reduced_implicants, ", ", self.bitcount)});
        pub fn reduce(self: *Self) !void {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            const arenaallr = arena.allocator();
            defer arena.deinit();

            var ones: TermList = .{};
            var dontcares: TermList = .{};

            // find max bitcount if not provided
            if (self.bitcount == 0) {
                for (self.ones) |x|
                    self.bitcount = std.math.max(self.bitcount, TBitSize - @clz(T, x));
                for (self.dontcares) |x|
                    self.bitcount = std.math.max(self.bitcount, TBitSize - @clz(T, x));
            }
            trace("bitcount {} ones {b}:{any}\n", .{ self.bitcount, self.ones, self.ones });

            for (self.ones) |x| {
                const term = try toTerm(arenaallr, x, try std.math.divCeil(TLen, self.bitcount, 2));
                try ones.append(arenaallr, term);
            }
            for (self.dontcares) |x| {
                const term = try toTerm(arenaallr, x, try std.math.divCeil(TLen, self.bitcount, 2));
                try dontcares.append(arenaallr, term);
            }

            self.reduced_implicants = try self.simplifyLos(ones.items, dontcares.items, arenaallr);
        }

        pub fn simplifyLos(self: *Self, ones: []Term, dontcares: []Term, arena: Allocator) !TermSet {
            var terms: TermSet = .{};
            for (ones) |x| try terms.put(arena, try arena.dupe(u8, x), {});
            for (dontcares) |x| try terms.put(arena, try arena.dupe(u8, x), {});

            if (terms.count() == 0) return TermSet{};

            // FIXME: optimize - deinit the arena after each of these 3 function calls
            var prime_implicants = try self.getPrimeImplicants(arena, &terms);
            defer self.deinitTermSet(&prime_implicants);
            trace("\n\nprime_implicants {} {}\n", .{ prime_implicants.count(), TermSetFmt.init(prime_implicants, comma_delim, self.bitcount) });

            var dcset: TSet = .{};
            for (self.dontcares) |t| try dcset.put(arena, t, {});

            var essential_implicants = try self.getEssentialImplicants(arena, &prime_implicants, dcset);
            defer self.deinitTermSet(&essential_implicants);
            trace("\n\nessential_implicants {} {}\n", .{ essential_implicants.count(), TermSetFmt.init(essential_implicants, comma_delim, self.bitcount) });

            var reduced_implicants = try self.reduceImplicants(arena, &essential_implicants, dcset);
            trace("\n\nreduced_implicants {} {}\n", .{ reduced_implicants.count(), TermSetFmt.init(reduced_implicants, comma_delim, self.bitcount) });
            return reduced_implicants;
        }

        fn termCount(t: Term, e: Element) u8 {
            var r: u8 = 0;
            for (t) |byte| {
                const dual = toDual(byte);
                r += @boolToInt(dual.a == @enumToInt(e));
                r += @boolToInt(dual.b == @enumToInt(e));
            }
            return r;
        }

        pub fn getPrimeImplicants(self: *Self, arena: Allocator, terms: *TermSet) !TermSet {
            const ngroups = self.bitcount + 1;
            trace("getPrimeImplicants terms {}\n", .{TermSetFmt.init(terms.*, comma_delim, self.bitcount)});

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
            var t2 = try arena.alloc(u8, try std.math.divCeil(TLen, self.bitcount, 2));

            while (true) {
                groups.clearRetainingCapacity();
                {
                    for (terms.keys()) |t| {
                        const key = GroupKey{ termCount(t, .one), termCount(t, .xor), termCount(t, .xnor) };
                        // trace("t {} key {any}\n", .{ TermFmt.init(t), key });
                        assert(key[1] == 0 or key[2] == 0);
                        const gop = try groups.getOrPut(arena, key);
                        if (!gop.found_existing) gop.value_ptr.* = .{};
                        try gop.value_ptr.put(arena, t, {});
                    }
                }

                {
                    var groupsiter = groups.iterator();
                    while (groupsiter.next()) |it| {
                        trace("group{any}: {} {}\n", .{ it.key_ptr.*, it.value_ptr.*.count(), TermSetFmt.init(it.value_ptr.*, comma_delim, self.bitcount) });
                    }
                }

                used.clearRetainingCapacity();
                terms.clearRetainingCapacity();

                // Find prime implicants
                var groupsiter = groups.iterator();
                while (groupsiter.next()) |it| {
                    var key_next = it.key_ptr.*;
                    key_next[0] += 1;
                    const group_next = groups.get(key_next) orelse continue;
                    const group = it.value_ptr.*;
                    for (group.keys()) |t1| {
                        // trace("t1 {}\n", .{TermFmt.init(t1)});
                        for (t1) |cs1, i| {
                            const dual = toDual(cs1);
                            // check both nibbles
                            inline for (comptime std.meta.fieldNames(Dual)) |f| {
                                const c1 = @field(dual, f);
                                if (c1 == zero) {
                                    std.mem.copy(u8, t2, t1);
                                    var dual2 = toDual(t2[i]);
                                    @field(dual2, f) = one;
                                    t2[i] = fromDual(dual2);
                                    // trace("t2 {}\n", .{TermFmt.init(t2)});
                                    if (group_next.contains(t2)) {
                                        const t12 = try arena.dupe(u8, t1);
                                        var dual12 = toDual(t12[i]);
                                        @field(dual12, f) = dash;
                                        t12[i] = fromDual(dual12);
                                        // trace("{} {} {}\n", .{ TermFmt.init(t1, self.bitcount), TermFmt.init(t2, self.bitcount), TermFmt.init(t12, self.bitcount) });
                                        try used.put(arena, t1, {});
                                        try used.put(arena, try arena.dupe(u8, t2), {});
                                        try terms.put(arena, t12, {});
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
                var gvalsiter = groups.valueIterator();
                while (gvalsiter.next()) |g| {
                    for (g.keys()) |t| {
                        if (!used.contains(t))
                            try marked.put(arena, t, {});
                    }
                }
                // TODO: audit marked inconsistency w/ qm.py
                trace("groups.len {} used.len {} marked.len {}\n", .{ groups.count(), used.count(), marked.count() });
                if (used.count() == 0) break;
            }

            trace("groups.len {} used.len {} marked.len {}\n", .{ groups.count(), used.count(), marked.count() });

            // Prepare the list of prime implicants
            var result: TermSet = .{};
            for (marked.keys()) |k| {
                try result.put(self.allocator, try self.allocator.dupe(u8, k), {});
            }

            var gvaluesiter = groups.valueIterator();
            while (gvaluesiter.next()) |g| {
                for (g.keys()) |k| {
                    const dupe = try self.allocator.dupe(u8, k);
                    const gop = try result.getOrPut(self.allocator, dupe);
                    if (gop.found_existing) self.allocator.free(dupe);
                }
            }
            return result;
        }

        fn termRank(t: u4) u16 {
            return if (t == 0 or t == invalid) 0 else @as(u16, 1) << t - 1;
        }

        fn getTermRank(term: Term, term_range: u16) !u16 {
            comptime {
                assert(5 == invalid);
                assert(4 == dash);
                assert(3 == xor);
                assert(2 == xnor);
                assert(1 == one);
                assert(0 == zero);
                assert(8 == termRank(dash));
                assert(4 == termRank(xor));
                assert(2 == termRank(xnor));
                assert(1 == termRank(one));
                assert(0 == termRank(zero));
                assert(0 == termRank(invalid));
            }
            var n: u16 = 0;
            for (term) |byte| {
                const dual = toDual(byte);
                n += termRank(dual.a);
                n += termRank(dual.b);
            }
            return 4 * term_range + n;
        }

        pub fn isSubSet(comptime Set: type, maybe_subset: Set, set: Set) bool {
            if (maybe_subset.count() > set.count()) return false;
            for (maybe_subset.keys()) |k| {
                if (!set.contains(k)) return false;
            }
            return true;
        }

        pub fn setsEqual(comptime Set: type, a: Set, b: Set) bool {
            if (a.count() != b.count()) return false;
            for (a.keys()) |k| {
                if (!b.contains(k)) return false;
            }
            return true;
        }

        pub const Permutations = struct {
            value: Term,
            n_bits: TLog2,
            n_xor: u8,
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

            inline fn setDualFieldIf(is_a: bool, dest_byte: *u8, src_byte: u8) void {
                const dest_dual = toDualPtr(dest_byte);
                const src_dual = toDual(src_byte);
                if (is_a)
                    dest_dual.a = src_dual.a
                else
                    dest_dual.b = src_dual.b;
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
                        const is_a = @truncate(i1, self.i) == 0;
                        const dual = toDual(self.value[i]);
                        const ele = if (is_a) dual.a else dual.b;
                        switch (ele) {
                            zero, one => setDualFieldIf(is_a, &self.res[i], self.value[i]),
                            dash => if (self.direction == 1) {
                                setDualFieldIf(is_a, &self.res[i], fromDual(Dual.init(zero, zero)));
                            } else {
                                const dualres = toDual(self.res[i]);
                                const nibble = if (is_a) dualres.a else dualres.b;
                                if (nibble == zero) {
                                    setDualFieldIf(is_a, &self.res[i], fromDual(Dual.init(one, one)));
                                    self.direction = 1;
                                }
                            },
                            xor => todo("xor"),
                            xnor => todo("xnor"),
                            invalid => todo("invalid"),
                            else => std.debug.panic("invalid TermElement {}", .{ele}),
                        }
                        if (trace_this and i < self.value.len)
                            trace("next()2 i {} direction {} value[i] {c} res[i] {c}\n", .{
                                i,
                                self.direction,
                                nibblesToBytes(self.value[i]),
                                nibblesToBytes(self.res.get(i)),
                            });
                    }

                    self.i += self.direction;
                    if (std.math.cast(TLen, self.i)) |i| {
                        if (i == self.n_bits * 2) {
                            self.direction = -1;
                            self.i = self.n_bits * 2 - 1;

                            // TODO: verify this works
                            var t: T = 0;
                            const skiplen = std.math.sub(usize, self.value.len * 2, self.n_bits) catch unreachable;
                            var j: TLog2 = 0;
                            for (self.res) |c| {
                                const dual = toDual(c);
                                if (j >= skiplen) {
                                    const b = @boolToInt(dual.a == one);
                                    t |= @as(T, b) << j;
                                }
                                j += 1;
                                if (j >= skiplen) {
                                    const b = @boolToInt(dual.b == one);
                                    t |= @as(T, b) << j;
                                }
                                if (j == TBitSize - 1) break;
                                j += 1;
                            }

                            if (trace_this) {
                                var buf: A = undefined;
                                const tterm = toTermBuf(&buf, t, self.n_bits);
                                trace(
                                    "t {b:0>8} tterm {} self.res {} self.exclude.contains(t) {}\n",
                                    .{ t, TermFmt.init(tterm), TermFmt.init(@as(A, self.res)[0..self.n_bits]), self.exclude.contains(t) },
                                );
                            }
                            if (!self.exclude.contains(t)) {
                                return self.res[0..self.n_bits];
                            }
                        }
                    }
                }
                return &[0]u8{};
            }
        };

        fn collectPerms(arena: Allocator, term: Term, result: *TermSet, excludes: TSet) !void {
            var permsiter = Permutations.init(term, excludes);
            while (true) {
                const p = permsiter.next();
                if (p.len == 0) break;
                try result.putNoClobber(arena, try arena.dupe(u8, p), {});
            }
        }

        pub fn getEssentialImplicants(self: *Self, arena: Allocator, terms: *TermSet, dcset: TSet) !TermSet {

            // Create all permutations for each term in terms.
            var perms: PermMap = .{};
            {
                for (terms.keys()) |term| {
                    const gop = try perms.getOrPut(arena, term);
                    if (!gop.found_existing) gop.value_ptr.* = .{};
                    // trace("t {}\n", .{TermFmt.init(term)});
                    try collectPerms(arena, term, gop.value_ptr, dcset);
                    // trace("term {} perms {}\n", .{ TermFmt.init(term, self.bitcount), gop.value_ptr.count() });
                }
            }
            trace("perms.len {}\n", .{perms.count()});
            // Now group the remaining terms and see if any term can be covered
            // by a combination of terms.
            var ei: TermSet = .{};
            var ei_range: TermSet = .{};
            var groups: TermMapOrdered = .{};

            for (terms.keys()) |t| {
                const permset = perms.get(t).?;
                const permcount = permset.count();
                trace("t {} permcount {} perms[t] {}\n", .{ TermFmt.init(t, self.bitcount), permcount, TermSetFmt.init(permset, comma_delim, self.bitcount) });
                const n = try getTermRank(t, @intCast(u16, permcount));
                const gop = try groups.getOrPut(arena, n);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                try gop.value_ptr.putNoClobber(arena, t, {});
            }

            trace("groups.len {}\n", .{groups.count()});
            groups.sort(struct {
                keys: []const u16,
                pub fn lessThan(this: @This(), aidx: usize, bidx: usize) bool {
                    return this.keys[aidx] > this.keys[bidx];
                }
            }{ .keys = groups.keys() });

            // trace("groups.keys :", .{});
            // for (groups.keys()) |k| {
            //     trace("{}, ", .{k});
            // }
            // trace("\n", .{});
            // for (groups.keys()) |t| {
            //     const gt = groups.get(t).?;
            //     trace("t {} {}\n", .{ t, TermSetFmt.init(gt, comma_delim, self.bitcount) });
            //     for (gt.keys()) |g| {
            //         const gperms = perms.get(g).?;
            //         trace("  {}: {}\n", .{ TermFmt.init(g, self.bitcount), TermSetFmt.init(gperms, comma_delim, self.bitcount) });
            //     }
            // }

            var giter = groups.iterator();
            while (giter.next()) |git| {
                const term = git.key_ptr.*;
                const group = git.value_ptr.*;
                trace("t {} gs.len {} ei_range.len {}\n", .{ term, group.count(), ei_range.count() });
                // const gs = groups.get(t).?;
                for (group.keys()) |g| {
                    const gperms = perms.get(g).?;
                    const issubset = isSubSet(TermSet, gperms, ei_range);
                    // trace("perms[{}] {} {}\n", .{ TermFmt.init(g, self.bitcount), gperms.count(), TermSetFmt.init(gperms, comma_delim, self.bitcount) });
                    // // trace("ei_range {} {}\n", .{ ei_range.count(), TermSetFmt.init(ei_range, comma_delim, self.bitcount) });
                    // trace("ei_range {}\n", .{ei_range.count()});
                    // trace("not perms[g] <= ei_range {}\n", .{!issubset});
                    if (!issubset) {
                        try ei.put(self.allocator, try self.allocator.dupe(u8, g), {});
                        for (gperms.keys()) |gpk| {
                            try ei_range.put(arena, gpk, {});
                        }
                    }
                }
            }
            if (ei.count() == 0) {
                const x = try self.allocator.alloc(u8, try std.math.divCeil(TLen, self.bitcount, 2));
                std.mem.set(u8, x, fromDual(.{ .a = dash, .b = dash }));
                try ei.put(self.allocator, x, {});
            }

            return ei;
        }

        fn elementCount(term: Term, e: u4, bitcount: TLen) TLen {
            var result: TLen = 0;
            const skiplen = term.len * 2 - bitcount;
            var j: TLen = 0;
            for (term) |byte| {
                const dual = toDual(byte);
                if (j >= skiplen)
                    result += @boolToInt(dual.a == e);
                j += 1;
                if (j >= skiplen)
                    result += @boolToInt(dual.b == e);
                j += 1;
            }
            return result;
        }

        fn complexity(term: Term, bitcount: TLen) f32 {
            var result: f32 = 0;
            result += 1.00 * @intToFloat(f32, elementCount(term, one, bitcount));
            result += 1.50 * @intToFloat(f32, elementCount(term, zero, bitcount));
            result += 1.25 * @intToFloat(f32, elementCount(term, xor, bitcount));
            result += 1.75 * @intToFloat(f32, elementCount(term, xnor, bitcount));
            return result;
        }

        fn ltByComplexity(bitcount: TLen, a: Term, b: Term) bool {
            return complexity(a, bitcount) < complexity(b, bitcount);
        }

        fn combineImplicants(self: Self, arena: Allocator, a: Term, b: Term, dcset: TSet) !Term {
            var permutations_a: TermSet = .{};
            try collectPerms(arena, a, &permutations_a, dcset);
            var permutations_b: TermSet = .{};
            try collectPerms(arena, b, &permutations_b, dcset);
            var a_potential = try arena.dupe(u8, a);
            var b_potential = try arena.dupe(u8, b);

            // FIXME: optimize - seems like these loops could be combined
            // const a_term_dcs = try indices(arena, a, dash);
            // for (a_term_dcs) |index| a_potential[index] = b[index];
            for (a) |_, i| {
                const adual = toDual(a[i]);
                const bdual = toDual(b[i]);
                const apotdual = toDualPtr(&a_potential[i]);
                if (adual.a == dash) apotdual.a = bdual.a;
                if (adual.b == dash) apotdual.b = bdual.b;
            }
            // const b_term_dcs = try indices(arena, b, dash);
            // for (b_term_dcs) |index| b_potential[index] = a[index];
            for (b) |_, i| {
                const adual = toDual(a[i]);
                const bdual = toDual(b[i]);
                const bpotdual = toDualPtr(&b_potential[i]);
                if (bdual.a == dash) bpotdual.a = adual.a;
                if (bdual.b == dash) bpotdual.b = adual.b;
            }
            for (permutations_b.keys()) |bp| try permutations_a.put(arena, bp, {});
            var valid: TermList = .{};
            var set: TermSet = .{};
            try collectPerms(arena, a_potential, &set, dcset);
            if (setsEqual(TermSet, set, permutations_a))
                try valid.append(arena, a_potential);
            set.clearRetainingCapacity();
            try collectPerms(arena, b_potential, &set, dcset);
            if (setsEqual(TermSet, set, permutations_a))
                try valid.append(arena, a_potential);
            // trace("valid {}\n", .{TermsFmt.init(valid.items, comma_delim, self.bitcount)});
            _ = self;
            std.sort.sort(Term, valid.items, self.bitcount, ltByComplexity);
            return if (valid.items.len > 0)
                valid.items[0]
            else
                &[0]u8{};
        }

        pub fn reduceImplicants(self: *Self, arena: Allocator, implicants: *TermSet, dcset: TSet) !TermSet {
            trace("implicants.len {}\n", .{implicants.count()});
            while (true) {
                outer: for (implicants.keys()) |a, i| {
                    for (implicants.keys()[i + 1 ..]) |b| {
                        const replacement = try self.combineImplicants(arena, a, b, dcset);
                        if (replacement.len > 0) {
                            trace("a {} b {} replacement {}\n", .{ TermFmt.init(a, self.bitcount), TermFmt.init(b, self.bitcount), TermFmt.init(replacement, self.bitcount) });
                            _ = implicants.swapRemove(a);
                            _ = implicants.swapRemove(b);
                            try implicants.put(arena, replacement, {});
                            break :outer;
                        }
                    }
                } else break;
            }

            var coverage: PermMap = .{};
            var combined_len: usize = 0;
            {
                for (implicants.keys()) |implicant| {
                    const gop = try coverage.getOrPut(arena, implicant);
                    if (!gop.found_existing) gop.value_ptr.* = .{};
                    // trace("implicant {}\n", .{TermFmt.init(implicant)});
                    try collectPerms(arena, implicant, gop.value_ptr, dcset);
                    combined_len += gop.value_ptr.count();
                }
            }
            trace("coverage.len {} combined {}\n", .{ coverage.count(), combined_len });

            var others_coverage: TermSet = .{};
            var redundant: std.ArrayListUnmanaged(Term) = .{};
            while (true) {
                redundant.clearRetainingCapacity();
                var coviter = coverage.iterator();
                while (coviter.next()) |it| {
                    const this_implicant = it.key_ptr.*;
                    const this_coverage = it.value_ptr.*;
                    others_coverage.clearRetainingCapacity();
                    var coviter2 = coverage.keyIterator();
                    while (coviter2.next()) |other_implicantp| {
                        const other_implicant = other_implicantp.*;
                        if (std.mem.eql(u8, other_implicant, this_implicant)) continue;
                        const cov = coverage.get(other_implicant).?;
                        for (cov.keys()) |n|
                            try others_coverage.put(arena, n, {});
                    }
                    const issubset = isSubSet(TermSet, this_coverage, others_coverage);
                    if (issubset) trace("this_coverage.len {} others_coverage.len {}\n", .{ this_coverage.count(), others_coverage.count() });
                    if (issubset)
                        try redundant.append(arena, this_implicant);
                }
                trace("redundant {}\n", .{redundant.items.len});
                if (redundant.items.len > 0) {
                    std.sort.sort(Term, redundant.items, self.bitcount, ltByComplexity);
                    const worst = redundant.items[redundant.items.len - 1];
                    trace("worst {}\nsorted redundant {}\n", .{ TermFmt.init(worst, self.bitcount), TermsFmt.init(redundant.items, comma_delim, self.bitcount) });
                    _ = coverage.remove(worst);
                } else break;
            }

            var result: TermSet = .{};
            var citer = coverage.keyIterator();
            while (citer.next()) |k|
                try result.put(self.allocator, try self.allocator.dupe(u8, k.*), {});
            if (result.count() == 0) {
                const x = try self.allocator.alloc(u8, try std.math.divCeil(TLen, self.bitcount, 2));
                std.mem.set(u8, x, fromDual(.{ .a = dash, .b = dash }));
                try result.put(self.allocator, x, {});
            }
            return result;
        }
    };
}

fn todo(msg: []const u8) noreturn {
    @panic(msg);
}

pub const show_trace = false;
pub fn trace(comptime fmt: []const u8, args: anytype) void {
    if (@hasDecl(@This(), "show_trace") and @field(@This(), "show_trace"))
        std.debug.print(fmt, args);
}
