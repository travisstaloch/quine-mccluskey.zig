//! adapted from https://github.com/tpircher/quine-mccluskey/
//! TODO: add support for xor/xnor reductions

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

pub const parsing = @import("parsing.zig");

const is_debug_build = @import("builtin").mode == .Debug;

// pub const show_trace = true;
pub fn trace(comptime fmt: []const u8, args: anytype) void {
    if (@hasDecl(@This(), "show_trace") and @field(@This(), "show_trace"))
        std.debug.print(fmt, args);
}

// pub const show_profile = true;
pub fn profile(comptime fmt: []const u8, timer: *std.time.Timer, args: anytype) void {
    if (@hasDecl(@This(), "show_profile") and @field(@This(), "show_profile")) {
        const lap = timer.lap();
        const dur = std.fmt.fmtDuration(lap);
        std.debug.print(fmt ++ " {}\n", args ++ .{dur});
    }
}

/// this imlementation packs 2 elements per byte.
/// this packed representation is called a 'nibbles' here.
/// this means that each byte holds 2 'Element' where
/// Element is an enum(u4){ zero, one, xor, xnor, dash }.
pub fn QuineMcCluskey(comptime _T: type) type {
    return struct {
        allocator: Allocator,
        bitcount: TLen,
        ones: []const T,
        dontcares: []const T,
        flags: std.enums.EnumSet(enum { use_xor }),
        reduced_implicants: TermSet = .{},

        comptime {
            assert(std.math.isPowerOfTwo(TBitSize));
            assert(fromNibbles(toNibbles(0x10)) == fromNibbles(Nibbles.init(1, 0)));
        }
        pub const T = _T;

        /// b is intentionally before a so that the byte 0x10 is
        /// equivalent to Nibbles{.a = 1, .b = 0}
        pub const Nibbles = packed struct {
            b: u4,
            a: u4,

            pub fn init(a: u4, b: u4) Nibbles {
                return .{ .a = a, .b = b };
            }
        };

        pub const Element = enum(u4) { zero, one, xnor, xor, dash, invalid };
        pub const Term = []const u8;
        pub const TBitSize = @bitSizeOf(T);
        pub const TLog2 = std.math.Log2Int(T);
        pub const TLen = std.math.Log2IntCeil(T);
        pub const TLenSigned = std.meta.Int(.signed, std.math.log2_int(T, TBitSize) + 2);

        pub inline fn toNibbles(c: u8) Nibbles {
            return @bitCast(Nibbles, c);
        }
        pub inline fn toNibblesPtr(c: *u8) *Nibbles {
            return @ptrCast(*Nibbles, c);
        }
        pub inline fn fromNibbles(nibs: Nibbles) u8 {
            return @bitCast(u8, nibs);
        }

        pub const TSet = std.AutoHashMapUnmanaged(T, void);
        pub const TList = std.ArrayListUnmanaged(T);
        pub const TermSet = std.StringArrayHashMapUnmanaged(void);
        //
        // --- leaving this here temporarily incase i come back to using a custom ctx ---
        //
        // const TermSetCtx = struct {
        //     const nelemnts = @intCast(u32, std.meta.fields(Element).len);
        //     pub fn hash(_: TermSetCtx, key: []const u8) u32 {
        //         var result: u32 = 0;
        //         var i: u32 = 0;
        //         for (key) |c| {
        //             const nibs = toNibbles(c);
        //             result += i * nelemnts + nibs.a;
        //             i += 1;
        //             result += i * nelemnts + nibs.b;
        //             i += 1;
        //         }
        //         return result;
        //     }
        //     pub fn eql(_: TermSetCtx, a: []const u8, b: []const u8, _: usize) bool {
        //         return std.mem.eql(u8, a, b);
        //     }
        // };
        // pub const TermSet = std.ArrayHashMapUnmanaged([]const u8, void, TermSetCtx, false);
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

        /// allocate a slice and write bits of t into nibbles of it
        pub fn tToTerm(allocator: Allocator, t: T, bitcount: TLen) !Term {
            const halfbitcount = (bitcount + 1) / 2;
            var term = try allocator.alloc(u8, halfbitcount);
            return try tToTermBuf(term, t, bitcount);
        }

        /// write bits of t into nibbles of buf
        pub fn tToTermBuf(buf: []u8, t: T, bitcount: TLen) !Term {
            // trace("toTermBuf t {} halfbitcount {}\n", .{ t, halfbitcount });
            const halfbitcount = (bitcount + 1) / 2;
            if (buf.len < halfbitcount) return error.BufferTooSmall;
            var i: TLen = 0;
            while (true) {
                const idiv2 = i / 2;
                var mask = @as(T, 1) << @intCast(TLog2, bitcount - i - 1);
                // trace("\ni {} idiv2 {} mask {b} @clz(T, mask) {} halfbitcount {}\n", .{ i, idiv2, mask, @clz(T, mask), halfbitcount });
                const c = &buf[idiv2];
                const nibs = toNibblesPtr(c);
                nibs.a = if (i < bitcount) @truncate(u1, @boolToInt(mask & t != 0)) else ele_uninit;
                mask >>= 1;
                i += 1;
                nibs.b = if (i < bitcount) @truncate(u1, @boolToInt(mask & t != 0)) else ele_uninit;
                i += 1;
                if (i >= bitcount) break;
            }
            return buf;
        }

        pub fn termToT(term: Term, bitcount: TLen) !T {
            var t: T = 0;
            var i: TLen = 0;
            while (true) {
                const nibs = toNibbles(term[i / 2]);
                t <<= 1;
                t |= @truncate(u1, nibs.a);
                i += 1;
                if (i >= bitcount) break;

                t <<= 1;
                t |= @truncate(u1, nibs.b);
                i += 1;
                if (i >= bitcount) break;
            }
            return t;
        }

        /// allocate a slice and pack human readable 'bytes' into it.
        /// if bitcount is odd, last nibble will == ele_uninit (0xF)
        pub fn bytesToTerm(allocator: Allocator, bytes: []const u8, bitcount: TLen) !Term {
            const halfbitcount = (bitcount + 1) / 2;
            var term = try allocator.alloc(u8, halfbitcount);
            return try bytesToTermBuf(term, bytes, bitcount);
        }

        /// pack human readable 'bytes' into buf.
        /// if bitcount is odd, last nibble will == ele_uninit (0xF)
        pub fn bytesToTermBuf(buf: []u8, bytes: []const u8, bitcount: TLen) !Term {
            // trace("buf.len {} bitcount {} bytes.len {}\n", .{ buf.len, bitcount, bytes.len });
            const halfbitcount = (bitcount + 1) / 2;
            if (buf.len < halfbitcount) return error.BufferTooSmall;
            if (bytes.len < bitcount) return error.BufferTooSmall;
            var i: usize = 0;

            for (buf) |*c| {
                const nibs = toNibblesPtr(c);
                nibs.a = if (i < bitcount) byteToNibble(bytes[i]) else ele_uninit;
                i += 1;
                nibs.b = if (i < bitcount) byteToNibble(bytes[i]) else ele_uninit;
                i += 1;
                if (i >= bitcount) break;
            }
            return buf[0..halfbitcount];
        }

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
        pub fn nibbleToByte(e: u4) !u8 {
            return switch (e) {
                zero => '0',
                one => '1',
                xor => '^',
                xnor => '~',
                dash => '-',
                else => error.InvalidNibble,
            };
        }

        /// widen e's nibbles into a 2-byte wide human readable representation
        pub fn nibblesToBytes(e: u8) ![2]u8 {
            const nibs = toNibbles(e);
            return [2]u8{ try nibbleToByte(nibs.a), try nibbleToByte(nibs.b) };
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
            return fromNibbles(.{ .a = byteToNibble(bytes[0]), .b = byteToNibble(bytes[1]) });
        }

        /// wrapper for printing Terms, can be passed to print functions like this:
        ///   std.debug.print("{}", .{TermFmt.init(term, bitcount)});
        pub const TermFmt = struct {
            term: Term,
            bitcount: TLen,
            pub fn init(term: Term, bitcount: TLen) TermFmt {
                return .{ .term = term, .bitcount = bitcount };
            }

            /// skip invalid nibbles and write at most bitcount bytes
            pub fn format(self: TermFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                var i: usize = 0;
                for (self.term) |e| {
                    const nibs = toNibbles(e);
                    if (i >= self.bitcount) break;
                    if (nibbleToByte(nibs.a)) |a| {
                        try writer.writeByte(a);
                        i += 1;
                    } else |_| {}
                    if (i >= self.bitcount) break;
                    if (nibbleToByte(nibs.b)) |b| {
                        try writer.writeByte(b);
                        i += 1;
                    } else |_| {}
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

        /// wrapper for printing Terms as variable names, can be passed to print functions like this:
        ///   std.debug.print("{}", .{TermFmtVars.init(term, bitcount, &.{"A", "B", "C"}, " && ", "!", .before)});
        ///                                            ^ given term "1-0" prints "A && !C"
        pub const TermFmtVars = struct {
            term: Term,
            bitcount: TLen,
            variables: []const []const u8,
            separator: []const u8,
            not_symbol: []const u8,
            not_style: NotStyle,
            pub const NotStyle = enum { before, after };
            pub fn init(
                term: Term,
                bitcount: TLen,
                variables: []const []const u8,
                separator: []const u8,
                not_symbol: []const u8,
                not_style: NotStyle,
            ) TermFmtVars {
                return .{
                    .term = term,
                    .bitcount = bitcount,
                    .variables = variables,
                    .separator = separator,
                    .not_symbol = not_symbol,
                    .not_style = not_style,
                };
            }

            /// write at most bitcount variables
            pub fn format(self: TermFmtVars, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                // TODO: figure out how to gracefully handle this error as returning error.VariablesTooShort produces this error:
                //   error: expected type 'std.os.WriteError', found '@typeInfo(@typeInfo(@TypeOf(.quine-mccluskey.QuineMcCluskey(u32).TermFmtVars.format)).Fn.return_type.?).ErrorUnion.error_set'
                if (self.variables.len < self.bitcount)
                    std.debug.panic("variables.len {} too small. must be atleast bitcount {}\n", .{ self.variables.len, self.bitcount });
                var written_count: usize = 0;
                for (self.term) |e, i| {
                    const nibs = toNibbles(e);
                    if (i * 2 >= self.bitcount) break;
                    if (nibs.a == zero or nibs.a == one) {
                        if (written_count != 0)
                            _ = try writer.write(self.separator);
                        if (self.not_style == .before and nibs.a == zero)
                            _ = try writer.write(self.not_symbol);
                        _ = try writer.write(self.variables[i * 2]);
                        if (self.not_style == .after and nibs.a == zero)
                            _ = try writer.write(self.not_symbol);
                        written_count += 1;
                    }

                    if (i * 2 + 1 >= self.bitcount) break;
                    if (nibs.b == zero or nibs.b == one) {
                        if (written_count != 0)
                            _ = try writer.write(self.separator);
                        if (self.not_style == .before and nibs.b == zero)
                            _ = try writer.write(self.not_symbol);
                        _ = try writer.write(self.variables[i * 2 + 1]);
                        if (self.not_style == .after and nibs.b == zero)
                            _ = try writer.write(self.not_symbol);
                        written_count += 1;
                    }
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
                    self.bitcount = std.math.max(self.bitcount, TBitSize - @clz(x));
                for (self.dontcares) |x|
                    self.bitcount = std.math.max(self.bitcount, TBitSize - @clz(x));
            }
            trace("bitcount {} ones {b}:{any}\n", .{ self.bitcount, self.ones, self.ones });

            for (self.ones) |x| {
                const term = try tToTerm(arenaallr, x, self.bitcount);
                try ones.append(arenaallr, term);
            }
            for (self.dontcares) |x| {
                const term = try tToTerm(arenaallr, x, self.bitcount);
                try dontcares.append(arenaallr, term);
            }

            self.reduced_implicants = try self.simplifyLos(ones.items, dontcares.items, &arena);
        }

        pub fn simplifyLos(self: *Self, ones: []Term, dontcares: []Term, arena: *std.heap.ArenaAllocator) !TermSet {
            var terms: TermSet = .{};

            const arenaallr = arena.allocator();
            for (ones) |x| try terms.put(arenaallr, try arenaallr.dupe(u8, x), {});
            for (dontcares) |x| try terms.put(arenaallr, try arenaallr.dupe(u8, x), {});

            if (terms.count() == 0) return TermSet{};

            var timer = try std.time.Timer.start();
            var prime_implicants = blk: {
                var prime_implicants = try self.getPrimeImplicants(arenaallr, &terms);
                trace("\n\nprime_implicants {} {}\n", .{ prime_implicants.count(), TermSetFmt.init(prime_implicants, comma_delim, self.bitcount) });
                profile("{s: <25}", &timer, .{"getPrimeImplicants"});
                arena.deinit();
                arena.* = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                break :blk prime_implicants;
            };
            defer self.deinitTermSet(&prime_implicants);

            var dcset: TSet = .{};
            defer dcset.deinit(self.allocator);
            for (self.dontcares) |t| try dcset.put(self.allocator, t, {});
            _ = timer.lap();
            var essential_implicants = blk: {
                var essential_implicants = try self.getEssentialImplicants(arenaallr, &prime_implicants, dcset);
                profile("{s: <25}", &timer, .{"getEssentialImplicants"});
                trace("\n\nessential_implicants {} {}\n", .{ essential_implicants.count(), TermSetFmt.init(essential_implicants, comma_delim, self.bitcount) });
                arena.deinit();
                arena.* = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                break :blk essential_implicants;
            };
            defer self.deinitTermSet(&essential_implicants);
            _ = timer.lap();
            var reduced_implicants = try self.reduceImplicants(arenaallr, &essential_implicants, dcset);
            trace("\n\nreduced_implicants {} {}\n", .{ reduced_implicants.count(), TermSetFmt.init(reduced_implicants, comma_delim, self.bitcount) });
            profile("{s: <25}", &timer, .{"reduceImplicants"});
            return reduced_implicants;
        }

        fn termCount(t: Term, e: Element) u8 {
            var r: u8 = 0;
            for (t) |byte| {
                const nibs = toNibbles(byte);
                r += @boolToInt(nibs.a == @enumToInt(e));
                r += @boolToInt(nibs.b == @enumToInt(e));
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
            var t2 = try arena.alloc(u8, (self.bitcount + 1) / 2);

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

                // {
                //     var groupsiter = groups.iterator();
                //     while (groupsiter.next()) |it| {
                //         trace("group{any}: {} {}\n", .{ it.key_ptr.*, it.value_ptr.*.count(), TermSetFmt.init(it.value_ptr.*, comma_delim, self.bitcount) });
                //     }
                // }

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
                            const nibs = toNibbles(cs1);
                            // check both nibbles
                            inline for (comptime std.meta.fieldNames(Nibbles)) |f| {
                                const c1 = @field(nibs, f);
                                if (c1 == zero) {
                                    std.mem.copy(u8, t2, t1);
                                    var nib2 = toNibbles(t2[i]);
                                    @field(nib2, f) = one;
                                    t2[i] = fromNibbles(nib2);
                                    // trace("t2 {}\n", .{TermFmt.init(t2)});
                                    if (group_next.contains(t2)) {
                                        const t12 = try arena.dupe(u8, t1);
                                        var nib12 = toNibbles(t12[i]);
                                        @field(nib12, f) = dash;
                                        t12[i] = fromNibbles(nib12);
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

            var gvalsiter = groups.valueIterator();
            while (gvalsiter.next()) |g| {
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

        fn getTermRank(term: Term, term_range: u16, bitcount: TLen) !u16 {
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
            for (term) |byte, i| {
                const nibs = toNibbles(byte);
                const ii = i * 2;
                n += @boolToInt(ii < bitcount) * termRank(nibs.a);
                n += @boolToInt(ii + 1 < bitcount) * termRank(nibs.b);
            }
            return 4 * term_range + n;
        }

        pub fn isSubSet(comptime Set: type, maybe_subset: Set, set: Set, comptime mode: enum { strict, lenient }) bool {
            if (mode == .strict and maybe_subset.count() >= set.count()) {
                return false;
            } else if (mode == .lenient and maybe_subset.count() > set.count())
                return false;

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

        pub const ele_uninit = @as(u4, 0xf);
        pub const byte_uninit = @as(u8, 0xff);

        pub fn collectPerms(arena: Allocator, term: Term, result: *TermSet, excludes: TSet, bitcount: TLen) !void {
            const dashcount = termCount(term, .dash);
            const max = @as(T, 1) << @intCast(TLog2, dashcount);

            const trace_this = false;
            if (trace_this) trace("\nterm {} dashcount {} max {}:0b{b}\n", .{ TermFmt.init(term, bitcount), dashcount, max, max });

            var dashi_arr: [TBitSize]TLog2 = undefined; // dash indices
            var dashi_len: TLog2 = 0;
            for (term) |c, i| {
                const nibs = toNibbles(c);
                if (nibs.a == dash) {
                    dashi_arr[dashi_len] = @intCast(TLog2, i * 2);
                    dashi_len += 1;
                }
                if (nibs.b == dash) {
                    dashi_arr[dashi_len] = @intCast(TLog2, i * 2 + 1);
                    dashi_len += 1;
                }
            }
            const dashis = dashi_arr[0..dashi_len];
            var termcopy = try arena.dupe(u8, term);
            defer arena.free(termcopy);

            if (trace_this) trace("dashi {any}\n", .{dashis});
            var perm: T = 0;
            while (perm < max) : (perm += 1) {
                if (trace_this) trace("i {}:0b{b}\n", .{ perm, perm });

                for (dashis) |dash_idx, i| {
                    const bit = @boolToInt((@as(T, 1) << @intCast(TLog2, i)) & perm != 0);
                    const byteidx = dash_idx / 2;
                    const isa = @truncate(u1, dash_idx) == 0;
                    if (trace_this) trace("dash_idx {} i {} bit {} byteidx {} isa {}\n", .{ dash_idx, i, bit, byteidx, isa });
                    const nibs = toNibblesPtr(&termcopy[byteidx]);
                    if (isa) nibs.a = bit else nibs.b = bit;
                    if (trace_this) trace("termcopy {} ({})\n", .{ TermFmt.init(termcopy, bitcount), std.fmt.fmtSliceHexLower(termcopy) });
                }
                const t = try termToT(termcopy, bitcount);
                if (!excludes.contains(t))
                    try result.put(arena, try arena.dupe(u8, termcopy), {});
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
                    try collectPerms(arena, term, gop.value_ptr, dcset, self.bitcount);

                    if (is_debug_build) {
                        const permcount = gop.value_ptr.count();
                        const dashcount = termCount(term, .dash);
                        const expected_permcount = std.math.pow(usize, 2, dashcount);
                        // trace("t {} {} {}:{}\n", .{ TermFmt.init(term, self.bitcount), std.fmt.fmtSliceHexLower(term), permcount, TermSetFmt.init(gop.value_ptr.*, comma_delim, self.bitcount) });
                        // trace("expected_permcount {} permcount {} dashcount {} \n", .{ expected_permcount, permcount, permcount });
                        assert(dcset.count() != 0 or expected_permcount == permcount);
                    }
                    // trace("term {} perms {}\n", .{ TermFmt.init(term, self.bitcount), gop.value_ptr.count() });
                }
            }
            trace("perms.len {} dcset.len {}\n", .{ perms.count(), dcset.count() });
            // Now group the remaining terms and see if any term can be covered
            // by a combination of terms.
            var ei: TermSet = .{};
            var ei_range: TermSet = .{};
            var groups: TermMapOrdered = .{};

            var totaln: usize = 0;
            var totalperms: usize = 0;
            for (terms.keys()) |t| {
                const permset = perms.get(t).?;
                const permcount = permset.count();
                // trace("t{: >2} {} {} {}:{}\n", .{ i, TermFmt.init(t, self.bitcount), std.fmt.fmtSliceHexLower(t), permcount, TermSetFmt.init(permset, comma_delim, self.bitcount) });
                const n = try getTermRank(t, @intCast(u16, permcount), self.bitcount);
                if (is_debug_build) {
                    totaln += n;
                    totalperms += permcount;
                    const dashcount = termCount(t, .dash);
                    const expected_permcount = std.math.pow(usize, 2, dashcount);
                    assert(dcset.count() != 0 or expected_permcount == permcount);
                }
                const gop = try groups.getOrPut(arena, n);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                try gop.value_ptr.putNoClobber(arena, t, {});
            }

            trace("groups.len {} totaln {} totalperms {}\n", .{ groups.count(), totaln, totalperms });
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
                for (group.keys()) |g| {
                    const gperms = perms.get(g).?;
                    const issubset = isSubSet(TermSet, gperms, ei_range, .lenient);
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
                const x = try self.allocator.alloc(u8, (self.bitcount + 1) / 2);
                std.mem.set(u8, x, fromNibbles(.{ .a = dash, .b = dash }));
                try ei.put(self.allocator, x, {});
            }

            return ei;
        }

        fn elementCount(term: Term, e: u4, bitcount: TLen) TLen {
            var result: TLen = 0;
            var j: TLen = 0;
            for (term) |byte| {
                const nibs = toNibbles(byte);
                result += @boolToInt(j < bitcount) * @boolToInt(nibs.a == e);
                j += 1;
                result += @boolToInt(j < bitcount) * @boolToInt(nibs.b == e);
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

        // unlike the python impl, a single perms set is used rather than permuatations_a/b
        fn combineImplicants(self: Self, arena: Allocator, a: Term, b: Term, dcset: TSet, perms: *TermSet, valid: *TermList, set: *TermSet) !Term {
            perms.clearRetainingCapacity();
            try collectPerms(arena, a, perms, dcset, self.bitcount);
            try collectPerms(arena, b, perms, dcset, self.bitcount);
            var a_potential = try arena.dupe(u8, a);
            var b_potential = try arena.dupe(u8, b);

            // FIXME: optimize - seems like these loops could be combined
            for (a) |_, i| {
                const anibs = toNibbles(a[i]);
                const bnibs = toNibbles(b[i]);
                const apotnibs = toNibblesPtr(&a_potential[i]);
                if (anibs.a == dash) apotnibs.a = bnibs.a;
                if (anibs.b == dash) apotnibs.b = bnibs.b;
            }
            for (b) |_, i| {
                const anibs = toNibbles(a[i]);
                const bnibs = toNibbles(b[i]);
                const bpotnibs = toNibblesPtr(&b_potential[i]);
                if (bnibs.a == dash) bpotnibs.a = anibs.a;
                if (bnibs.b == dash) bpotnibs.b = anibs.b;
            }

            set.clearRetainingCapacity();
            valid.clearRetainingCapacity();

            try collectPerms(arena, a_potential, set, dcset, self.bitcount);
            if (setsEqual(TermSet, set.*, perms.*))
                try valid.append(arena, a_potential);
            set.clearRetainingCapacity();

            try collectPerms(arena, b_potential, set, dcset, self.bitcount);
            if (setsEqual(TermSet, set.*, perms.*))
                try valid.append(arena, a_potential);
            // trace("valid {}\n", .{TermsFmt.init(valid.items, comma_delim, self.bitcount)});
            std.sort.sort(Term, valid.items, self.bitcount, ltByComplexity);
            return if (valid.items.len > 0)
                valid.items[0]
            else
                &[0]u8{};
        }

        pub fn reduceImplicants(self: *Self, arena: Allocator, implicants: *TermSet, dcset: TSet) !TermSet {
            trace("implicants.len {}\n", .{implicants.count()});
            var valid: TermList = .{};
            var set: TermSet = .{};
            var perms_a: TermSet = .{};
            var timer = try std.time.Timer.start();
            while (true) {
                outer: for (implicants.keys()) |a, i| {
                    for (implicants.keys()[i + 1 ..]) |b| {
                        const replacement = try self.combineImplicants(arena, a, b, dcset, &perms_a, &valid, &set);
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
            profile("{s: <25}", &timer, .{"reduceImplicants1"});

            var coverage: PermMap = .{};
            var combined_len: usize = 0;
            {
                for (implicants.keys()) |implicant| {
                    const gop = try coverage.getOrPut(arena, implicant);
                    if (!gop.found_existing) gop.value_ptr.* = .{};
                    // trace("implicant {}\n", .{TermFmt.init(implicant)});
                    try collectPerms(arena, implicant, gop.value_ptr, dcset, self.bitcount);
                    if (is_debug_build)
                        combined_len += gop.value_ptr.count();
                }
            }
            profile("{s: <25}", &timer, .{"reduceImplicants2"});
            trace("coverage.len {} combined {}\n", .{ coverage.count(), combined_len });

            var redundant: std.ArrayListUnmanaged(Term) = .{};
            defer redundant.deinit(arena);

            while (true) {
                // check if each implicant's coverage is a subset of all the other coverages combined.
                // if so add to 'redundant'.
                // at each step remove worst redundant from coverages until no more redundants remain
                redundant.clearRetainingCapacity();
                var coviter = coverage.iterator();
                var others_total_count: usize = 0;
                while (coviter.next()) |it| {
                    const this_implicant = it.key_ptr.*;
                    const this_coverage = it.value_ptr.*;

                    // this_coverage is a subset of all other coverages if every element is found.
                    // so search for each element. if any one is not found in another coverage we
                    // know its not a subset.
                    const issubset = blk: for (this_coverage.keys()) |k| {
                        var coviter2 = coverage.iterator();
                        while (coviter2.next()) |it2| {
                            const other_implicant = it2.key_ptr.*;
                            if (std.mem.eql(u8, other_implicant, this_implicant)) continue;
                            const other_coverage = it2.value_ptr.*;
                            if (other_coverage.contains(k)) continue :blk;
                        }
                        // element not found
                        break :blk false;
                    } else true;

                    if (issubset) {
                        // trace("issubset {} this_coverage.len {} others_coverage.len {}\n", .{ issubset, this_coverage.count(), others_coverage.count() });
                        // trace("this {}:{} others {}\n", .{ TermFmt.init(this_implicant, self.bitcount), TermSetFmt.init(this_coverage, comma_delim, self.bitcount), TermSetFmt.init(others_coverage, comma_delim, self.bitcount) });
                        try redundant.append(arena, this_implicant);
                    }
                }
                trace("redundant {} others_total_count {}\n", .{ redundant.items.len, others_total_count });
                if (redundant.items.len > 0) {
                    std.sort.sort(Term, redundant.items, self.bitcount, ltByComplexity);
                    const worst = redundant.items[redundant.items.len - 1];
                    // trace("worst {}\nsorted redundant {}\n", .{ TermFmt.init(worst, self.bitcount), TermsFmt.init(redundant.items, comma_delim, self.bitcount) });
                    _ = coverage.remove(worst);
                } else break;
            }
            profile("{s: <25}", &timer, .{"reduceImplicants3"});

            var result: TermSet = .{};
            var citer = coverage.keyIterator();
            while (citer.next()) |k|
                try result.put(self.allocator, try self.allocator.dupe(u8, k.*), {});
            if (result.count() == 0) {
                const x = try self.allocator.alloc(u8, (self.bitcount + 1) / 2);
                std.mem.set(u8, x, fromNibbles(.{ .a = dash, .b = dash }));
                try result.put(self.allocator, x, {});
            }
            return result;
        }
    };
}

fn todo(msg: []const u8) noreturn {
    @panic(msg);
}
