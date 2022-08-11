//! adapated from http://eprints.lqdtu.edu.vn/id/eprint/10375/1/main_camera_ready.pdf

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

/// T is the storage type for implicants.
/// 2bits are required per variable.  so for 16 variables, use T = u32.
/// this is because each 2bits of an implicant stores either 0, 1 or 2
/// where 2 means 'dash' or '-'
pub fn QuineMcCluskey(comptime _T: type) type {
    return struct {
        variables: []const []const u8,
        allocator: Allocator,
        minterms: []T,
        ones: []const THalf,
        dontcares: []const THalf,
        prime_list: TList = .{},
        final_prime_list: TList = .{},

        const Self = @This();
        pub const T = _T;
        pub const TLog2 = std.math.Log2Int(T);
        pub const TBitSize = @bitSizeOf(T);
        pub const TLen = LenInt(T);
        pub const TList = std.ArrayListUnmanaged(T);
        pub const THalf = std.meta.Int(.unsigned, TBitSize / 2);
        pub const THalfLen = LenInt(TLen);
        pub const THalfLog2 = std.math.Log2Int(THalf);
        pub const THalfList = std.ArrayListUnmanaged(THalf);
        pub const Masks = [TBitSize / @bitSizeOf(usize)]usize;
        pub const TUsedMap = std.AutoArrayHashMapUnmanaged(T, bool);
        pub const TBitSet = std.StaticBitSet(TBitSize);

        pub fn LenInt(comptime U: type) type {
            return std.meta.Int(.unsigned, std.math.log2(@bitSizeOf(U)) + 1);
        }

        pub const ImplicantList = struct {
            ts: TUsedMap = .{},

            pub fn deinit(ilist: *ImplicantList, allocator: Allocator) void {
                ilist.ts.deinit(allocator);
            }
            pub fn append(ilist: *ImplicantList, allocator: Allocator, t: T, used: bool) !void {
                try ilist.ts.put(allocator, t, used);
            }
            pub fn appendSliceTs(ilist: *ImplicantList, allocator: Allocator, ts: []const T, useds: struct { useds: []const bool = &.{} }) !void {
                if (useds.useds.len == 0) {
                    for (ts) |t| {
                        const gop = try ilist.ts.getOrPut(allocator, t);
                        if (!gop.found_existing) gop.value_ptr.* = false;
                    }
                } else {
                    assert(useds.useds.len == ts.len);
                    for (useds.useds) |used, i| {
                        if (!used) {
                            try ilist.ts.put(allocator, ts[i], used);
                        }
                    }
                }
            }
        };

        /// stores indices of another (non specified) list and divides it into groups by index
        pub const Groups = struct {
            /// indices for some other list, describes other list's ordering
            indices: std.ArrayListUnmanaged(usize),
            /// bounds are indexes of `indices` - dividing them into groups
            bounds: std.ArrayListUnmanaged(usize),

            pub fn deinit(self: *Groups, allocator: Allocator) void {
                self.indices.deinit(allocator);
                self.bounds.deinit(allocator);
            }
        };

        pub fn init(
            allocator: Allocator,
            variables: []const []const u8,
            ones: []const THalf,
            dontcares: []const THalf,
        ) Self {
            return Self{
                .variables = variables,
                .allocator = allocator,
                .ones = ones,
                .dontcares = dontcares,
                .minterms = &.{},
            };
        }

        pub fn deinit(self: *Self) void {
            self.prime_list.deinit(self.allocator);
            self.final_prime_list.deinit(self.allocator);
            self.allocator.free(self.minterms);
        }

        pub fn initAndReduce(allocator: Allocator, variables: []const []const u8, ones: []const THalf, dontcares: []const THalf) !Self {
            var qm = Self.init(allocator, variables, ones, dontcares);
            try qm.reduce();
            return qm;
        }

        pub fn reduce(qm: *Self) !void {
            try qm.findPrimeTs();
            try qm.findEssentialPrimeTs();
        }

        /// widens each bit into two. allocates and returns caller owned slice.
        /// allows for storing additional state in each 2bits.
        pub fn widenT(t: THalf) T {
            @setEvalBranchQuota(TBitSize);
            var tmut = t;
            var s: T = 0;
            comptime var i: TLog2 = 0;
            inline while (i < TBitSize / 2) : (i += 1) {
                // trace("{b:0>32} {b:0>32}\n", .{ t, s });
                s |= @as(T, @truncate(u1, tmut)) << (i * 2);
                tmut >>= 1;
            }
            comptime assert(i == @bitSizeOf(THalf));
            return s;
        }

        /// compress a T which has been widened into half as many bits.
        /// only 2bit sequences matching `target` are counted as 1 bits in result.
        pub fn compressT(t: T, comptime target: u2) THalf {
            var result: THalf = 0;
            var tmut = t;
            comptime var i: TLog2 = 0;
            inline while (i < TBitSize / 2) : (i += 1) {
                result <<= 1;
                switch (@truncate(u2, tmut)) {
                    target => result |= 1,
                    else => {},
                }
                tmut >>= 2;
            }
            comptime assert(i == @bitSizeOf(THalf));
            return @bitReverse(THalf, result);
        }

        /// helper for making sequence of 2s: 101010...
        fn allDashesT() T {
            @setEvalBranchQuota(TBitSize);
            var t: T = 0;
            var i: TLen = 0;
            while (i < TBitSize) : (i += 2) {
                t <<= 2;
                t |= 2;
            }
            assert(i == TBitSize);
            return t;
        }

        pub const dashes = allDashesT();
        pub const dashes_complement = ~dashes;

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

        /// wrapper for printing an implicant in binary with dashes
        pub const TFmt = struct {
            t: T,
            bitcount: TLog2,
            pub fn init(t: T, bitcount: TLog2) TFmt {
                return .{ .t = t, .bitcount = bitcount };
            }
            pub fn format(self: TFmt, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                var t = bitReverse(T, self.t, self.bitcount, 2);
                var i: usize = 0;
                while (i < self.bitcount) : (i += 1) {
                    try writer.writeByte(switch (@truncate(u2, t)) {
                        0b00 => '0',
                        0b01 => '1',
                        0b10 => '-',
                        else => @panic("invalid bit"),
                    });
                    t >>= 2;
                }
            }
        };

        /// wrapper for printing an implicant as variable names
        pub const TFmtVars = struct {
            t: T,
            variables: []const []const u8,
            pub fn init(t: T, variables: []const []const u8) TFmtVars {
                return .{ .t = t, .variables = variables };
            }
            pub fn format(self: TFmtVars, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
                const bitcount = @intCast(TLog2, self.variables.len);
                var t = bitReverse(T, self.t, bitcount, 2);
                var i: usize = 0;
                while (i < bitcount) : (i += 1) {
                    const variable = self.variables[i];
                    switch (@truncate(u2, t)) {
                        0b00 => try writer.print("{s}'", .{variable}),
                        0b01 => try writer.print("{s}", .{variable}),
                        0b10 => {},
                        else => @panic("invalid bit"),
                    }
                    t >>= 2;
                }
            }
        };

        /// 1. given `variables`={"A", "B"} and `delimiter`=" + "
        ///   - AB'         => (A and !B)
        ///   - AB' + C'D   => (A and !B) or (!C and D)
        /// 2. given `variables`={"AB", "A", "B"} and `delimiter`=" + "
        ///   - AB'         => (!AB)
        ///     - if "A" were before "AB" in `variables` then the result would be the same as 1.
        pub fn parseTerms(allocator: Allocator, input: []const u8, delimiter: []const u8, variables: []const []const u8) ![]THalf {
            // TODO: move this into separate parsers module
            const m = @intCast(THalfLen, variables.len - 1);
            var iter = std.mem.split(u8, input, delimiter);
            var terms: THalfList = .{};
            defer terms.deinit(allocator);
            while (iter.next()) |_term| {
                const term = std.mem.trim(u8, _term, &std.ascii.spaces);
                var termval: THalf = 0;
                var i: usize = 0;
                while (i < term.len) {
                    const idx = for (variables) |v, j| {
                        if (std.mem.startsWith(u8, term[i..], v))
                            break @intCast(THalfLen, j);
                    } else return error.ParseError;
                    i += variables[idx].len;
                    const not = @boolToInt(i < term.len and term[i] == '\'');
                    // this is just a branchless equivalent of:
                    //   if(!not) termval |= (1 << (m-idx)) else i += 1;
                    termval |= (@as(THalf, not +% 1) << @intCast(THalfLog2, m - idx));
                    i += not;
                }
                try terms.append(allocator, termval);
            }

            var result = terms.toOwnedSlice(allocator);
            std.sort.sort(THalf, result, {}, comptime ltByPopCount(THalf));
            return result;
        }

        /// like parseTerms except
        ///   - returns []T rather than []THalf which allows for dashes
        ///   - each term starts out all dashes. this way you can tell the
        ///     difference between a missing term variable and a negated term variable
        pub fn parseTerms2(allocator: Allocator, input: []const u8, delimiter: []const u8, variables: []const []const u8) ![]T {
            // TODO: move this into separate parsers module
            var iter = std.mem.split(u8, input, delimiter);
            var terms: TList = .{};
            defer terms.deinit(allocator);
            while (iter.next()) |_term| {
                const term = std.mem.trim(u8, _term, &std.ascii.spaces);
                var termval: std.StaticBitSet(TBitSize) = if (TBitSize <= 64) .{ .mask = dashes } else .{ .masks = @bitCast(Masks, dashes) };
                var i: usize = 0;
                while (i < term.len) {
                    const idx = for (variables) |v, j| {
                        if (std.mem.startsWith(u8, term[i..], v))
                            break @intCast(TLog2, j);
                    } else return error.ParseError;
                    i += variables[idx].len;
                    const not = i < term.len and term[i] == '\'';
                    termval.setValue(variables.len - idx - 1, !not);
                    i += @boolToInt(not);
                }
                try terms.append(allocator, if (TBitSize <= 64) termval.mask else @bitCast(T, termval.masks));
            }

            var result = terms.toOwnedSlice(allocator);
            std.sort.sort(T, result, {}, comptime ltByPopCount(T));
            return result;
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
                    return @popCount(U, lhs) < @popCount(U, rhs);
                }
            }.func;
        }

        pub fn ltIndicesByPopCount(ts: []const T, lhs: usize, rhs: usize) bool {
            return @popCount(T, ts[lhs]) < @popCount(T, ts[rhs]);
        }

        pub fn printEssentialTerms(self: *Self, writer: anytype, delimiter: []const u8) !void {
            for (self.final_prime_list.items) |imp, i| {
                if (i != 0) _ = try writer.write(delimiter);
                try writer.print("{}", .{TFmtVars.init(imp, self.variables)});
            }
            if (self.final_prime_list.items.len == 0)
                try writer.writeByteNTimes('-', self.variables.len);
        }

        pub fn printEssentialTermsBin(self: *Self, writer: anytype, delimiter: []const u8) !void {
            for (self.final_prime_list.items) |imp, i| {
                if (i != 0) _ = try writer.write(delimiter);
                try writer.print("{}", .{TFmt.init(imp, @intCast(TLog2, self.variables.len))});
            }
            if (self.final_prime_list.items.len == 0)
                try writer.writeByteNTimes('-', self.variables.len);
        }

        /// group all `ilist` indices into `groups.indices`
        /// sorted by the corresponding ilist entry's popCount.
        /// store group boundaries in `groups.bounds`.
        /// note that `groups.bounds` sores indices to the `groups.indices` list
        pub fn makeGroups(self: Self, ilist: ImplicantList, groups: *Groups) !void {
            groups.indices.items.len = 0;
            groups.bounds.items.len = 0;
            const keys = ilist.ts.keys();
            // trace("keys {b:0>8}\n", .{keys});
            for (keys) |_, i| {
                try groups.indices.append(self.allocator, i);
            }
            std.sort.sort(usize, groups.indices.items, keys, ltIndicesByPopCount);
            var popcount: T = 0;
            for (groups.indices.items) |idx, idxidx| {
                const it = keys[idx];
                // trace("makeGroups it {b:0>8}\n", .{it});
                const pc = @popCount(T, it);
                if (pc != popcount) {
                    try groups.bounds.append(self.allocator, idxidx);
                    popcount = pc;
                }
            }
            try groups.bounds.append(self.allocator, groups.indices.items.len);
        }

        /// reduce and populate `prime_list`
        fn findPrimeTs(self: *Self) !void {
            const m = @intCast(TLog2, self.variables.len);

            var minterms = if (self.minterms.len != self.ones.len + self.dontcares.len) blk: {
                self.allocator.free(self.minterms);
                break :blk try self.allocator.alloc(T, self.ones.len + self.dontcares.len);
            } else self.minterms;
            for (self.ones) |x, i| minterms[i] = widenT(x);
            for (self.dontcares) |x, i| minterms[self.ones.len + i] = widenT(x);
            std.sort.sort(T, minterms, {}, comptime ltByPopCount(T));
            self.minterms = minterms;

            var implicant_lists = [1]ImplicantList{.{}} ** 2;
            var prime_list: ImplicantList = .{};
            var groups: Groups = .{ .indices = .{}, .bounds = .{} };
            defer {
                for (implicant_lists) |*l| l.deinit(self.allocator);
                prime_list.deinit(self.allocator);
                groups.deinit(self.allocator);
            }
            var listid: u1 = 0;
            try implicant_lists[listid].appendSliceTs(self.allocator, self.minterms, .{});

            var combined = true;
            while (combined) : (listid +%= 1) {
                combined = false;
                const implicant_list = implicant_lists[listid];
                const new_implicant_list = &implicant_lists[listid +% 1];

                try self.makeGroups(implicant_list, &groups);
                trace("groups: indices {any} bounds {any}\n", .{ groups.indices.items, groups.bounds.items });
                const ilistkeys = implicant_list.ts.keys();
                var boundid: usize = 0;
                var bound0: usize = 0;
                while (boundid + 1 < groups.bounds.items.len) : (boundid += 1) {
                    var bound1 = groups.bounds.items[boundid];
                    var bound2 = groups.bounds.items[boundid + 1];
                    const group0 = groups.indices.items[bound0..bound1];
                    const group1 = groups.indices.items[bound1..bound2];
                    trace("round {} bound0 {} bound1 {} bound2 {} \ng0\n", .{ boundid, bound0, bound1, bound2 });
                    var reversed = false;
                    for (group0) |i| {
                        const t0 = ilistkeys[i];
                        trace("  {} {} {}\n", .{ i, TFmt.init(t0, m), TFmtVars.init(t0, self.variables) });
                    }
                    trace("g1\n", .{});
                    for (group1) |i| {
                        const t1 = ilistkeys[i];
                        trace("  {} {} {}\n", .{ i, TFmt.init(t1, m), TFmtVars.init(t1, self.variables) });
                    }
                    for (group0) |id0| {
                        const it0 = ilistkeys[id0];
                        // TODO: optimization - if reversed, iterate group1 backwards
                        _ = reversed;
                        for (group1) |id1| {
                            const it1 = ilistkeys[id1];

                            var xor = (it0 ^ it1) & dashes_complement;
                            if ((it0 & dashes) == (it1 & dashes) and @popCount(T, xor) == 1) {
                                combined = true;
                                var newitem = it0 & ~xor;
                                xor <<= 1;
                                newitem |= xor;
                                trace(
                                    "combining {}:{} {}:{} newitem {}:{}\n",
                                    .{ id0, TFmt.init(it0, m), id1, TFmt.init(it1, m), TFmt.init(newitem, m), TFmtVars.init(newitem, self.variables) },
                                );
                                implicant_list.ts.getPtr(it0).?.* = true;
                                implicant_list.ts.getPtr(it1).?.* = true;

                                try new_implicant_list.append(self.allocator, newitem, false);
                            }
                        }
                        reversed = !reversed;
                    }
                    bound0 = bound1;
                }

                var iter = implicant_list.ts.iterator();
                while (iter.next()) |it|
                    if (!it.value_ptr.*)
                        try prime_list.ts.put(self.allocator, it.key_ptr.*, it.value_ptr.*);
                implicant_lists[listid].ts.clearRetainingCapacity();
            }

            var primeiter = prime_list.ts.iterator();
            while (primeiter.next()) |it| {
                trace("prime {}:{}-{}\n", .{ TFmt.init(it.key_ptr.*, m), TFmtVars.init(it.key_ptr.*, self.variables), it.value_ptr.* });
                assert(!it.value_ptr.*);
                try self.prime_list.append(self.allocator, it.key_ptr.*);
            }
        }

        fn rank(t: T) T {
            var tmut = t;
            var result: T = 0;
            var i: TLen = 0;
            while (i < TBitSize / 2 - 1) : (i += 1) {
                result += @as(T, switch (@truncate(u2, tmut)) {
                    0 => 0,
                    1 => 1,
                    2 => 8,
                    else => @panic("unreachable"),
                });
                tmut >>= 2;
            }
            return result;
        }

        pub fn gtByRank(_: void, lhs: PrimeCoverage, rhs: PrimeCoverage) bool {
            return rank(lhs.prime) > rank(rhs.prime);
        }

        /// represents a prime and the minterms which it covers.
        /// minterms is a bitset of indices into minterms list
        /// cover means that the prime has been reduced from the minterms
        pub const PrimeCoverage = struct {
            // minterm_set_indices: std.StaticBitSet(TBitSize),
            minterm_set_indices: std.DynamicBitSetUnmanaged,
            prime: T,
            added: bool = false,
        };

        fn findEssentialPrimeTs(self: *Self) !void {

            // make list of prime coverages
            // generate bitset of indices from permutations of prime
            var primecovs: std.ArrayListUnmanaged(PrimeCoverage) = .{};
            defer {
                for (primecovs.items) |*pc| pc.minterm_set_indices.deinit(self.allocator);
                primecovs.deinit(self.allocator);
            }
            for (self.prime_list.items) |prime| {
                const pdashes = prime & dashes;
                // find first minterm which is the prime with 0s for dashes
                const pnodashes = prime & dashes_complement;
                // trace("{}:{} prime {b:0>8} pnodashes {} minterms {any}\n", .{ TFmt.init(prime, @intCast(TLog2, self.variables.len)), TFmtVars.init(prime, self.variables), prime, pnodashes, self.minterms });
                const mts_idx0 = std.mem.indexOfScalar(T, self.minterms, pnodashes).?;
                var primecov: PrimeCoverage = .{ .minterm_set_indices = try std.DynamicBitSetUnmanaged.initEmpty(self.allocator, self.minterms.len), .prime = prime };
                primecov.minterm_set_indices.set(mts_idx0);

                // loop over dashes, adding to primecov.minterms
                var pdashes_bitset: TBitSet = if (TBitSize <= 64) .{ .mask = pdashes } else .{ .masks = @bitCast(Masks, pdashes) };
                var pdashes_iter = pdashes_bitset.iterator(.{});
                // trace("pdashes {b:0>8} pnodashes {b:0>8} prime {b:0>8}:{}\n", .{ pdashes, pnodashes, prime, TFmtVars.init(prime, self.variables) });
                while (pdashes_iter.next()) |dash_idx| {
                    const dash_mask = (@as(T, 1) << @intCast(TLog2, dash_idx)) >> 1;
                    const mts = primecov.minterm_set_indices;
                    var mts_iter = mts.iterator(.{});
                    while (mts_iter.next()) |mts_idx| {
                        const minterm = self.minterms[mts_idx] | dash_mask;
                        // trace("this minterms[mts_idx] {} minterm {} {} minterms.len {} minterms {any} \n", .{ self.minterms[mts_idx], minterm, TLog2, self.minterms.len, self.minterms });
                        const mt_idx = std.mem.indexOfScalar(T, self.minterms, minterm).?;
                        primecov.minterm_set_indices.set(mt_idx);
                    }
                }
                trace("{} minterms : ", .{TFmtVars.init(prime, self.variables)});
                var mtsiter = primecov.minterm_set_indices.iterator(.{});
                while (mtsiter.next()) |mtidx|
                    trace("{}, ", .{compressT(self.minterms[mtidx], 1)});
                trace("\n", .{});
                try primecovs.append(self.allocator, primecov);
            }

            const m = @intCast(TLog2, self.variables.len);
            var max_rank: usize = 1;
            std.sort.sort(PrimeCoverage, primecovs.items, {}, gtByRank);

            var victim: ?*PrimeCoverage = null;
            while (max_rank != 0) {
                max_rank = 0;
                var temp: ?*PrimeCoverage = null;
                for (primecovs.items) |*primecov| {
                    if (primecov.added) continue;
                    const prime = primecov.prime;
                    trace("prime {} ", .{TFmt.init(prime, m)});
                    if (victim) |v|
                        trace("victim {}\n", .{TFmt.init(v.prime, m)})
                    else
                        trace("victim null\n", .{});
                    // Let val-1 be the minterm set of a prime implicant
                    // victim is the prime implicant that is merged from the largest number of minterm
                    if (victim) |v| {
                        // remove victim minterms from this one
                        // trace("victim indices {b:0>16}\n", .{v.minterm_set_indices.mask});
                        // trace("before removal {b:0>16}\n", .{primecov.minterm_set_indices.mask});
                        const num_masks = (primecov.minterm_set_indices.bit_length + (@bitSizeOf(std.DynamicBitSetUnmanaged.MaskInt) - 1)) / @bitSizeOf(std.DynamicBitSetUnmanaged.MaskInt);
                        for ((primecov.minterm_set_indices.masks)[0..num_masks]) |*mask, i| mask.* &= ~(v.minterm_set_indices.masks)[i];
                        // trace("after removal  {b:0>16}\n", .{primecov.minterm_set_indices.mask});
                    }
                    const rk = primecov.minterm_set_indices.count();
                    if (rk > max_rank) {
                        max_rank = rk;
                        temp = primecov;
                    }
                }
                if (temp) |t| {
                    t.added = true;
                    trace("adding {}\n", .{TFmtVars.init(t.prime, self.variables)});
                    try self.final_prime_list.append(self.allocator, t.prime);
                }
                victim = temp;
            }
        }
    };
}

fn todo(msg: []const u8) noreturn {
    @panic(msg);
}

// const show_trace = true;
fn trace(comptime fmt: []const u8, args: anytype) void {
    if (@hasDecl(@This(), "show_trace"))
        std.debug.print(fmt, args);
}
