//! adapted from https://github.com/vj-ug/Quine-McCluskey-algorithm/blob/master/quinecplusplus.cpp

const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn QuineMcCluskey(comptime _T: type) type {
    return struct {
        table: BNumListList = .{},
        p_group: BNumListList = .{},
        final_group: BNumListList = .{},
        allocator: Allocator,
        dontcares: []const T = undefined,
        variables: []const []const u8,
        essentials: BNumSet = .{},

        const Self = @This();
        pub const T = _T;
        pub const TLog2 = std.math.Log2Int(T);
        pub const TBitSize = @bitSizeOf(T);
        pub const TLen = std.meta.Int(.unsigned, std.math.log2(TBitSize) + 1);

        pub const BNum = struct {
            number: T,
            dashes: T,
            used: bool,
        };

        pub const BNumList = std.ArrayListUnmanaged(BNum);
        pub const BNumListList = std.ArrayListUnmanaged(BNumList);
        pub fn init(allocator: Allocator, variables: []const []const u8) Self {
            return .{
                .allocator = allocator,
                .variables = variables,
            };
        }

        pub fn deinit(self: *Self) void {
            for (self.table.items) |*l| l.deinit(self.allocator);
            self.table.deinit(self.allocator);
            for (self.p_group.items) |*l| l.deinit(self.allocator);
            self.p_group.deinit(self.allocator);
            for (self.final_group.items) |*l| l.deinit(self.allocator);
            self.final_group.deinit(self.allocator);
            self.essentials.deinit(self.allocator);
        }

        fn resize(self: Self, l: *BNumListList, len: usize) !void {
            var i = l.items.len;
            try l.ensureTotalCapacity(self.allocator, len);
            l.expandToCapacity();
            while (i < l.items.len) : (i += 1) {
                // std.debug.print("i/count {}/{}\n", .{ i, count });
                l.items[i] = .{};
            }
        }

        pub fn createTable(self: *Self, ones: []const T, dontcares: []const T) !void {
            self.dontcares = dontcares;
            try self.createTableImpl(ones);
            try self.createTableImpl(dontcares);
        }

        fn createTableImpl(self: *Self, input: []const T) !void {
            for (input) |x| {
                const count = @popCount(T, x);
                if (count >= self.table.items.len) {
                    try self.resize(&self.table, count + 1);
                }
                // std.debug.print("count/len {}/{}\n", .{ count, self.table.items.len });
                try self.table.items[count].append(
                    self.allocator,
                    .{ .number = x, .dashes = 0, .used = false },
                );
            }
        }

        pub fn printTable(self: *Self, writer: anytype, comptime nbits: u16) !void {
            const bfmt = comptime std.fmt.comptimePrint("{{b:0>{}}}", .{nbits});
            _ = try writer.write("\ntable\n");
            for (self.table.items) |list, i| {
                try writer.print("{}", .{i});
                for (list.items) |it| {
                    try writer.print("\tm{}\t" ++ bfmt ++ "\n", .{ it.number, it.number });
                }
                _ = try writer.write("-------------------------------------\n");
            }
        }

        /// like the original table, but the paring of numbers from the original table-
        /// dashes are represented by a 1. for example original A=0011 B=1011, new number
        /// is -011 which is represented as C.number=A&B=0011,C.dashes=A^B=1000
        pub fn createPGroup(self: *Self) !void {
            for (self.table.items) |_, i| {
                for (self.table.items[i].items) |*itptr| {
                    const it = itptr.*;
                    for (self.table.items[i + 1].items) |*it2ptr| {
                        const it2 = it2ptr.*;
                        const number = it.number & it2.number;
                        const dashes = it.number ^ it2.number;

                        if (@popCount(T, dashes) != 1) continue;
                        itptr.used = true;
                        it2ptr.used = true;

                        const count = @popCount(T, number);
                        if (count >= self.p_group.items.len)
                            try self.resize(&self.p_group, count + 1);
                        try self.p_group.items[count].append(
                            self.allocator,
                            .{ .number = number, .dashes = dashes, .used = false },
                        );
                    }
                }
            }
        }

        pub fn printPGroup(self: *Self, writer: anytype) !void {
            // const bfmt = comptime std.fmt.comptimePrint("{{b:0>{}}}", .{nbits});
            _ = try writer.write("\np_group\n");
            for (self.p_group.items) |list, i| {
                try writer.print("{}", .{i});
                for (list.items) |it| {
                    _ = try writer.write("\t\t");
                    try printTermBin(it.number, it.dashes, writer, self.variables.len);
                    // try writer.print("\t\t" ++ bfmt ++ "{}\n", .{ it.number, it.dashes });
                    _ = try writer.write("\n");
                }
                _ = try writer.write("-------------------------------------\n");
            }
        }

        /// creates final table. works like p_group(). example; in p_group you have:
        /// A=-001 B=-011 -> C= -0-1 which will be represented as
        /// C.number=A&B=0001&0011=0001, and C.dashes=A^B^A.dashes=0001^0011^1000=1010.
        /// Computation is done only when A.dashes = b.dashes
        pub fn createFinalGroup(self: *Self) !void {
            for (self.p_group.items) |_, i| {
                for (self.p_group.items[i].items) |*itptr| {
                    const it = itptr.*;
                    for (self.p_group.items[i + 1].items) |*it2ptr| {
                        const it2 = it2ptr.*;
                        if (it.dashes != it2.dashes) continue;

                        const number = it.number & it2.number;
                        var dashes = it.number ^ it2.number;
                        if (@popCount(T, dashes) != 1) continue;

                        dashes ^= it.dashes;
                        itptr.used = true;
                        it2ptr.used = true;

                        const count = @popCount(T, number);
                        if (count >= self.final_group.items.len)
                            try self.resize(&self.final_group, count + 1);
                        try self.final_group.items[count].append(
                            self.allocator,
                            .{ .number = number, .dashes = dashes, .used = true },
                        );
                    }
                }
            }
        }

        pub fn printEssentialTerms(self: *Self, writer: anytype, delimiter: []const u8) !void {
            for (self.essentials.keys()) |bnum, i| {
                if (i != 0) _ = try writer.write(delimiter);
                try self.printTerm(bnum, writer);
            }
            if (self.essentials.keys().len == 0)
                try writer.writeByteNTimes('-', self.variables.len);
        }

        pub fn printEssentialTermsBin(self: *Self, writer: anytype, delimiter: []const u8) !void {
            for (self.essentials.keys()) |bnum, i| {
                if (i != 0) _ = try writer.write(delimiter);
                try self.printTermBin(bnum, writer);
            }
            if (self.essentials.keys().len == 0)
                try writer.writeByteNTimes('-', self.variables.len);
        }

        // print all the values from the final table, except for duplicates.
        // print all the unused numbers from original table and mid process table
        pub fn printFinalGroup(self: *Self, writer: anytype) !void {
            // const bfmt = comptime std.fmt.comptimePrint("{{b:0>{}}}", .{nbits});
            var seen: BNumList = .{};
            defer seen.deinit(self.allocator);
            _ = try writer.write("\nfinal_group\n");

            for (self.final_group.items) |list| {
                for (list.items) |it| {
                    if (!contains(seen, it)) {
                        try printTermBin(it.number, it.dashes, writer, self.variables.len);
                        _ = try writer.write("\n");
                        try seen.append(self.allocator, it);
                    }
                }
            }

            for (self.p_group.items) |list| {
                for (list.items) |it| {
                    if (!it.used) {
                        try printTermBin(it.number, it.dashes, writer, self.variables.len);
                        _ = try writer.write("\n");
                    }
                }
            }

            for (self.table.items) |list| {
                for (list.items) |it| {
                    if (!it.used) {
                        try printTermBin(it.number, it.dashes, writer, self.variables.len);
                        _ = try writer.write("\n");
                    }
                }
            }
        }

        // pub fn primeGroupIterAsync(self: *Self, out: *?BNum) !void {
        //     var seen: BNumList = .{};
        //     defer seen.deinit(self.allocator);

        //     for (self.final_group.items) |list| {
        //         for (list.items) |it| {
        //             if (!contains(seen, it)) {
        //                 out.* = it;
        //                 try seen.append(self.allocator, it);
        //                 suspend {}
        //             }
        //         }
        //     }

        //     for (self.p_group.items) |list| {
        //         for (list.items) |it| {
        //             if (!it.used) {
        //                 out.* = it;
        //                 suspend {}
        //             }
        //         }
        //     }

        //     for (self.table.items) |list| {
        //         for (list.items) |it| {
        //             if (!it.used) {
        //                 out.* = it;
        //                 suspend {}
        //             }
        //         }
        //     }
        //     out.* = null;
        // }

        pub fn PrimeGroupIteratorCb(comptime Cb: type, comptime UserCtx: type) type {
            return struct {
                pub const Error = @typeInfo(@typeInfo(Cb).Fn.return_type.?).ErrorUnion.error_set;

                pub fn primeGroupIterCb(self: *Self, comptime cb: Cb, user_ctx: UserCtx) Error!void {
                    var seen: BNumList = .{};
                    defer seen.deinit(self.allocator);

                    for (self.final_group.items) |list| {
                        for (list.items) |it| {
                            if (!contains(seen, it)) {
                                try cb(it, self, user_ctx);
                                try seen.append(self.allocator, it);
                            }
                        }
                    }

                    for (self.p_group.items) |list| {
                        for (list.items) |it| {
                            if (!it.used) {
                                try cb(it, self, user_ctx);
                            }
                        }
                    }

                    for (self.table.items) |list| {
                        for (list.items) |it| {
                            if (!it.used) {
                                try cb(it, self, user_ctx);
                            }
                        }
                    }
                }
            };
        }

        pub fn printTermBin(self: Self, bnum: BNum, writer: anytype) !void {
            const varslen = @intCast(TLen, self.variables.len);
            var n = bitReverse(bnum.number, varslen);
            var d = bitReverse(bnum.dashes, varslen);
            var count: T = 0;
            while (count < varslen) : (count += 1) {
                if (@truncate(u1, d) == 1) {
                    _ = try writer.write("-");
                } else {
                    try writer.print("{b:1}", .{@truncate(u1, n)});
                }

                n >>= 1;
                d >>= 1;
            }
        }

        pub fn printTerm(self: Self, bnum: BNum, writer: anytype) !void {
            const varslen = @intCast(TLen, self.variables.len);
            var n = bitReverse(bnum.number, varslen);
            var d = bitReverse(bnum.dashes, varslen);
            var count: TLen = 0;
            while (count < varslen) : (count += 1) {
                if (@truncate(u1, d) == 0) {
                    const s: []const u8 = if (@truncate(u1, n) == 0) "'" else "";
                    try writer.print("{s}{s}", .{ self.variables[count], s });
                }

                n >>= 1;
                d >>= 1;
            }
        }

        fn contains(seen: BNumList, n: BNum) bool {
            for (seen.items) |it| {
                if (n.number == it.number and n.dashes == it.dashes)
                    return true;
            }

            return false;
        }

        pub const BNumSet = std.AutoArrayHashMapUnmanaged(BNum, void);
        pub const PrimeInfo = struct {
            map: std.AutoHashMapUnmanaged(T, BNumSet) = .{},

            fn deinit(self: *PrimeInfo, allocator: Allocator) void {
                var iter = self.map.iterator();
                while (iter.next()) |it| {
                    it.value_ptr.deinit(allocator);
                }
                self.map.deinit(allocator);
            }
        };

        /// for a given `bnum`, reconstruct its input terms
        /// given a bnum={0100, 1000}, adds 4 and 12 to `prime_info.map`
        /// in binary {0100, (0100 | 1000)}
        fn primeGroupCb(bnum: BNum, qm: *Self, prime_info: *PrimeInfo) !void {
            var terms = std.AutoHashMap(T, void).init(qm.allocator);
            defer terms.deinit();
            try terms.put(bnum.number, {});
            var bitset = if (TBitSize <= 64)
                std.StaticBitSet(TBitSize){ .mask = bnum.dashes }
            else
                std.StaticBitSet(TBitSize){ .masks = @bitCast([TBitSize / @bitSizeOf(usize)]usize, bnum.dashes) };
            var biter = bitset.iterator(.{});
            while (biter.next()) |bitidx| {
                const mask = @as(T, 1) << @intCast(TLog2, bitidx);
                var setiter = terms.keyIterator();
                while (setiter.next()) |k|
                    try terms.put(k.* | mask, {});
            }

            var setiter = terms.keyIterator();
            while (setiter.next()) |kptr| {
                const k = kptr.*;
                if (std.mem.indexOfScalar(T, qm.dontcares, k) != null) continue;
                const gop = try prime_info.map.getOrPut(qm.allocator, k);
                if (!gop.found_existing) gop.value_ptr.* = .{};
                try gop.value_ptr.put(qm.allocator, bnum, {});
            }
        }

        const IterCb = PrimeGroupIteratorCb(@TypeOf(primeGroupCb), *PrimeInfo);
        pub fn createEssentialGroup(self: *Self) !void {
            var prime_info: PrimeInfo = .{};
            defer prime_info.deinit(self.allocator);

            try IterCb.primeGroupIterCb(self, primeGroupCb, &prime_info);
            var iter = prime_info.map.iterator();
            while (iter.next()) |it| {
                const bnumset = it.value_ptr.*;
                if (bnumset.count() == 1) {
                    const bnum = bnumset.keys()[0];
                    try self.essentials.put(self.allocator, bnum, {});
                }
            }
        }

        pub fn reduce(allocator: Allocator, ones: []const T, dontcares: []const T, variables: []const []const u8) !Self {
            var qm = Self.init(allocator, variables);
            try qm.createTable(ones, dontcares);
            try qm.createPGroup();
            try qm.createFinalGroup();
            try qm.createEssentialGroup();
            return qm;
        }

        // don't build lookup table for large int sizes as bitReverseLookup
        // ends up being very slow for TBitSize >= 2048
        const bitReverse = if (TBitSize >= 256) bitReverseLoop else bitReverseLookup;

        /// reverse only lower `len` bits
        fn bitReverseLoop(num: T, len: TLen) T {
            var reverse_num: T = 0;
            var i: TLen = 0;
            while (i < len) : (i += 1) {
                if ((num & (@as(T, 1) << @intCast(TLog2, i))) != 0)
                    reverse_num |= @as(T, 1) << @intCast(TLog2, ((len - 1) - i));
            }
            return reverse_num;
        }

        const FnTT = if (@import("builtin").zig_backend == .stage2_llvm)
            *const fn (T) T
        else
            fn (T) T;

        fn BitReverseLenFn(comptime bitlen: u16) FnTT {
            return struct {
                fn func(t: T) T {
                    const I = std.meta.Int(.unsigned, bitlen);
                    const i = @intCast(I, t);
                    return @bitReverse(I, i);
                }
            }.func;
        }

        const bit_rev_fns = blk: {
            var result: [TBitSize + 1]FnTT = undefined;
            comptime var bitlen: TLen = 1;
            inline while (bitlen <= TBitSize) : (bitlen += 1) {
                result[bitlen] = BitReverseLenFn(bitlen);
            }
            break :blk result;
        };

        /// reverse only lower `len` bits
        fn bitReverseLookup(t: T, len: TLen) T {
            return bit_rev_fns[len](t);
        }

        /// expected syntax: AB' means (A and !B)
        pub fn parseTerms(allocator: Allocator, input: []const u8, delimiter: []const u8, variables: []const []const u8) ![]T {
            var iter = std.mem.split(u8, input, delimiter);
            var terms: std.ArrayListUnmanaged(T) = .{};
            defer terms.deinit(allocator);
            while (iter.next()) |term| {
                var termi: T = 0;
                var i: usize = 0;
                while (i < term.len) {
                    var len: TLen = 1;
                    len += @boolToInt(i + 1 < term.len and term[i + 1] == '\'');
                    if (len == 1) {
                        const idx = for (variables) |v, j| {
                            if (std.mem.eql(u8, v, term[i..][0..v.len]))
                                break @intCast(TLog2, j);
                        } else null orelse unreachable;
                        termi |= @as(T, 1) << idx;
                    }
                    i += len;
                }
                try terms.append(allocator, bitReverse(termi, @intCast(TLen, variables.len)));
            }

            var result = terms.toOwnedSlice(allocator);
            std.sort.sort(T, result, {}, lessThan);
            return result;
        }

        pub fn lessThan(_: void, lhs: T, rhs: T) bool {
            return lhs < rhs;
        }
    };
}

fn todo(msg: []const u8) noreturn {
    @panic(msg);
}
