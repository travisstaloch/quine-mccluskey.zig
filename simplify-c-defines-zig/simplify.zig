const std = @import("std");
const Allocator = std.mem.Allocator;
const qm = @import("quine-mccluskey");
const Qm32 = qm.QuineMcCluskey(u32);

const StringLine = struct {
    str: []const u8,
    line: usize,
};
const StringLineList = std.ArrayList(StringLine);
fn load_defines(allocator: Allocator, filepath: []const u8) !StringLineList {
    var res = StringLineList.init(allocator);
    const f = try std.fs.cwd().openFile(filepath, .{});
    defer f.close();
    const reader = std.io.bufferedReader(f.reader()).reader();
    var buf: [1024]u8 = undefined;
    var i: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| : (i += 1) {
        const trimmed = std.mem.trim(u8, line, &std.ascii.spaces);
        if (std.mem.indexOf(u8, trimmed, "#define") != null)
            try res.append(.{ .str = try allocator.dupe(u8, trimmed), .line = i });
    }
    return res;
}

const Run = struct {
    df: []const u8,
    start: usize,
    end: usize,

    pub fn format(value: Run, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{s} {} {}", .{ value.d, value.start, value.end });
    }
};

const RunList = std.ArrayList(Run);
fn find_runs(allocator: Allocator, lst: StringLineList) !RunList {
    var res = RunList.init(allocator);
    var i: usize = 0;
    while (i < lst.items.len) {
        const df = lst.items[i].str;
        const start = lst.items[i].line;
        i += 1;
        while (i < lst.items.len and std.mem.eql(u8, lst.items[i].str, df)) {
            i += 1;
        }
        try res.append(.{ .df = df, .start = start, .end = lst.items[i - 1].line });
    }
    return res;
}

const LineList = std.ArrayList(usize);
fn find_lines(allocator: Allocator, run: Run, filepath: []const u8) !LineList {
    var res = LineList.init(allocator);

    const f = try std.fs.cwd().openFile(filepath, .{});
    defer f.close();
    const reader = std.io.bufferedReader(f.reader()).reader();
    var buf: [1024]u8 = undefined;
    var i: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| : (i += 1) {
        const trimmed = std.mem.trim(u8, line, &std.ascii.spaces);
        if (std.mem.indexOf(u8, trimmed, run.df) != null and run.start <= i and i <= run.end)
            try res.append(i);
    }
    return res;
}

const StringList = std.ArrayList([]const u8);
fn get_defs(allocator: Allocator, linelist: LineList, filepath: []const u8) !StringList {
    var res = StringList.init(allocator);
    var slist = std.AutoHashMap(usize, void).init(allocator);
    defer slist.deinit();
    for (linelist.items) |l| try slist.put(l, {});
    const f = try std.fs.cwd().openFile(filepath, .{});
    defer f.close();
    const reader = std.io.bufferedReader(f.reader()).reader();
    var buf: [1024]u8 = undefined;
    var i: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| : (i += 1) {
        if (slist.contains(i + 1)) {
            const trimmed = std.mem.trim(u8, line, &std.ascii.spaces);
            const item = std.mem.trimLeft(u8, trimmed, "#if ");
            try res.append(try allocator.dupe(u8, item));
        }
    }
    return res;
}

const variables: []const []const u8 = &.{
    "__APPLE__",
    "__ZIG_OS_VERSION_MIN_MAJOR__ == 11",
    "__ZIG_OS_VERSION_MIN_MAJOR__ == 12",
    "__SYS_APPLEAPIOPTS_H__",
    "__APPLE_API_STANDARD",
    "__APPLE_API_STABLE",
    "__APPLE_API_STRICT_CONFORMANCE",
    "__APPLE_API_EVOLVING",
    "__APPLE_API_UNSTABLE",
    "__APPLE_API_PRIVATE",
    "__APPLE_API_OBSOLETE",
};

const def_variables: []const []const u8 = &.{
    "defined(__APPLE__)",
    "__ZIG_OS_VERSION_MIN_MAJOR__ == 11",
    "__ZIG_OS_VERSION_MIN_MAJOR__ == 12",
    "defined(__SYS_APPLEAPIOPTS_H__)",
    "defined(__APPLE_API_STANDARD)",
    "defined(__APPLE_API_STABLE)",
    "defined(__APPLE_API_STRICT_CONFORMANCE)",
    "defined(__APPLE_API_EVOLVING)",
    "defined(__APPLE_API_UNSTABLE)",
    "defined(__APPLE_API_PRIVATE)",
    "defined(__APPLE_API_OBSOLETE)",
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allr = arena.allocator();
    const args = try std.process.argsAlloc(allr);
    defer std.process.argsFree(allr, args);
    if (args.len < 2) return error.MissingFilePathArg;

    const filepath = args[1];
    const l = try load_defines(allr, filepath);
    const runs = try find_runs(allr, l);
    var qm32 = Qm32.init(allr, &.{}, &.{}, .{});
    defer qm32.deinit();

    // Tell Quine-McCluskey to not worry about what happens when
    // __ZIG_OS_VERSION_MIN_MAJOR__ is both 11 and 12 at the same time, and when
    // __ZIG_OS_VERSION_MIN_MAJOR__ is not 11 and not 12 at the same time.
    var dontcares: Qm32.TList = .{};
    const dcstrs: []const []const u8 = &.{ "-00--------", "-11--------" };
    const bitcount = @intCast(Qm32.TLen, dcstrs[0].len);
    var perms: Qm32.TermSet = .{};
    defer qm32.deinitTermSet(&perms);
    for (dcstrs) |dcstr| {
        perms.clearRetainingCapacity();
        const termdc = try Qm32.bytesToTerm(allr, dcstr, bitcount);
        try Qm32.collectPerms(allr, termdc, &perms, .{}, bitcount);
        for (perms.keys()) |k| try dontcares.append(allr, try Qm32.termToT(k, bitcount));
    }

    const alldashesterm = try Qm32.bytesToTerm(allr, "-" ** bitcount, bitcount);
    defer allr.free(alldashesterm);

    for (runs.items[0..1]) |run| {
        const ls = try find_lines(allr, run, filepath);
        defer ls.deinit();
        const dfs = try get_defs(allr, ls, filepath);
        defer dfs.deinit();
        var ones = std.ArrayList(u32).init(allr);
        for (dfs.items) |itemp| {
            const trimmed = std.mem.trim(u8, itemp, &std.ascii.spaces);
            var spiter = std.mem.split(u8, trimmed, " && ");
            var one = try allr.dupe(u8, alldashesterm);
            defer allr.free(one);
            var found = false;
            var termscount: usize = 0;
            while (spiter.next()) |x| : (termscount += 1) {
                for (variables) |v, vidx| {
                    if (std.mem.indexOf(u8, x, v) != null) {
                        found = true;
                        const not = std.mem.indexOfScalar(u8, x, '!') != null;
                        // because term elements are packed 2 per byte, this checks and
                        // sets the appropriate nibble - a when vidx is even or b when odd
                        const byteidx = vidx / 2;
                        const nibs = Qm32.toNibblesPtr(&one[byteidx]);
                        const isfirst_nibble = @truncate(u1, vidx) == 0;
                        const value = @boolToInt(!not);
                        if (isfirst_nibble) nibs.a = value else nibs.b = value;
                        break;
                    }
                }
            }

            if (found) {
                perms.clearRetainingCapacity();
                try Qm32.collectPerms(allr, one, &perms, .{}, bitcount);
                for (perms.keys()) |k| try ones.append(try Qm32.termToT(k, bitcount));
            }
        }
        // std.debug.print("ones len {} dc len {} \n", .{ ones.items.len, dontcares.items.len });
        qm32.ones = ones.items;
        qm32.dontcares = dontcares.items;
        try qm32.reduce();
        // std.debug.print("reduced   {}\n", .{Qm32.TermSetFmt.init(qm32.reduced_implicants, ", ", bitcount)});

        const stdout = std.io.getStdOut().writer();
        _ = try stdout.write("#if ");
        for (qm32.reduced_implicants.keys()) |imp, i| {
            if (i != 0) _ = try stdout.write(" || ");
            try stdout.print("{}", .{Qm32.TermFmtVars.init(imp, qm32.bitcount, def_variables, " && ", "!", .before)});
        }
        const newline = if (@import("builtin").os.tag == .windows) "\r\n" else "\n";
        _ = try stdout.write(newline);
        _ = try stdout.write(run.df);
        _ = try stdout.write(newline ++ "#endif" ++ newline);
    }
}
