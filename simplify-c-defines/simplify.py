from qm import QuineMcCluskey
import sys

def get_bit_index(s):

    if "__APPLE__" in s:
        return 0
    elif "__ZIG" in s and "11" in s:
        return 1
    elif "__ZIG" in s and "12" in s:
        return 2
    elif "SYS_APPLEAPIOPTS" in s:
        return 3
    elif "_APPLE_API_STANDARD" in s:
        return 4
    elif "APPLE_API_STABLE" in s:
        return 5
    elif "APPLE_API_STRICT_CON" in s:
        return 6
    elif "APPLE_API_EVOLVING" in s:
        return 7
    elif "APPLE_API_UNSTABLE" in s:
        return 8
    elif "APPLE_API_PRIVATE" in s:
        return 9
    elif "APPLE_API_OBSOLETE" in s:
        return 10


def is_not(s):
    if "!" in s:
        return True
    else:
        return False


def get_string_from_index(index):
    if index == 0:
        return "defined(__APPLE__)"
    if index == 1:
        return "__ZIG_OS_VERSION_MIN_MAJOR__ == 11"
    if index == 2:
        return "__ZIG_OS_VERSION_MIN_MAJOR__ == 12"
    if index == 3:
        return "defined(__SYS_APPLEAPIOPTS_H__)"
    if index == 4:
        return "defined(__APPLE_API_STANDARD)"
    if index == 5:
        return "defined(__APPLE_API_STABLE)"
    if index == 6:
        return "defined(__APPLE_API_STRICT_CONFORMANCE)"
    if index == 7:
        return "defined(__APPLE_API_EVOLVING)"
    if index == 8:
        return "defined(__APPLE_API_UNSTABLE)"
    if index == 9:
        return "defined(__APPLE_API_PRIVATE)"
    if index == 10:
        return "defined(__APPLE_API_OBSOLETE)"

    return "nothing"


def find_lines(s, start, stop, filepath):
    res = []
    with open(filepath) as f:
        lst = f.readlines()
        for i, itemp in enumerate(lst):
            item = itemp.strip()
            if s in item and i >= start and i <= stop:
                res.append(i)
    return res


def get_defs(linelist, filepath):
    res = []
    slist = set(linelist)
    with open(filepath) as f:
        lst = f.readlines()
        for i, itemp in enumerate(lst):
            if (i + 1) in slist:
                item = itemp.strip().lstrip("#if ")
                res.append(item)
    return res


def load_defines(filepath):
    res = []
    with open(filepath) as f:
        lst = f.readlines()
        for i, itemp in enumerate(lst):
            item = itemp.strip()
            if "#define" in item:
                res.append((item, i))
    return res


def find_runs(lst):
    res = []
    i = 0
    while i < len(lst):
        d = lst[i][0]
        index = lst[i][1]
        i = i + 1
        while i < len(lst) and lst[i][0] == d:
            i = i + 1
        res.append((d, index, lst[i - 1][1]))

    return res


def main():
    filepath = sys.argv[1]
    l = load_defines(filepath)
    runs = find_runs(l)
    for df, start, stop in runs[0:1]:
        ls = find_lines(df, start, stop, filepath)
        dfs = get_defs(ls, filepath)
        qm = QuineMcCluskey()
        flist = []
        for itemp in dfs:
            item = itemp.strip().split(" && ")
            init = ["-"] * 11
            for x in item:
                index = get_bit_index(x)
                if is_not(x):
                    init[index] = "0"
                else:
                    init[index] = "1"
            # print(''.join(init))
            for x in qm.permutations("".join(init)):
                flist.append(x)

        # Tell Quine-McCluskey to not worry about what happens when
        # __ZIG_OS_VERSION_MIN_MAJOR__ is both 11 and 12 at the same time, and when
        # __ZIG_OS_VERSION_MIN_MAJOR__ is not 11 and not 12 at the same time.
        dc = []
        for x in qm.permutations("-00--------"):
            dc.append(x)
        for x in qm.permutations("-11--------"):
            dc.append(x)
        # print("ones len ", len(flist), "dc len ", len(dc))
        N = 5
        simple = qm.simplify_los(flist, dc=dc)
        # print(simple)
        for x in simple:
            lres = []
            for i, v in enumerate(x):
                if v != "-":
                    if v == "0":
                        lres.append("!" + get_string_from_index(i))
                    else:
                        lres.append(get_string_from_index(i))
            print("#if " + " && ".join(lres))
            print(df)
            print("#endif")


if __name__ == "__main__":
    main()
