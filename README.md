:warning: **Work in progress.  Expect bugs and/or missing features** :warning:

# quine-mccluskey.zig
Quine–McCluskey algorithm in zig: useful for minimizing boolean functions

# usage

```zig
    const allocator = ...;
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
```
### run tests
```
zig build test
```

# tests
see [src/tests.zig](src/tests.zig)

# about
- [Wikipedia article](https://en.wikipedia.org/wiki/Quine%E2%80%93McCluskey_algorithm)
- adapted from https://github.com/tpircher/quine-mccluskey/
- https://www.tutorialspoint.com/digital_circuits/digital_circuits_quine_mccluskey_tabular_method.htm

# ideas / references
- https://publications.waset.org/4728/pdf
- https://www.ijcaonline.org/archives/volume177/number39/gidde-2020-ijca-919845.pdf
- http://eprints.lqdtu.edu.vn/id/eprint/10375/1/main_camera_ready.pdf
- https://en.wikipedia.org/wiki/Canonical_normal_form
- Petrick's method
  - https://en.wikipedia.org/wiki/Petrick's_method
  - https://www.allaboutcircuits.com/technical-articles/prime-implicant-simplification-using-petricks-method/
  - https://uweb.engr.arizona.edu/~ece474a/uploads/Main/lecture_logicopt.pdf
  - https://github.com/mohdomama/Quine-McCluskey
  - https://github.com/mirkat1206/Quine-McCluskey-Petrick-Method

# todo
- [ ] add support for xor/xnor reductions
- [ ] improve memory usage
  - Fri Aug 19 [7cb4c](https://github.com/travisstaloch/quine-mccluskey.zig/commits/7cb4c99fa5da85149c7a4e0193b6815c88ab44bd) <br>
    count 15 fill_factor 1.0e-01 k 3.2768e+04 n 3277 <br>
    t1 11.111s reduced terms from 3229   to 2716 
  - Thu Sep 1 [eb541](https://github.com/travisstaloch/quine-mccluskey.zig/commits/eb5415d9ed3055096f24050ec3cfa6b71619e5b1) <br>
    count 15 fill_factor 1.0e-01 k 3.2768e+04 n 3277 <br>
    t1 6.408s reduced terms from 3247   to 2739  