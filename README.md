:warning: **Work in progress.  Expect bugs and/or missing features** :warning:

# quine-mccluskey.zig
Quine–McCluskey algorithm in zig: useful for minimizing boolean functions

# usage
```
zig build test
```

# tests
see [src/tests.zig](src/tests.zig)

# about
- [Wikipedia article](https://en.wikipedia.org/wiki/Quine%E2%80%93McCluskey_algorithm)
- adapted from https://github.com/vj-ug/Quine-McCluskey-algorithm/blob/master/quinecplusplus.cpp
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