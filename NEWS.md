# vctrs 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.

* Character indexing is now only supported for named objects, an error is raised for unnamed objects (#171).

* `vec_c()` and `vec_rbind()` now handle data.frame columns properly (@yutannihilation, #182).

* `vec_arith()` is consistent with base R when combining `difftime` and `date`, with a warning if casts are lossy (#192).
