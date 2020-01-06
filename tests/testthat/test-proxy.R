
test_that("vec_data() preserves names (#245)", {
  x <- set_names(letters, LETTERS)
  expect_identical(vec_names(x), vec_names(vec_data(x)))

  x <- diag(2)
  rownames(x) <- letters[1:2]
  colnames(x) <- LETTERS[1:2]
  expect_identical(vec_names(x), vec_names(vec_data(x)))
})

test_that("vec_data() preserves size (#245)", {
  x <- set_names(letters, LETTERS)
  expect_identical(vec_size(x), vec_size(vec_data(x)))

  x <- diag(2)
  expect_identical(vec_size(x), vec_size(vec_data(x)))
})

test_that("vec_data() preserves dim and dimnames (#245)", {
  x <- set_names(letters, LETTERS)
  expect_identical(vec_dim(x), vec_dim(vec_data(x)))

  x <- diag(2)
  expect_identical(vec_dim(x), vec_dim(vec_data(x)))

  x <- diag(2)
  rownames(x) <- letters[1:2]
  colnames(x) <- LETTERS[1:2]
  expect_identical(dimnames(x), dimnames(vec_data(x)))
})

test_that("strips vector attributes apart from names, dim and dimnames", {
  x <- new_vctr(1:10, a = 1, b = 2)
  expect_null(attributes(vec_data(x)))

  x <- new_vctr(c(x = 1, y = 2), a = 1, b = 2)
  expect_equal(names(attributes(vec_data(x))), "names")

  x <- new_vctr(1, a = 1, dim = c(1L, 1L))
  expect_equal(names(attributes(vec_data(x))), "dim")

  x <- new_vctr(1, a = 1, dim = c(1L, 1L), dimnames = list("foo", "bar"))
  expect_equal(names(attributes(vec_data(x))), c("dim", "dimnames"))
})

test_that("vec_proxy() is a no-op with data vectors", {
  for (x in vectors) {
    expect_identical(vec_proxy(!!x), !!x)
  }

  x <- structure(1:3, foo = "bar")
  expect_identical(vec_proxy(!!x), !!x)
})

test_that("vec_proxy() transforms records to data frames", {
  for (x in records) {
    expect_identical(vec_proxy(x), new_data_frame(unclass(x)))
  }
})

test_that("vec_proxy() is a no-op with non vectors", {
  x <- foobar(list())
  expect_identical(vec_proxy(x), x)
})

test_that("can take the proxy of non-vector objects", {
  local_env_proxy()
  expect_identical(vec_proxy(new_proxy(1:3)), 1:3)
})

test_that("vec_data() asserts vectorness", {
  expect_error(vec_data(new_sclr()), class = "vctrs_error_scalar_type")
  expect_error(vec_data(~foo), class = "vctrs_error_scalar_type")
})

test_that("vec_data() is proxied", {
  local_env_proxy()
  x <- new_proxy(mtcars)
  expect_identical(vec_data(x), vec_data(mtcars))
})

test_that("vec_proxy_equal() is recursive over data frames (#641)", {
  x <- new_data_frame(list(x = foobar(1:3)))
  default <- vec_proxy_equal(x)
  expect_is(default$x, "vctrs_foobar")

  local_methods(vec_proxy_equal.vctrs_foobar = function(...) c(0, 0, 0))
  overridden <- vec_proxy_equal(x)
  expect_identical(overridden$x, c(0, 0, 0))
})

test_that("vec_proxy_push_vcols() pushes virtual columns", {
  proxy <- data_frame(x = 1:3)

  out <- vec_proxy_push_vcols(proxy, `pkg::foo` = data_frame(bar = 11:13))
  exp <- data_frame(
    x = 1:3,
    `vctrs::virtual_cols` = data_frame(
      `pkg::foo` = data_frame(bar = 11:13)
    )
  )
  expect_identical(out, exp)

  out <- vec_proxy_push_vcols(out, `pkg::baz` = data_frame(bam = 21:23))
  exp <- data_frame(
    x = 1:3,
    `vctrs::virtual_cols` = data_frame(
      `pkg::foo` = data_frame(bar = 11:13),
      `pkg::baz` = data_frame(bam = 21:23)
    )
  )
  expect_identical(out, exp)
})

test_that("can push virtual columns on empty data frame", {
  proxy <- data_frame(n = 3L)
  out <- vec_proxy_push_vcols(proxy, `pkg::foo` = 1:3)
  exp <- data_frame(`vctrs::virtual_cols` = data_frame(`pkg::foo` = 1:3))
  expect_identical(out, exp)
})

test_that("vec_proxy_pop_vcols() pops virtual columns", {
  vcols <- data_frame(
    foo = 11:13,
    bar = 21:23
  )
  proxy <- data_frame(
    x = 1:3,
    `vctrs::virtual_cols` = vcols
  )

  out <- vec_proxy_pop_vcols(proxy, "bar")
  expect_identical(out$proxy$`vctrs::virtual_cols`, data_frame(foo = 11:13))
  expect_identical(out$vcols, data_frame(bar = 21:23))

  out <- vec_proxy_pop_vcols(out$proxy, "foo")
  expect_identical(out$proxy, data_frame(x = 1:3))
  expect_identical(out$vcols, data_frame(foo = 11:13))
})

test_that("vec_proxy_pop_vcols() pops multiple virtual columns", {
  vcols <- data_frame(
    foo = 11:13,
    bar = 21:23,
    foo = 31:33,
    bar = 41:43
  )
  proxy <- data_frame(
    x = 1:3,
    `vctrs::virtual_cols` = vcols
  )

  out <- vec_proxy_pop_vcols(proxy, "foo")
  expect_identical(out$proxy$`vctrs::virtual_cols`, data_frame(bar = 21:23, bar = 41:43))
  expect_identical(out$vcols, data_frame(foo = 11:13, foo = 31:33))

  out <- vec_proxy_pop_vcols(out$proxy, "bar")
  expect_identical(out$proxy, data_frame(x = 1:3))
  expect_identical(out$vcols, data_frame(bar = 21:23, bar = 41:43))
})

test_that("can proxy and restore virtual columns", {
  local_vcols_methods()
  x <- new_vcols(data_frame(x = 1:3), groups = c(1L, 1L, 2L))
  expect_identical(vec_restore(vec_proxy(x), to = x), x)
})
