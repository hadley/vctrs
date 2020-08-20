
test_that("vec_map() handles S3 vectors", {
  vctr <- new_vctr(1:3)
  rcrd <- new_rcrd(list(x = 1:3, y = 4:6))

  expect_identical(
    vec_map(vctr, identity),
    vec_chop(vctr)
  )
  expect_identical(
    vec_map(rcrd, identity),
    vec_chop(rcrd)
  )

  expect_identical(
    vec_map(vctr, identity, .ptype = vctr),
    vctr
  )
  expect_identical(
    vec_map(rcrd, identity, .ptype = rcrd),
    rcrd
  )
})

test_that("vec_map() handles S3 lists", {
  local_list_rcrd_methods()
  x <- new_list_rcrd(list(1:2, 3:5, 6:9))

  exp <- list(1:2, 3:4, 6:7)
  expect_identical(
    vec_map(x, `[`, 1:2),
    exp
  )

  expect_identical(
    vec_map(x, `[`, 1:2, .ptype = x),
    new_list_rcrd(exp)
  )
})

test_that("vec_map() requires a non-NULL ptype", {
  expect_error(vec_map(1, identity, .ptype = NULL), "can't be NULL")
})

test_that("functions must return size 1 vector when mapping to atomic", {
  expect_error(
    map(1:2, rep, 2, .ptype = int()),
    "must return a size 1 vector"
  )
})


# Tests imported from purrr -----------------------------------------------

# These tests rely on compat-purrr-vctrs

test_that("preserves names", {
  out <- map(list(x = 1, y = 2), identity)
  expect_equal(names(out), c("x", "y"))
})

test_that("creates simple call", {
  out <- map(1, function(x) sys.call())[[1]]
  expect_equal(out, quote(.fn(.elt, ...)))
})

test_that("fails on non-vectors", {
  expect_error(map(environment(), identity), class = "vctrs_error_scalar_type")
  expect_error(map(quote(a), identity), class = "vctrs_error_scalar_type")
})

test_that("0 length input gives 0 length output", {
  out1 <- map(list(), identity)
  expect_equal(out1, list())

  return("Used to work in purrr")

  out2 <- map(NULL, identity)
  expect_equal(out2, list())
})

test_that("map() always returns a list", {
  expect_is(map(mtcars, mean), "list")
})

test_that("types automatically coerced upwards", {
  expect_identical(map_int(c(FALSE, TRUE), identity), c(0L, 1L))

  expect_identical(map_dbl(c(FALSE, TRUE), identity), c(0, 1))
  expect_identical(map_dbl(c(1L, 2L), identity), c(1, 2))

  return("Used to work in purrr")

  expect_identical(map_int(as.raw(0:1), identity), 0:1)
  expect_identical(map_dbl(as.raw(0:1), identity), c(0, 1))
  expect_identical(map_chr(as.raw(0:255), identity), as.character(as.raw(0:255)))

  expect_identical(map_chr(c(FALSE, TRUE), identity), c("FALSE", "TRUE"))
  expect_identical(map_chr(c(1L, 2L), identity), c("1", "2"))
  expect_identical(map_chr(c(1.5, 2.5), identity), c("1.500000", "2.500000"))
})

test_that("map_raw",{
  expect_equal(map_raw("a", charToRaw), charToRaw("a"))
})

test_that("logical and integer NA become correct double NA", {
  expect_identical(
    map_dbl(list(NA, NA_integer_), identity),
    c(NA_real_, NA_real_)
  )
})

test_that("map forces arguments in same way as base R", {
  f_map <- map(1:2, function(i) function(x) x + i)
  f_base <- lapply(1:2, function(i) function(x) x + i)

  expect_equal(f_map[[1]](0), f_base[[1]](0))
  expect_equal(f_map[[2]](0), f_base[[2]](0))
})

test_that("walk is used for side-effects", {
  expect_output(walk(1:3, str))
})

test_that("map_if() and map_at() always return a list", {
  skip_if_not_installed("tibble")
  df <- tibble::tibble(x = 1, y = "a")
  expect_identical(map_if(df, is.character, ~"out"), list(x = 1, y = "out"))
  expect_identical(map_at(df, 1, ~"out"), list(x = "out", y = "a"))
})

test_that("map_at() works with tidyselect", {
  skip_if_not_installed("tidyselect")
  x <- list(a = "b", b = "c", aa = "bb")
  one <- map_at(x, quos(a), toupper)
  expect_identical(one$a, "B")
  expect_identical(one$aa, "bb")
  two <- map_at(x, quos(tidyselect::contains("a")), toupper)
  expect_identical(two$a, "B")
  expect_identical(two$aa, "BB")
})

test_that("negative .at omits locations", {
  x <- c(1, 2, 3)
  out <- map_at(x, -1, ~ .x * 2)
  expect_equal(out, list(1, 4, 6))
})

test_that("map works with calls and pairlists", {
  expect_true(TRUE)
  return("Used to work in purrr")

  out <- map(quote(f(x)), ~ quote(z))
  expect_equal(out, list(quote(z), quote(z)))

  out <- map(pairlist(1, 2), ~ . + 1)
  expect_equal(out, list(2, 3))
})

test_that("primitive dispatch correctly", {
  local_bindings(.env = global_env(),
    as.character.test_class = function(x) "dispatched!"
  )
  x <- structure(list(), class = "test_class")
  expect_identical(map(list(x, x), as.character), list("dispatched!", "dispatched!"))
})

test_that("map_if requires predicate functions", {
  expect_error(map_if(1:3, ~ NA, ~ "foo"), "must return")
})

test_that("`.else` maps false elements", {
  expect_identical(map_if(-1:1, ~ .x > 0, paste, .else = ~ "bar", "suffix"), list("bar", "bar", "1 suffix"))
})

test_that("map() with empty input copies names", {
  named_list <- named(list())
  expect_identical(    map(named_list, identity), named(list()))
  expect_identical(map_lgl(named_list, identity), named(lgl()))
  expect_identical(map_int(named_list, identity), named(int()))
  expect_identical(map_dbl(named_list, identity), named(dbl()))
  expect_identical(map_chr(named_list, identity), named(chr()))
  expect_identical(map_raw(named_list, identity), named(raw()))
})
