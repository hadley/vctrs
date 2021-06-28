
test_that("constructor sets attributes", {
  x <- new_vctr(1:4, class = "x", x = 1)
  expect_equal(x, structure(1:4, class = c("x", "vctrs_vctr"), x = 1))

  x <- new_vctr(1:4, class = "x", x = 1, inherit_base_type = TRUE)
  expect_equal(x, structure(1:4, class = c("x", "vctrs_vctr", "integer"), x = 1))
})

test_that(".data must be a vector", {
  expect_error(new_vctr(mean), "vector type")
})

test_that("attributes other than names are ignored", {
  out <- new_vctr(structure(1, a = 1))
  expect_null(attributes(out)$a)
})

test_that("default format method is internal", {
  x <- new_vctr(1, class = "x")
  expect_equal(format(x), format(x))
})

test_that("vctr class is proxied", {
  expect_identical(vec_proxy(new_vctr(1:3)), new_vctr(1:3))
  expect_identical(vec_proxy(new_vctr(as.list(1:3))), unclass(new_vctr(as.list(1:3))))
  expect_true(vec_is(new_vctr(as.list(1:3))))
})

test_that("Can opt out of base type", {
  x <- new_vctr(1, class = "x", inherit_base_type = FALSE)
  expect_s3_class(x, c("x", "vctrs_vctr"), exact = TRUE)
})

test_that("base type is always set for lists", {
  expect_s3_class(new_vctr(list()), "list")
})

test_that("cannot opt out of the base type with lists", {
  expect_error(new_vctr(list(), inherit_base_type = FALSE), "must inherit from the base type")
})

test_that("data frames are not allowed", {
  expect_error(new_vctr(mtcars), "can't be a data frame")
})

test_that("attributes must be named", {
  expect_error(vec_set_attributes(1, list(1)), "must be named")
  expect_error(vec_set_attributes(1, list(y = 1, 2)), "2 does not")
})

test_that("can strip all attributes without adding new ones", {
  expect_equal(vec_set_attributes(structure(1, a = 1), NULL), 1)
})

test_that("`c.vctrs_vctr()` checks for default method arguments (#791)", {
  expect_error(c(new_vctr(1), recursive = TRUE), "`recursive` must be")
  expect_error(c(new_vctr(1), use.names = FALSE), "`use.names` must be")
})


# Cast/restore ------------------------------------------------------------

test_that("cast to NULL returns x", {
  x <- new_vctr(1, class = "x")
  expect_equal(vec_cast(NULL, x), NULL)
})

test_that("cast succeeds if attributes equal", {
  x1 <- new_vctr(1, class = "x", a = 1, b = 2)
  x2 <- new_vctr(2, class = "x", a = 1, b = 2)

  expect_equal(vec_cast(x1, x2), x1)
  expect_equal(vec_cast(x2, x1), x2)
})

test_that("and fails if attributes are different", {
  x1 <- new_vctr(1, class = "x", a = 1, b = 2)
  x2 <- new_vctr(2, class = "x", a = 2, b = 2)

  expect_error(vec_cast(x1, x2), class = "vctrs_error_incompatible_type")
})

test_that("restoring to atomic vector of same type preserves attributes", {
  x1 <- new_vctr(1, class = "x")
  x2 <- new_vctr(2, class = "x")

  expect_equal(vec_restore(2, x1), x2)
})

test_that("restoring to atomic vector of different type throws error", {
  x1 <- new_vctr(1, class = "x")

  expect_error(vec_restore("x", x1), class = "vctrs_error_incompatible_type")
})

test_that("base coercion methods mapped to vec_cast", {
  x <- new_vctr(1, inherit_base_type = FALSE)

  expect_error(as.logical(x), class = "vctrs_error_incompatible_type")
  expect_error(as.integer(x), class = "vctrs_error_incompatible_type")
  expect_error(as.logical(x), class = "vctrs_error_incompatible_type")
  expect_error(as.double(x), class = "vctrs_error_incompatible_type")
  expect_error(as.character(x), class = "vctrs_error_incompatible_type")
  expect_error(as.Date(x), class = "vctrs_error_incompatible_type")
  expect_error(as.POSIXct(x), class = "vctrs_error_incompatible_type")
  expect_error(as.POSIXlt(x), class = "vctrs_error_incompatible_type")

  expect_equal(as.list(x), list(x))
})

test_that("as.data.frame creates data frame", {
  x <- new_vctr(1:3)
  df <- as.data.frame(x)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)
  expect_named(df, "x")
})

test_that("as.data.frame on shaped vctrs doesn't bring along extra attributes", {
  x <- new_vctr(1:3, foo = "bar", dim = c(3L, 1L))
  df <- as.data.frame(x)
  expect_null(attr(df, "foo", exact = TRUE))
})

test_that("as.data.frame2() unclasses input to avoid dispatch on as.data.frame()", {
  x <- structure(1:2, dim = c(1L, 2L), dimnames = list("r1", c("c1", "c2")), class = "foo")
  expect <- data.frame(c1 = 1L, c2 = 2L, row.names = "r1")

  local_methods(as.data.frame.foo = function(x, ...) "dispatched!")

  expect_identical(as.data.frame2(x), expect)
})

test_that("as.list() chops vectors", {
  expect_identical(
    as.list(new_vctr(1:3)),
    list(new_vctr(1L), new_vctr(2L), new_vctr(3L))
  )

  x <- new_vctr(as.list(1:3))
  expect_identical(as.list(x), as.list(1:3))
})


# equality + comparison + arith + math ---------------------------------------

test_that("equality functions remapped", {
  x <- new_vctr(c(1, 1, NA), inherit_base_type = FALSE)

  expect_error(x == 1, class = "vctrs_error_incompatible_type")
  expect_error(x != 1, class = "vctrs_error_incompatible_type")
  expect_equal(is.na(x), c(FALSE, FALSE, TRUE))
  expect_true(anyNA(x))

  expect_equal(unique(x), new_vctr(c(1, NA), inherit_base_type = FALSE))
  expect_equal(duplicated(x), c(FALSE, TRUE, FALSE))
  expect_true(anyDuplicated(x))
})

test_that("is.na<-() supported", {
  x <- new_vctr(1:4)
  is.na(x) <- c(FALSE, FALSE, TRUE, NA)
  expect_identical(x, new_vctr(c(1:2, NA, 4L)))

  x <- new_vctr(1:4)
  is.na(x) <- TRUE
  expect_identical(x, new_vctr(rep(NA_integer_, 4)))

  x <- new_vctr(1:4)
  is.na(x) <- c(2, 3)
  expect_identical(x, new_vctr(c(1L, NA, NA, 4L)))

  names <- c("a", "b", "c", "d")
  x <- set_names(new_vctr(1:4), names)
  is.na(x) <- c("d", "b", "b")
  expect_identical(x, set_names(new_vctr(c(1L, NA, 3L, NA)), names))

  x <- new_vctr(1:4)
  expect_error(is.na(x) <- "x", "character names to index an unnamed vector")
  expect_error(is.na(x) <- 5, class = "vctrs_error_subscript_oob")
})

test_that("comparison functions remapped", {
  local_methods(
    vec_proxy_compare.bizzaro = function(x, ...) -vec_data(x)
  )

  x1 <- new_vctr(c(1, 2), class = "bizzaro")
  x2 <- new_vctr(2, class = "bizzaro")

  expect_equal(order(x1), c(2L, 1L))
  expect_equal(x1 < x2, c(FALSE, FALSE))
  expect_equal(x1 <= x2, c(FALSE, TRUE))
  expect_equal(x1 > x2, c(TRUE, FALSE))
  expect_equal(x1 >= x2, c(TRUE, TRUE))
})

test_that("operators remapped", {
  local_methods(
    vec_arith.bizzaro = function(op, x, y) 1L
  )
  x <- new_vctr(c(1, 2), class = "bizzaro")

  expect_equal(x + 1, 1L)
  expect_equal(x - 1, 1L)
  expect_equal(x * 1, 1L)
  expect_equal(x / 1, 1L)
  expect_equal(x ^ 1, 1L)
  expect_equal(x %% 1, 1L)
  expect_equal(x %/% 1, 1L)
  expect_equal(x & 1, 1L)
  expect_equal(x | 1, 1L)

  expect_equal(!x, 1L)
  expect_equal(+x, 1L)
  expect_equal(-x, 1L)
})

test_that("math functions overridden", {
  local_methods(
    vec_math.bizzaro = function(fn, x, ...) vec_math_base(fn, 2L)
  )
  x <- new_vctr(c(1, NA), class = "bizzaro")

  expect_equal(mean(x), 2L)
  expect_equal(sum(x), 2L)

  expect_equal(is.finite(x), TRUE)
  expect_equal(is.infinite(x), FALSE)
  expect_equal(is.nan(x), FALSE)
})

test_that("diff matches base R", {
  local_methods(
    vec_arith.vctrs_minus = function(op, x, y) vec_arith_base(op, x, y)
  )
  x1 <- cumsum(cumsum(1:10))
  x2 <- new_vctr(x1, class = "vctrs_minus")

  expect_equal(diff(x2), diff(x1))
  expect_equal(diff(x2, lag = 2L), diff(x1, lag = 2L))
  expect_equal(diff(x2, differences = 2L), diff(x1, differences = 2L))

  expect_equal(diff(x2, lag = 11), x2[0L])
  expect_equal(diff(x2, differences = 11), x2[0L])
})

# names -------------------------------------------------------------------

test_that("all elements must be named if any are named", {
  expect_error(new_vctr(setNames(1:2, c("a", NA))), "must not be `NA`")
})

test_that("the empty string is an allowed name", {
  expect_named(new_vctr(set_names(1, "")), "")
  expect_named(new_vctr(set_names(1:2, c("", "x"))), c("", "x"))
})

test_that("can not provide invalid names", {
  x <- new_vctr(c(a = 1, b = 2))
  expect_error(names(x) <- "x", "length")
  expect_error(names(x) <- c("x", NA), "must not be `NA`")
  expect_error(names(x) <- c("x", "y", "z"), "length")
  expect_error(names(x) <- NULL, NA)
})

test_that("can set names to the empty string", {
  x <- new_vctr(c(a = 1, b = 2))

  names(x) <- c("", "")
  expect_named(x, c("", ""))

  names(x) <- c("", "x")
  expect_named(x, c("", "x"))
})

test_that("can use [ and [[ with names", {
  local_methods(
    vec_ptype2.vctrs_vctr.double = function(...) dbl(),
    vec_ptype2.double.vctrs_vctr = function(...) dbl()
  )
  x <- new_vctr(c(a = 1, b = 2))

  expect_equal(x["b"], new_vctr(c(b = 2)))
  expect_equal(x[["b"]], new_vctr(2)) # [[ drops names

  x[["c"]] <- 3
  expect_equal(x[["c"]], new_vctr(3))
  x["d"] <- 4
  expect_equal(x[["d"]], new_vctr(4))
})

test_that("can use [ and [[ with names - list vctr", {
  local_methods(
    vec_ptype2.vctrs_vctr = function(...) list(),
    vec_ptype2.list.vctrs_vctr = function(...) list()
  )
  y <- new_vctr(list(a = 1, b = 2))
  y[["c"]] <- 3
  expect_equal(y[["c"]], 3)
  y["d"] <- list(4)
  expect_equal(y[["d"]], 4)
})

test_that("can use [[<- to replace n-dimensional elements", {
  local_methods(
    vec_restore.vctrs_mtrx = function(x, to, ...) x,
    vec_ptype2.double.vctrs_mtrx = function(...) dbl(),
    vec_ptype2.vctrs_mtrx = function(...) dbl()
  )
  x <- new_vctr(rep(1, times = 4), dim = c(2, 2), class = "vctrs_mtrx")
  x[[2, 2]] <- 4
  expect_equal(x[[2, 2]], 4)
})

test_that("subsetting preserves attributes", {
  x <- new_vctr(c(a = 1, b = 2))
  attr(x, "extra") <- TRUE

  y <- x[1]
  expect_equal(attr(x, "extra"), TRUE)
})

test_that("$ inherits from underlying vector", {
  x1 <- new_vctr(c(a = 1, b = 2))
  expect_error(x1$a, "atomic vectors")
  expect_error(x1$a <- 2, "atomic vectors")

  x2 <- new_vctr(list(a = 1, b = 2))
  expect_equal(x2$a, 1)
  x2$a <- 10
  expect_equal(x2$a, 10)
})


# unsupported/unimplemented operations --------------------------------------

test_that("can't touch protected attributes", {
  x <- new_vctr(1:4)

  expect_error(dim(x) <- c(2, 2), class = "vctrs_error_unsupported")
  expect_error(dimnames(x) <- list("x"), class = "vctrs_error_unsupported")

  expect_error(levels(x), class = "vctrs_error_unsupported")
  expect_error(levels(x) <- "x", class = "vctrs_error_unsupported")

  # but it's ok to set names to NULL; this happens at least in vec_c
  # and maybe elsewhere. We may need to back off on this level of
  # strictness in the future
  expect_error(names(x) <- NULL, NA)
})

test_that("summary is unimplemented", {
  x <- new_vctr(1:4)
  expect_error(summary(x), class = "vctrs_error_unimplemented")
})

# hidden class ------------------------------------------------------------
# We can't construct classes in test because the methods are not found
# when vctr generics call other generics. Instead we rely on a very simple
# class implemented in vctr.R

test_that("class preserved when subsetting", {
  h <- new_hidden(1:4)

  expect_s3_class(h, "hidden")
  expect_s3_class(h[1], "hidden")
  expect_s3_class(h[[1]], "hidden")
  expect_s3_class(rep(h[1], 2), "hidden")
  expect_s3_class(as.list(h)[[1]], "hidden")

  length(h) <- 3
  expect_s3_class(h, "hidden")
})

test_that("RHS cast when using subset assign", {
  local_hidden()
  h <- new_hidden(1)

  expect_error(h[[1]] <- "x", class = "vctrs_error_incompatible_type")
  expect_error(h[1] <- "x", class = "vctrs_error_incompatible_type")

  h[2] <- 1
  expect_equal(h, new_hidden(c(1, 1)))

  h[[2]] <- 2
  expect_equal(h, new_hidden(c(1, 2)))
})

test_that("c passes on to vec_c", {
  local_hidden()

  h <- new_hidden(1)

  expect_equal(c(h), h)
  expect_equal(c(h, NULL), h)

  expect_equal(c(h, 1), rep(h, 2))
  expect_equal(c(h, h), rep(h, 2))
})

test_that("summaries preserve class", {
  h <- new_hidden(c(1, 2))

  expect_equal(sum(h), new_hidden(3))
  expect_equal(mean(h), new_hidden(1.5))
})

test_that("methods using vec_proxy_compare agree with base", {
  h <- new_hidden(c(1:10))
  h_na <- new_hidden(c(NA, 1:10))

  expect_agree <- function(f, x, na.rm = FALSE) {
    f <- enexpr(f)
    expect_equal(vec_data((!!f)(x, na.rm = na.rm)), (!!f)(vec_data(x), na.rm = na.rm))
  }

  expect_agree(min, h)
  expect_agree(max, h)
  expect_agree(range, h)

  expect_agree(min, h_na)
  expect_agree(max, h_na)
  expect_agree(range, h_na)

  expect_agree(min, h_na, na.rm = TRUE)
  expect_agree(max, h_na, na.rm = TRUE)
  expect_agree(range, h_na, na.rm = TRUE)
})

test_that("can put in data frame", {
  h <- new_hidden(1:4)

  expect_named(as.data.frame(h), "h")
  expect_named(data.frame(x = h), "x")
})

test_that("base coercions default to vec_cast", {
  local_hidden()
  h <- new_hidden(1)
  expect_error(as.character(h), class = "vctrs_error_incompatible_type")
  expect_error(as.integer(h), class = "vctrs_error_incompatible_type")
  expect_error(generics::as.factor(h), class = "vctrs_error_incompatible_type")
  expect_error(generics::as.ordered(h), class = "vctrs_error_incompatible_type")
  expect_error(generics::as.difftime(h), class = "vctrs_error_incompatible_type")
  expect_equal(as.logical(h), TRUE)
  expect_equal(as.double(h), 1)
})

test_that("default print and str methods are useful", {
  h <- new_hidden(1:4)

  expect_known_output(
    {
      print(h)
      cat("\n")
      print(h[0])
      cat("\n")
      str(h)
    },
    file = "test-vctr-print.txt",
  )
})

test_that("default print method shows names", {
  h <- new_hidden(c(A = 1, B = 2, C = 3))

  expect_known_output(
    {
      print(h)
    },
    file = "test-vctr-print-names.txt",
  )
})

test_that("can't transpose", {
  h <- new_hidden(1:4)
  expect_error(t(h), class = "vctrs_error_unsupported")
})

test_that("shaped vctrs can be cast to data frames", {
  x <- new_vctr(1:4, dim = 4)
  expect_identical(as.data.frame(x), data.frame(V1 = 1:4))

  x <- new_vctr(1:4, dim = c(2, 2))
  expect_identical(as.data.frame(x), data.frame(V1 = 1:2, V2 = 3:4))
})


# slicing -----------------------------------------------------------------

test_that("additional subscripts are handled (#269)", {
  new_2d <- function(.data, dim) {
    vctrs::new_vctr(.data, dim = dim, class = "vctrs_2d")
  }
  x <- new_2d(c(1, 2), dim = c(2L, 1L))

  expect_identical(x[1], new_2d(1, dim = c(1, 1)))
  expect_identical(x[1, 1], new_2d(1, dim = c(1, 1)))
  expect_identical(x[, 1], new_2d(c(1, 2), dim = c(2, 1)))
})


# summary generics --------------------------------------------------------

test_that("na.rm is forwarded to summary generics", {
  x <- new_vctr(dbl(1, 2, NA))

  expect_identical(mean(x, na.rm = FALSE), new_vctr(dbl(NA)))
  expect_identical(mean(x, na.rm = TRUE), new_vctr(1.5))

  expect_identical(min(x, na.rm = FALSE), new_vctr(dbl(NA)))
  expect_identical(min(x, na.rm = TRUE), new_vctr(1))

  expect_identical(max(x, na.rm = FALSE), new_vctr(dbl(NA)))
  expect_identical(max(x, na.rm = TRUE), new_vctr(2))

  x <- new_vctr(lgl(TRUE, NA))

  expect_identical(all(x, na.rm = FALSE), lgl(NA))
  expect_identical(all(x, na.rm = TRUE), TRUE)
})

test_that("Summary generics behave identically to base for empty vctrs (#88)", {
  expect_warning(
    expect_identical(
      new_vctr(max(numeric())),
      max(new_vctr(numeric()))
    )
  )

  expect_warning(
    expect_identical(
      new_vctr(min(numeric())),
      min(new_vctr(numeric()))
    )
  )

  expect_identical(
    new_vctr(range(1)),
    range(new_vctr(1))
  )

  expect_identical(
    new_vctr(prod(numeric())),
    prod(new_vctr(numeric()))
  )

  expect_identical(
    new_vctr(sum(numeric())),
    sum(new_vctr(numeric()))
  )

  expect_identical(
    new_vctr(cummax(numeric())),
    cummax(new_vctr(numeric()))
  )

  expect_identical(
    new_vctr(cummin(numeric())),
    cummin(new_vctr(numeric()))
  )

  expect_identical(
    new_vctr(cumprod(numeric())),
    cumprod(new_vctr(numeric()))
  )

  expect_identical(
    new_vctr(cumsum(numeric())),
    cumsum(new_vctr(numeric()))
  )

  expect_identical(
    new_vctr(mean(numeric())),
    mean(new_vctr(numeric()))
  )
})

test_that("generic predicates return logical vectors (#251)", {
  x <- new_vctr(c(1, 2))
  expect_identical(is.finite(x), c(TRUE, TRUE))
  expect_identical(is.infinite(x), c(FALSE, FALSE))
  expect_identical(is.nan(x), c(FALSE, FALSE))

  x <- new_vctr(TRUE)
  expect_identical(any(x), TRUE)
  expect_identical(all(x), TRUE)
})

test_that("xtfrm() converts logical types to integer", {
  expect_identical(xtfrm(new_vctr(c(TRUE, FALSE, NA), foo = "bar")), c(1L, 0L, NA))
})

test_that("xtfrm() unwraps integer and double atomic types", {
  expect_identical(xtfrm(new_vctr(1:3, foo = "bar")), 1:3)
  expect_identical(xtfrm(new_vctr(1:3 + 0, foo = "bar")), 1:3 + 0)
})

test_that("xtfrm() works with character subclass", {
  expect_identical(xtfrm(new_vctr(chr())), int())
})

test_that("xtfrm() maintains ties when falling through to vec_rank() (#1354)", {
  x <- new_vctr(c("F", "F", "M", "A", "M", "A"))
  expect_identical(xtfrm(x), c(2L, 2L, 3L, 1L, 3L, 1L))
})

test_that("xtfrm() propagates NAs when falling through to vec_rank()", {
  x <- new_vctr(c("F", NA))
  expect_identical(xtfrm(x), c(1L, NA))
})

test_that("xtfrm() uses C locale ordering with character proxies", {
  x <- new_vctr(c("A", "a", "B"))
  expect_identical(xtfrm(x), c(1L, 3L, 2L))
})

test_that("xtfrm() works on rcrd types", {
  x <- new_rcrd(list(x = c(1, 2, 1, NA), y = c(2, 1, 1, NA)))
  expect_identical(xtfrm(x), c(2L, 3L, 1L, NA))
})

test_that("Summary generics behave as expected if na.rm = TRUE and all values are NA (#1357)", {
  expect_identical(min(new_vctr(NA_real_), na.rm = TRUE), new_vctr(Inf))
  expect_identical(max(new_vctr(NA_real_), na.rm = TRUE), new_vctr(-Inf))
  expect_identical(range(new_vctr(NA_real_), na.rm = TRUE), new_vctr(c(Inf, -Inf)))

  expect_identical(min(new_vctr(NA_integer_), na.rm = TRUE), new_vctr(NA_integer_))
  expect_identical(max(new_vctr(NA_integer_), na.rm = TRUE), new_vctr(NA_integer_))
  expect_identical(range(new_vctr(NA_integer_), na.rm = TRUE), new_vctr(c(NA_integer_, NA_integer_)))

  expect_identical(min(new_vctr(NA_character_), na.rm = TRUE), new_vctr(NA_character_))
  expect_identical(max(new_vctr(NA_character_), na.rm = TRUE), new_vctr(NA_character_))
  expect_identical(range(new_vctr(NA_character_), na.rm = TRUE), new_vctr(c(NA_character_, NA_character_)))

  expect_identical(min(new_vctr(NA), na.rm = TRUE), new_vctr(NA))
  expect_identical(max(new_vctr(NA), na.rm = TRUE), new_vctr(NA))
  expect_identical(range(new_vctr(NA), na.rm = TRUE), new_vctr(c(NA, NA)))
})

test_that("Summary generics behave as expected for empty vctrs (#1357)", {
  expect_identical(min(new_vctr(numeric()), na.rm = TRUE), new_vctr(Inf))
  expect_identical(max(new_vctr(numeric()), na.rm = TRUE), new_vctr(-Inf))
  expect_identical(range(new_vctr(numeric()), na.rm = TRUE), new_vctr(c(Inf, -Inf)))

  expect_identical(min(new_vctr(integer()), na.rm = TRUE), new_vctr(NA_integer_))
  expect_identical(max(new_vctr(integer()), na.rm = TRUE), new_vctr(NA_integer_))
  expect_identical(range(new_vctr(integer()), na.rm = TRUE), new_vctr(c(NA_integer_, NA_integer_)))

  expect_identical(min(new_vctr(character()), na.rm = TRUE), new_vctr(NA_character_))
  expect_identical(max(new_vctr(character()), na.rm = TRUE), new_vctr(NA_character_))
  expect_identical(range(new_vctr(character()), na.rm = TRUE), new_vctr(c(NA_character_, NA_character_)))

  expect_identical(min(new_vctr(logical()), na.rm = TRUE), new_vctr(NA))
  expect_identical(max(new_vctr(logical()), na.rm = TRUE), new_vctr(NA))
  expect_identical(range(new_vctr(logical()), na.rm = TRUE), new_vctr(c(NA, NA)))
})
