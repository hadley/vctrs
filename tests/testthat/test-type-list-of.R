
test_that("list_of inherits from list", {
  x1 <- list_of(1, 1)
  expect_s3_class(x1, "list")
})

test_that("list_of works like list", {
  x1 <- list_of(1, 1)
  expect_type(x1, "list")
  expect_s3_class(x1, "vctrs_list_of")
  expect_equal(attr(x1, "ptype"), double())

  x2 <- list_of(1, 1, .ptype = integer())
  expect_equal(attr(x1, "ptype"), integer())

  x3 <- as_list_of(list(1, 1))
  expect_equal(x3, x1)

  x4 <- list_of(a = 1, b = 2)
  expect_equal(x4$b, 2)
  expect_error(x4$c, "Invalid index", fixed = TRUE)
})

test_that("list_of errors if it can't find common type", {
  expect_error(list_of(1, "a"), class = "vctrs_error_incompatible_type")
  expect_error(list_of(), "find common type")
})

test_that("can use as_list_of to change type", {
  x1 <- list_of(1)
  expect_equal(as_list_of(x1), x1)

  x2 <- as_list_of(x1, .ptype = integer())
  expect_identical(x2[[1]], 1L)
})

test_that("is_list_of as expected", {
  expect_false(is_list_of(list(1)))
  expect_true(is_list_of(list_of(1)))
})

test_that("print method gives human friendly output", {
  skip_on_cran() # Depends on tibble

  x <- list_of(1, 2:3)

  expect_known_output({
      print(x)
      cat("\n")
      print(tibble::tibble(x))
    },
    file = test_path("test-list_of-print.txt")
  )
})

test_that("str method is reasonably correct", {
  x <- list_of(1, 2:3)

  expect_known_output({
      str(x)
      cat("\n")
      str(list(list(x, y = 2:1)))
    },
    file = test_path("test-list_of-str.txt")
  )

  expect_known_output({
      str(x[0])
      cat("\n")
      str(list(list(x, y = 2:1)))
    },
    file = test_path("test-list_of-str-empty.txt")
  )
})

test_that("constructor requires list input", {
  expect_error(new_list_of(1), "must be a list")
  expect_error(new_list_of(mtcars), "must be a list")
})

test_that("constructor requires size 0 ptype", {
  expect_error(new_list_of(ptype = 1), "must have size 0")
})

# Subsetting --------------------------------------------------------------

test_that("[ preserves type", {
  x <- list_of(1)
  expect_equal(x[1], x)
})

test_that("[<-, [[<- and $<- coerce their input", {
  x <- list_of(x = 1, y = 1, z = 1, w = 1)

  x[1] <- list(FALSE)
  expect_identical(x, list_of(x = 0, y = 1, z = 1, w = 1))

  x[[2]] <- FALSE
  expect_identical(x, list_of(x = 0, y = 0, z = 1, w = 1))

  x$z <- FALSE
  expect_identical(x, list_of(x = 0, y = 0, z = 0, w = 1))

  x[3:4] <- c(TRUE, FALSE)
  expect_identical(x, list_of(x = 0, y = 0, z = 1, w = 0))

  x[[2]] <- NULL
  expect_equal(x, list_of(x = 0, y = NULL, z = 1, w = 0))

  expect_error(x[[2]] <- list(20), class = "vctrs_error_incompatible_type")
  expect_error(x$y <- list(20), class = "vctrs_error_incompatible_type")

  x[3:4] <- list(NULL)
  expect_equal(x, list_of(x = 0, y = NULL, z = NULL, w = NULL))

  expect_equal(is.na(x), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("assingment can increase size of vector", {
  x <- list_of(x = 1)
  x[[2]] <- 2
  x$z <- 3
  x[4:5] <- c(4,5)

  expect_length(x, 5)
})


# Type system -------------------------------------------------------------

test_that("list_of() are vectors", {
  expect_true(vec_is_vector(list_of(1)))
  expect_true(vec_is(list_of(1)))
})

test_that("list coercions are symmetric and unchanging", {
  types <- list(
    list(),
    list_of(.ptype = integer()),
    list_of(.ptype = double()),
    list_of(.ptype = character())
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(
    mat,
    test_path("test-list_of-type.txt"),
    print = TRUE,
    width = 200
  )
})

test_that("max<list_of<a>, list_of<b>> is list_of<max<a, b>>", {
  r_int <- list_of(.ptype = integer())
  r_dbl <- list_of(.ptype = double())

  expect_equal(vec_ptype_common(r_int, r_int), r_int)
  expect_equal(vec_ptype_common(r_int, r_dbl), r_int)
})

test_that("safe casts work as expected", {
  x <- list_of(1)
  expect_identical(vec_cast(NULL, x), NULL)
  expect_identical(vec_cast(NA, x), list_of(NULL, .ptype = double()))
  expect_identical(vec_cast(list(1), x), list_of(1))
  expect_identical(vec_cast(list(TRUE), x), list_of(1))
  expect_identical(vec_cast(x, list()), list(1))

  # These used to be allowed
  expect_error(vec_cast(1L, x), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(1, x), class = "vctrs_error_incompatible_type")
})

test_that("lossy casts generate warning (no longer the case)", {
  expect_error(
    vec_cast(list(c(1.5, 1), 1L), to = list_of(1L)),
    class = "vctrs_error_cast_lossy"
  )
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), list_of(1)), class = "vctrs_error_incompatible_type")
})

test_that("validation", {
  expect_error(validate_list_of(list_of(1, 2, 3)), NA)
  expect_error(
    validate_list_of(new_list_of(list(factor("foo")), vec_ptype(factor("bar")))),
    class = "vctrs_error_cast_lossy"
  )
})

test_that("list_of() has as.character() method (tidyverse/tidyr#654)", {
  exp <- rep(paste0("<", vec_ptype_abbr(mtcars), ">"), 2)
  expect_identical(as.character(list_of(mtcars, mtcars)), exp)
})

test_that("vec_ptype2(<list_of<>>, NA) is symmetric (#687)", {
  lof <- list_of(1, 2, 3)
  expect_identical(vec_ptype2(lof, NA), vec_ptype(lof))
  expect_identical(vec_ptype2(NA, lof), vec_ptype(lof))
})

test_that("list_of() coerces to list() and list_of()", {
  expect_identical(
    vec_ptype_common(list_of(1), list()),
    list_of(.ptype = double())
  )
  expect_identical(
    vec_cast_common(list_of(1), list()),
    list(list_of(1), list_of(.ptype = double()))
  )

  # There is always a common type between list-of and list
  expect_identical(
    vec_ptype_common(list_of(1), list("")),
    list_of(.ptype = double())
  )
  # The incompatibility is raised upon casting
  expect_error(
    vec_cast_common(list_of(1), list("")),
    class = "vctrs_error_incompatible_type"
  )

  # One list() in the middle of common type reduction doesn't change
  # the result
  expect_error(
    vec_ptype_common(list_of(1), list_of("")),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype_common(list_of(1), list(), list_of("")),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("can concatenate list and list-of (#1161)", {
  expect_identical(
    vec_c(list(1), list_of(2)),
    list_of(1, 2)
  )
  expect_error(
    vec_c(list(""), list_of(2)),
    class = "vctrs_error_incompatible_type"
  )
})
