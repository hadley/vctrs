context("test-fields")

test_that("n_fields captures number of fields", {
  r <- new_rcrd(list(x = 1, y = 2))
  expect_equal(n_fields(r), 2)
})


# get ---------------------------------------------------------------------

test_that("can extract valid field", {
  r <- new_rcrd(list(x = 1, y = 2))

  expect_equal(field(r, "x"), 1)
  expect_equal(field(r, 1L), 1)
})

test_that("can extract field even if encoding is different", {
  x1 <- "fa\u00e7ile"
  skip_if_not(Encoding(x1) == "UTF-8")

  x2 <- iconv(x1, from = "UTF-8", to = "latin1")
  skip_if_not(Encoding(x2) == "latin1")

  r <- new_rcrd(setNames(list(1), x1))
  expect_equal(field(r, x1), 1)
  expect_equal(field(r, x2), 1)
})

test_that("invalid indices throw error", {
  r <- new_rcrd(list(x = 1, y = 2))

  expect_error(field(r, "z"), "Invalid index")
  expect_error(field(r, NA_character_), "Invalid index")
  expect_error(field(r, ""), "Invalid index")
  expect_error(field(r, letters), "Invalid index")

  expect_error(field(r, 0L), "Invalid index")
  expect_error(field(r, NA_integer_), "Invalid index")

  expect_error(field(r, 0), "Invalid index")
  expect_error(field(r, NA_real_), "Invalid index")
  expect_error(field(r, Inf), "Invalid index")

  expect_error(field(r, mean), "Invalid index")

})

test_that("corrupt rcrd throws error", {
  r <- new_rcrd(list(x = 1, y = 2))

  expect_error(field(1:10, 1L), "Corrupt rcrd")
  expect_error(field(list(), 1L), "Corrupt rcrd")

  expect_error(field(list(1), "x"), "Corrupt x")
  expect_error(field(setNames(list(1, 1), "y"), "x"), "Corrupt x")
})


# set ---------------------------------------------------------------------

test_that("field<- modifies a copy", {
  r1 <- new_rcrd(list(x = 1, y = 2))
  r2 <- r1

  field(r1, "x") <- 3
  expect_equal(field(r1, "x"), 3)
  expect_equal(field(r2, "x"), 1)
})

test_that("field<- checks inputs", {
  x <- list()
  expect_error(field(x, "x") <- 1, "Corrupt rcrd")

  r <- new_rcrd(list(x = 1))
  expect_error(field(r, "x") <- 1:3, "Invalid value")
  expect_error(field(r, "x") <- environment(), "Invalid value")
})

test_that("field<- respects size, not length (#450)", {
  r1 <- new_rcrd(list(df = new_data_frame(.size = 2L)))
  new_df <- data.frame(x = 1:2)

  field(r1, 'df') <- new_df
  expect_equal(field(r1, "df"), new_df)

  expect_error(field(r1, 'df') <- new_data_frame(.size = 3L), "Invalid value")
})
