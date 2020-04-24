context("test-bind")

# rows --------------------------------------------------------------------

test_that("empty inputs return an empty data frame", {
  expect_equal(vec_rbind(), data_frame())
  expect_equal(vec_rbind(NULL, NULL), data_frame())
})

test_that("NULL is idempotent", {
  df <- data_frame(x = 1)
  expect_equal(vec_rbind(df, NULL), df)
})

test_that("output is tibble if any input is tibble", {
  df <- data_frame(x = 1)
  dt <- tibble::tibble(x = 1)

  expect_s3_class(vec_rbind(dt), "tbl_df")
  expect_s3_class(vec_rbind(dt, df), "tbl_df")
  expect_s3_class(vec_rbind(df, dt), "tbl_df")
})

test_that("type of column is common type of individual columns", {
  x_int <- data_frame(x = 1L)
  x_dbl <- data_frame(x = 2.5)
  x_chr <- data_frame(x = "a")

  expect_equal(vec_rbind(x_int, x_int), data_frame(x = c(1L, 1L)))
  expect_equal(vec_rbind(x_int, x_dbl), data_frame(x = c(1, 2.5)))

  expect_error(vec_rbind(x_int, x_chr), class = "vctrs_error_incompatible_type")
})

test_that("result contains union of columns", {
  expect_named(
    vec_rbind(
      data_frame(x = 1),
      data_frame(y = 1)
    ),
    c("x" , "y")
  )

  expect_named(
    vec_rbind(
      data_frame(y = 1, x = 1),
      data_frame(y = 1, z = 2)
    ),
    c("y", "x", "z")
  )
})

test_that("all inputs coerced to data frames", {
  expect_equal(
    vec_rbind(data_frame(x = 1L), c(x = 1.5)),
    data_frame(x = c(1, 1.5))
  )
})

test_that("names are supplied if needed", {
  expect_message(out <- vec_rbind(data_frame(...1 = 1), 1), "->")
  expect_equal(out, data_frame(...1 = c(1, 1)))
})

test_that("matrix becomes data frame and has names properly repaired", {
  x <- matrix(1:4, nrow = 2)
  expect_equal(vec_rbind(x), data.frame(...1 = 1:2, ...2 = 3:4))
})

test_that("can bind data.frame columns", {
  df <- data.frame(x = NA, y = 1:2)
  df$x <- data.frame(a = 1:2)

  expected <- data.frame(x = NA, y = c(1:2, 1:2))
  expected$x <- data.frame(a = c(1:2, 1:2))

  expect_equal(vec_rbind(df, df), expected)
})

test_that("can rbind unspecified vectors", {
  df <- data.frame(x = 1)
  expect_identical(vec_rbind(NA, df), data.frame(x = c(NA, 1)))
  expect_identical(vec_rbind(df, NA), data.frame(x = c(1, NA)))
  expect_identical(vec_rbind(NA, df, NA), data.frame(x = c(NA, 1, NA)))
  expect_identical(vec_rbind(c(x = NA), data.frame(x = 1)), data.frame(x = c(NA, 1)))
  expect_identical(vec_rbind(c(y = NA), df), data.frame(y = c(NA, NA), x = c(NA, 1)))

  out <- suppressMessages(vec_rbind(c(x = NA, x = NA), df))
  exp <- data.frame(x...1 = c(NA, NA), x...2 = c(NA, NA), x = c(NA, 1))
  expect_identical(out, exp)
})

test_that("as_df_row() tidies the names of unspecified vectors", {
  expect_identical(as_df_row(c(NA, NA)), c(NA, NA))
  expect_identical(as_df_row(unspecified(2)), unspecified(2))
  expect_identical(as_df_row(c(a = NA, a = NA), quiet = TRUE), data.frame(a...1 = NA, a...2 = NA))
  expect_identical(as_df_row(c(a = TRUE, a = TRUE), quiet = TRUE), data.frame(a...1 = TRUE, a...2 = TRUE))
})

test_that("can rbind spliced lists", {
  data <- list(c(a = 1, b = 2), c(a = TRUE, b = FALSE))
  expect_identical(vec_rbind(!!!data), data_frame(a = c(1, 1), b = c(2, 0)))
})

test_that("can rbind list columns", {
  out <- vec_rbind(data_frame(x = list(1, 2)), data_frame(x = list(3)))
  expect_identical(out, data_frame(x = list(1, 2, 3)))
})

test_that("can rbind lists", {
  out <- vec_rbind(list(x = 1:2))
  expect_identical(out, data_frame(x = list(c(1L, 2L))))

  out <- vec_rbind(list(x = 1:2, y = 3L))
  expect_identical(out, data_frame(x = list(c(1L, 2L)), y = list(3L)))

  out <- vec_rbind(list(x = 1, y = 2), list(y = "string"))
  expect_identical(out, data_frame(x = list(1, NULL), y = list(2, "string")))
})

test_that("can rbind factors", {
  fctr <- factor(c("a", "b"))
  expect_equal(vec_rbind(fctr), data_frame(...1 = fctr[1], ...2 = fctr[2]))

  fctr_named <- set_names(fctr)
  expect_equal(vec_rbind(fctr_named), data_frame(a = fctr[1], b = fctr[2]))
})

test_that("can rbind dates", {
  date <- new_date(c(0, 1))
  expect_equal(vec_rbind(date), data_frame(...1 = date[1], ...2 = date[2]))

  date_named <- set_names(date, c("a", "b"))
  expect_equal(vec_rbind(date_named), data_frame(a = date[1], b = date[2]))
})

test_that("can rbind POSIXlt objects into POSIXct objects", {
  datetime <- as.POSIXlt(new_datetime(0))
  expect_is(vec_rbind(datetime, datetime)[[1]], "POSIXct")

  datetime_named <- set_names(datetime, "col")
  expect_named(vec_rbind(datetime_named, datetime_named), "col")
})

test_that("can rbind table objects (#913)", {
  x <- new_table(1:4, c(2L, 2L))
  y <- x

  colnames <- c("c1", "c2")
  rownames <- c("r1", "r2", "r3", "r4")

  dimnames(x) <- list(rownames[1:2], colnames)
  dimnames(y) <- list(rownames[3:4], colnames)

  expect <- data.frame(c1 = c(1:2, 1:2), c2 = c(3:4, 3:4), row.names = rownames)

  expect_identical(vec_rbind(x, y), expect)
})

test_that("can rbind missing vectors", {
  expect_identical(vec_rbind(na_int), data_frame(...1 = na_int))
  expect_identical(vec_rbind(na_int, na_int), data_frame(...1 = int(na_int, na_int)))
})

test_that("can rbind unspecified vectors", {
  expect_identical(vec_rbind(NA), data_frame(...1 = NA))
  expect_identical(vec_rbind(NA, NA), data_frame(...1 = lgl(NA, NA)))
})

test_that("vec_rbind() respects size invariants (#286)", {
  expect_identical(vec_rbind(), new_data_frame(.size = 0L))

  expect_identical(vec_rbind(int(), int()), new_data_frame(.size = 2L))
  expect_identical(vec_rbind(int(), TRUE), new_data_frame(list(...1 = lgl(NA, TRUE))))

  expect_identical(vec_rbind(int(), new_data_frame(.size = 2L), int()), new_data_frame(.size = 4L))
})

test_that("can repair names in `vec_rbind()` (#229)", {
  expect_error(vec_rbind(.name_repair = "none"), "can't be `\"none\"`")
  expect_error(vec_rbind(.name_repair = "minimal"), "can't be `\"minimal\"`")

  expect_named(vec_rbind(list(a = 1, a = 2), .name_repair = "unique"), c("a...1", "a...2"))
  expect_error(vec_rbind(list(a = 1, a = 2), .name_repair = "check_unique"), class = "vctrs_error_names_must_be_unique")

  expect_named(vec_rbind(list(`_` = 1)), "_")
  expect_named(vec_rbind(list(`_` = 1), .name_repair = "universal"), c("._"))

  expect_named(vec_rbind(list(a = 1, a = 2), .name_repair = ~ toupper(.)), c("A", "A"))
})

test_that("can construct an id column", {
  df <- data.frame(x = 1)

  expect_named(vec_rbind(df, df, .names_to = "id"), c("id", "x"))
  expect_equal(vec_rbind(df, df, .names_to = "id")$id, c(1L, 2L))

  expect_equal(vec_rbind(a = df, b = df, .names_to = "id")$id, c("a", "b"))

  expect_equal(vec_rbind(a = df, df, .names_to = "id")$id, c("a", ""))
})

test_that("vec_rbind() fails with arrays of dimensionality > 3", {
  expect_error(vec_rbind(array(NA, c(1, 1, 1))), "Can't bind arrays")
})

test_that("row names are preserved by vec_rbind()", {
  df1 <- mtcars[1:3, ]
  df2 <- mtcars[4:5, ]
  expect_identical(vec_rbind(df1, df2), mtcars[1:5, ])

  row.names(df2) <- NULL
  out <- mtcars[1:5, ]
  row.names(out) <- c(row.names(df1), "...4", "...5")
  expect_identical(vec_rbind(df1, df2), out)
})

test_that("can assign row names in vec_rbind()", {
  df1 <- mtcars[1:3, ]
  df2 <- mtcars[4:5, ]

  # Combination
  out <- vec_rbind(
    foo = df1,
    df2,
    .names_to = NULL
  )
  exp <- mtcars[1:5, ]
  row.names(exp) <- c(paste0("foo...", row.names(df1)), row.names(df2))
  expect_identical(out, exp)

  out <- vec_rbind(foo = df1, df2, .names_to = "id")
  exp <- mtcars[1:5, ]
  exp <- vec_cbind(id = c(rep("foo", 3), rep("", 2)), exp)
  expect_identical(out, exp)

  # Sequence
  out <- vec_rbind(
    foo = unrownames(df1),
    df2,
    bar = unrownames(mtcars[6, ]),
    .names_to = NULL
  )
  exp <- mtcars[1:6, ]
  row.names(exp) <- c(paste0("foo", 1:3), row.names(df2), "bar")
  expect_identical(out, exp)

  out <- vec_rbind(
    foo = unrownames(df1),
    df2,
    bar = unrownames(mtcars[6, ]),
    .names_to = "id"
  )
  exp <- mtcars[1:6, ]
  exp <- vec_cbind(id = c(rep("foo", 3), rep("", 2), "bar"), exp)
  row.names(exp) <- c(paste0("...", 1:3), row.names(df2), "...6")
  expect_identical(out, exp)
})


# cols --------------------------------------------------------------------

test_that("empty inputs give data frame", {
  expect_equal(vec_cbind(), data_frame())
  expect_equal(vec_cbind(NULL), data_frame())
  expect_equal(vec_cbind(data.frame(a = 1), NULL), data_frame(a = 1))
})

test_that("NULL is idempotent", {
  df <- data_frame(x = 1)
  expect_equal(vec_cbind(df, NULL), df)
})

test_that("outer names are respected", {
  expect_named(vec_cbind(x = 1, y = 4), c("x", "y"))
  expect_named(vec_cbind(a = data.frame(x = 1)), "a")
})

test_that("inner names are respected", {
  expect_named(vec_cbind(data_frame(x = 1), data_frame(y = 1)), c("x", "y"))
})

test_that("nameless vectors get tidy defaults", {
  expect_named(vec_cbind(1:2, 1), c("...1", "...2"))
})

test_that("matrix becomes data frame", {
  x <- matrix(1:4, nrow = 2)
  expect_equal(vec_cbind(x), data.frame(...1 = 1:2, ...2 = 3:4))

  # Packed if named
  expect_equal(vec_cbind(x = x), data_frame(x = x))
})

test_that("duplicate names are de-deduplicated", {
  expect_message(
    expect_named(vec_cbind(x = 1, x = 1), c("x...1", "x...2")),
    "x -> x...1",
    fixed = TRUE
  )
  expect_named(vec_cbind(data.frame(x = 1), data.frame(x = 1)), c("x...1", "x...2"))
})

test_that("rows recycled to longest", {
  df <- data.frame(x = 1:3)

  expect_dim(vec_cbind(df), c(3, 1))
  expect_dim(vec_cbind(df, NULL), c(3, 1))
  expect_dim(vec_cbind(df, y = 1), c(3, 2))
  expect_dim(vec_cbind(data.frame(x = 1), y = 1:3), c(3, 2))

  expect_dim(
    vec_cbind(
      data.frame(a = 1, b = 2),
      y = 1:3
    ),
    c(3, 3)
  )
})

test_that("output is tibble if any input is tibble", {
  df <- data.frame(x = 1)
  dt <- tibble::tibble(y = 2)

  expect_s3_class(vec_cbind(dt), "tbl_df")
  expect_s3_class(vec_cbind(df, dt), "tbl_df")
  expect_s3_class(vec_cbind(dt, df), "tbl_df")
})

test_that("can override default .nrow", {
  expect_dim(vec_cbind(1, .size = 3), c(3, 1))
})

test_that("can repair names in `vec_cbind()` (#227)", {
  expect_error(vec_cbind(a = 1, a = 2, .name_repair = "none"), "can't be `\"none\"`")

  expect_named(vec_cbind(a = 1, a = 2, .name_repair = "unique"), c("a...1", "a...2"))
  expect_error(vec_cbind(a = 1, a = 2, .name_repair = "check_unique"), class = "vctrs_error_names_must_be_unique")

  expect_named(vec_cbind(`_` = 1, .name_repair = "universal"), "._")

  expect_named(vec_cbind(a = 1, a = 2, .name_repair = "minimal"), c("a", "a"))
  expect_named(vec_cbind(a = 1, a = 2, .name_repair = toupper), c("A", "A"))
})

test_that("can supply `.names_to` to `vec_rbind()` (#229)", {
  expect_error(vec_rbind(.names_to = letters), "must be")
  expect_error(vec_rbind(.names_to = 10), "must be")

  x <- data_frame(foo = 1:2, bar = 3:4)
  y <- data_frame(foo = 5L, bar = 6L)

  expect_identical(
    vec_rbind(a = x, b = y, .names_to = "quux"),
    data_frame(quux = c("a", "a", "b"), foo = c(1L, 2L, 5L), bar = c(3L, 4L, 6L))
  )
  expect_identical(
    vec_rbind(a = x, b = y, .names_to = "foo"),
    data_frame(foo = c("a", "a", "b"), bar = c(3L, 4L, 6L))
  )

  # No names
  expect_identical(
    vec_rbind(x, y, .names_to = "quux"),
    data_frame(quux = c(1L, 1L, 2L), foo = c(1L, 2L, 5L), bar = c(3L, 4L, 6L))
  )
  expect_identical(
    vec_rbind(x, y, .names_to = "foo"),
    data_frame(foo = c(1L, 1L, 2L), bar = c(3L, 4L, 6L))
  )

  # Partial names
  expect_identical(vec_rbind(x, b = y, .names_to = "quux")$quux, c("", "", "b"))
})

test_that("can supply existing `.names_to`", {
  x <- data.frame(a = 1, id = TRUE)
  expect_identical(
    vec_rbind(foo = x, bar = c(a = 2), .names_to = "id"),
    data_frame(a = c(1, 2), id = c("foo", "bar"))
  )

  y <- data.frame(id = TRUE, a = 1)
  expect_identical(
    vec_rbind(foo = y, bar = c(a = 2), .names_to = "id"),
    data_frame(id = c("foo", "bar"), a = c(1, 2))
  )
})

test_that("vec_cbind() returns visibly (#452)", {
  # Shouldn't be needed once `check_unique` is implemented in C
  expect_visible(vctrs::vec_cbind(x = 1, .name_repair = "check_unique"))
})

test_that("vec_cbind() packs named data frames (#446)", {
  expect_identical(vec_cbind(data_frame(y = 1:3)), data_frame(y = 1:3))
  expect_identical(vec_cbind(x = data_frame(y = 1:3)), data_frame(x = data_frame(y = 1:3)))
})

test_that("vec_cbind() packs 1d arrays", {
  a <- array(1:2)
  expect_identical(vec_cbind(a), data_frame(...1 = 1:2))
  expect_identical(vec_cbind(x = a), data_frame(x = a))
})

test_that("vec_cbind() packs named matrices", {
  m <- matrix(1:4, 2)
  expect_identical(vec_cbind(m), data_frame(...1 = 1:2, ...2 = 3:4))
  expect_identical(vec_cbind(x = m), data_frame(x = m))
})

test_that("vec_cbind() never packs named vectors", {
  expect_identical(vec_cbind(1:2), data_frame(...1 = 1:2))
  expect_identical(vec_cbind(x = 1:2), data_frame(x = 1:2))
})

test_that("names are repaired late if unpacked", {
  out1 <- vec_cbind(a = 1, data_frame(b = 2, b = 3))
  out2 <- vec_cbind(a = 1, as.matrix(data_frame(b = 2, b = 3)))
  out3 <- vec_cbind(a = 1, matrix(1:2, nrow = 1))
  expect_named(out1, c("a", "b...2", "b...3"))
  expect_named(out2, c("a", "b...2", "b...3"))
  expect_named(out3, c("a", "...2", "...3"))
})

test_that("names are not repaired if packed", {
  out1 <- vec_cbind(a = 1, packed = data_frame(b = 2, b = 3))
  out2 <- vec_cbind(a = 1, packed = as.matrix(data_frame(b = 2, b = 3)))
  out3 <- vec_cbind(a = 1, packed = matrix(1:2, nrow = 1))

  expect_named(out1, c("a", "packed"))
  expect_named(out2, c("a", "packed"))
  expect_named(out3, c("a", "packed"))

  expect_named(out1$packed, c("b", "b"))
  expect_identical(colnames(out2$packed), c("b", "b"))
  expect_identical(colnames(out3$packed), NULL)
})

test_that("vec_cbind() fails with arrays of dimensionality > 3", {
  a <- array(NA, c(1, 1, 1))
  expect_error(vec_cbind(a), "Can't bind arrays")
  expect_error(vec_cbind(x = a), "Can't bind arrays")
})

test_that("vec_rbind() consistently handles unnamed outputs", {
  # Name repair of columns is a little weird but unclear we can do better
  expect_identical(
    vec_rbind(1, 2, .names_to = NULL),
    data.frame(...1 = c(1, 2))
  )
  expect_identical(
    vec_rbind(1, 2, ...10 = 3, .names_to = NULL),
    data.frame(...1 = c(1, 2, 3), row.names = c("...1", "...2", "...3"))
  )

  expect_identical(
    vec_rbind(a = 1, b = 2, .names_to = NULL),
    data.frame(...1 = c(1, 2), row.names = c("a", "b"))
  )
  expect_identical(
    vec_rbind(c(a = 1), c(b = 2), .names_to = NULL),
    data.frame(a = c(1, NA), b = c(NA, 2))
  )
})

test_that("vec_rbind() ignores named inputs by default (#966)", {
  expect_identical(
    vec_rbind(foo = c(a = 1)),
    data.frame(a = 1)
  )
  expect_identical(
    vec_rbind(foo = c(a = 1), .names_to = NULL),
    data.frame(a = 1, row.names = "foo")
  )
})

test_that("vec_cbind() consistently handles unnamed outputs", {
  expect_identical(
    vec_cbind(1, 2),
    data.frame(...1 = 1, ...2 = 2)
  )
  expect_identical(
    vec_cbind(1, 2, ...10 = 3),
    data.frame(...1 = 1, ...2 = 2, ...3 = 3)
  )
  expect_identical(
    vec_cbind(a = 1, b = 2),
    data.frame(a = 1, b = 2)
  )
  expect_identical(
    vec_cbind(c(a = 1), c(b = 2)),
    new_data_frame(list(...1 = c(a = 1), ...2 = c(b = 2)))
  )
})

test_that("rbind() and cbind() have informative outputs when repairing names", {
  verify_output(test_path("output", "bind-name-repair.txt"), {
    "# vec_rbind()"
    vec_rbind(1, 2)
    vec_rbind(1, 2, .names_to = NULL)

    vec_rbind(1, 2, ...10 = 3)
    vec_rbind(1, 2, ...10 = 3, .names_to = NULL)

    vec_rbind(a = 1, b = 2)
    vec_rbind(a = 1, b = 2, .names_to = NULL)

    vec_rbind(c(a = 1), c(b = 2))
    vec_rbind(c(a = 1), c(b = 2), .names_to = NULL)

    vec_rbind(mtcars[1:4, ], mtcars[1:3, ])

    "# vec_cbind()"
    vec_cbind(1, 2)
    vec_cbind(1, 2, ...10 = 3)
    vec_cbind(a = 1, b = 2)
    vec_cbind(c(a = 1), c(b = 2))
  })
})

test_that("cbind() deals with row names", {
  expect_identical(
    vec_cbind(mtcars[1:3], foo = 1),
    cbind(mtcars[1:3], foo = 1)
  )
  expect_identical(
    vec_cbind(mtcars[1:3], mtcars[4]),
    cbind(mtcars[1:3], mtcars[4])
  )

  out <- vec_cbind(
    mtcars[1, 1, drop = FALSE],
    unrownames(mtcars[1:3, 2, drop = FALSE])
  )
  exp <- mtcars[1:3, c(1, 2)]
  exp[[1]] <- exp[[1, 1]]
  row.names(exp) <- paste0(c("Mazda RX4..."), 1:3)
  expect_identical(out, exp)

  # Should work once we have frame prototyping
  expect_error(
    vec_cbind(mtcars[1:3], vec_slice(mtcars[4], nrow(mtcars):1)),
    "different row names"
  )
})

test_that("can rbind data frames with matrix columns (#625)", {
  df <- tibble(x = 1:2, y = matrix(1:4, nrow = 2))
  expect_identical(vec_rbind(df, df), vec_slice(df, c(1, 2, 1, 2)))
})

test_that("rbind repairs names of data frames (#704)", {
  df <- data_frame(x = 1, x = 2)
  df_repaired <- data_frame(x...1 = 1, x...2 = 2)
  expect_identical(vec_rbind(df), df_repaired)
  expect_identical(vec_rbind(df, df), vec_rbind(df_repaired, df_repaired))

  expect_error(
    vec_rbind(df, df, .name_repair = "check_unique"),
    class = "vctrs_error_names_must_be_unique"
  )
})

test_that("vec_rbind() works with simple homogeneous foreign S3 classes", {
  expect_identical(
    vec_rbind(set_names(foobar(1), "x"), set_names(foobar(2), "x")),
    data_frame(x = foobar(c(1, 2)))
  )
})

test_that("vec_rbind() works with simple homogeneous foreign S4 classes", {
  skip_if_cant_set_names_on_s4()

  joe1 <- .Counts(1L, name = "Joe")
  joe2 <- .Counts(2L, name = "Joe")

  expect_identical(
    vec_rbind(set_names(joe1, "x"), set_names(joe2, "x")),
    data_frame(x = .Counts(1:2, name = "Joe"))
  )
})

test_that("vec_rbind() fails with complex foreign S3 classes", {
  verify_errors({
    x <- structure(foobar(1), attr_foo = "foo")
    y <- structure(foobar(2), attr_bar = "bar")
    expect_error(
      vec_rbind(set_names(x, "x"), set_names(y, "x")),
      class = "vctrs_error_incompatible_type"
    )
  })
})

test_that("vec_rbind() fails with complex foreign S4 classes", {
  verify_errors({
    joe <- .Counts(1L, name = "Joe")
    jane <- .Counts(2L, name = "Jane")
    expect_error(vec_rbind(joe, jane), class = "vctrs_error_incompatible_type")
  })
})

test_that("vec_rbind() falls back to c() if S3 method is available", {
  skip("TODO")
})

test_that("vec_rbind() falls back to c() if S4 method is available", {
  skip("TODO")
})

test_that("vec_cbind() and vec_rbind() have informative error messages", {
  skip_if_cant_set_names_on_s4()

  verify_output(test_path("error", "test-bind.txt"), {
    "# vec_rbind() fails with complex foreign S3 classes"
    x <- structure(foobar(1), attr_foo = "foo")
    y <- structure(foobar(2), attr_bar = "bar")
    vec_rbind(set_names(x, "x"), set_names(y, "x"))

    "# vec_rbind() fails with complex foreign S4 classes"
    joe <- .Counts(1L, name = "Joe")
    jane <- .Counts(2L, name = "Jane")
    vec_rbind(set_names(joe, "x"), set_names(jane, "x"))
  })
})

test_that("rbind supports names and inner names (#689)", {
  out <- vec_rbind(
    data_frame(x = list(a = 1, b = 2)),
    data_frame(x = list(3)),
    data_frame(x = list(d = 4))
  )
  expect_identical(out$x, list(a = 1, b = 2, 3, d = 4))

  vec_x <- set_names(1:3, letters[1:3])
  vec_y <- c(FOO = 4L)
  oo_x <- set_names(as.POSIXlt(c("2020-01-01", "2020-01-02", "2020-01-03")), letters[1:3])
  oo_y <- c(FOO = as.POSIXlt(c("2020-01-04")))
  df_x <- new_data_frame(list(x = 1:3), row.names = letters[1:3])
  df_y <- new_data_frame(list(x = 4L), row.names = "d")
  mat_x <- matrix(1:3, 3, dimnames = list(letters[1:3]))
  mat_y <- matrix(4L, 1, dimnames = list("d"))
  nested_x <- new_data_frame(
    list(df = df_x, mat = mat_x, vec = vec_x, oo = oo_x),
    row.names = c("foo", "bar", "baz")
  )
  nested_y <- new_data_frame(
    list(df = df_y, mat = mat_y, vec = vec_y, oo = oo_y),
    row.names = c("quux")
  )

  nested_out <- vec_rbind(nested_x, nested_y)
  expect_identical(row.names(nested_out), c("foo", "bar", "baz", "quux"))
  expect_identical(row.names(nested_out$df), c("a", "b", "c", "d"))
  expect_identical(row.names(nested_out$mat), c("a", "b", "c", "d"))
  expect_identical(names(nested_out$vec), c("a", "b", "c", "FOO"))
  expect_identical(names(nested_out$oo), c("a", "b", "c", "FOO"))
})
