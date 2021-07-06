
test_that("date-times have informative types", {
  expect_identical(vec_ptype_abbr(Sys.Date()), "date")
  expect_identical(vec_ptype_full(Sys.Date()), "date")

  expect_identical(vec_ptype_abbr(Sys.time()), "dttm")
  expect_identical(vec_ptype_full(Sys.time()), "datetime<local>")

  expect_identical(vec_ptype_abbr(new_duration(10)), "drtn")
  expect_identical(vec_ptype_full(new_duration(10)), "duration<secs>")
})

test_that("vec_ptype() returns a double date for integer dates", {
  x <- structure(0L, class = "Date")
  expect_true(is.double(vec_ptype(x)))
})

test_that("dates and times are vectors", {
  expect_true(vec_is(Sys.Date()))
  expect_true(vec_is(as.POSIXct("2020-01-01")))
  expect_true(vec_is(as.POSIXlt("2020-01-01")))
})

test_that("vec_cast() converts POSIXct with int representation to double when converting zones", {
  x <- structure(integer(), class = c("POSIXct", "POSIXt"), tzone = "UTC")
  y <- structure(numeric(), class = c("POSIXct", "POSIXt"), tzone = "America/Los_Angeles")
  expect_true(is.double(vec_cast(x, y)))
})

test_that("vec_c() converts POSIXct with int representation to double representation (#540)", {
  time1 <- seq(as.POSIXct("2015-12-01", tz = "UTC"), length.out = 2, by = "days")
  time2 <- vec_c(time1)
  expect_true(is.double(time2))

  time3 <- vec_c(time1, time1)
  expect_true(is.double(time3))
})

test_that("vec_c() and vec_rbind() convert Dates with int representation to double representation (#396)", {
  x <- structure(0L, class = "Date")
  df <- data.frame(x = x)

  expect_true(is.double(vec_c(x)))
  expect_true(is.double(vec_c(x, x)))

  expect_true(is.double(vec_rbind(df)$x))
  expect_true(is.double(vec_rbind(df, df)$x))
})

test_that("vec_c() and vec_ptype() standardize missing `tzone` attributes (#561)", {
  x <- structure(0L, class = c("POSIXct", "POSIXt"))
  expect_identical(attr(vec_ptype(x), "tzone"), "")
  expect_identical(attr(vec_c(x, x), "tzone"), "")
})

# constructor -------------------------------------------------------------

test_that("can create a date", {
  expect_identical(new_date(), structure(double(), class = "Date"))
  expect_identical(new_date(0), structure(0, class = "Date"))
})

test_that("retains input names", {
  expect_named(new_date(c(x = 0)), "x")
})

test_that("drops attributes except names", {
  expect_identical(new_date(structure(1, foo = "bar")), new_date(1))
})

test_that("only allows doubles", {
  expect_error(new_date(1L), "must be a double vector")
  expect_error(new_date("x"), "must be a double vector")
})

test_that("can create a datetime", {
  expect_identical(new_datetime(), structure(double(), class = c("POSIXct", "POSIXt"), tzone = ""))
  expect_identical(new_datetime(0), structure(0, class = c("POSIXct", "POSIXt"), tzone = ""))
})

test_that("retains input names", {
  expect_named(new_datetime(c(x = 0)), "x")
})

test_that("drops attributes except names", {
  expect_identical(new_datetime(structure(1, foo = "bar")), new_datetime(1))
})

test_that("only allows doubles", {
  expect_error(new_datetime(1L), "must be a double vector")
  expect_error(new_datetime("x"), "must be a double vector")
})

test_that("tzone is allowed to be `NULL`", {
  expect_identical(new_datetime(tzone = NULL), new_datetime(tzone = ""))
})

test_that("tzone must be character or `NULL`", {
  expect_error(new_datetime(tzone = 1), "character vector or `NULL`")
})

# coerce ------------------------------------------------------------------

test_that("datetime coercions are symmetric and unchanging", {
  types <- list(
    new_date(),
    new_datetime(),
    new_datetime(tzone = "US/Central"),
    as.POSIXlt(character(), tz = "US/Central"),
    difftime(Sys.time() + 1000, Sys.time()),
    difftime(Sys.time() + 1, Sys.time())
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(
    mat,
    test_path("test-type-date-time.txt"),
    print = TRUE,
    width = 200
  )
})

test_that("tz comes from first non-empty", {
  # On the assumption that if you've set the time zone explicitly it
  # should win

  x <- as.POSIXct("2020-01-01")
  y <- as.POSIXct("2020-01-01", tz = "America/New_York")

  expect_identical(vec_ptype2(x, y), y[0])
  expect_identical(vec_ptype2(y, x), y[0])

  z <- as.POSIXct("2020-01-01", tz = "Pacific/Auckland")
  expect_identical(vec_ptype2(y, z), y[0])
  expect_identical(vec_ptype2(z, y), z[0])
})

test_that("POSIXlt always steered towards POSIXct", {
  dtc <- as.POSIXct("2020-01-01", tz = "UTC")
  dtl <- as.POSIXlt("2020-01-01", tz = "UTC")

  expect_identical(vec_ptype2(dtc, dtl), dtc[0])
  expect_identical(vec_ptype2(dtl, dtc), dtc[0])
  expect_identical(vec_ptype2(dtl, dtl), dtc[0])
})

test_that("vec_ptype2() on a POSIXlt with multiple time zones returns the first", {
  x <- as.POSIXlt(new_datetime(), tz = "Pacific/Auckland")

  expect_identical(attr(x, "tzone"), c("Pacific/Auckland", "NZST", "NZDT"))
  expect_identical(attr(vec_ptype2(x, new_date()), "tzone"), "Pacific/Auckland")
})

test_that("vec_ptype2(<Date>, NA) is symmetric (#687)", {
  date <- new_date()
  expect_identical(
    vec_ptype2(date, NA),
    vec_ptype2(NA, date)
  )
})

test_that("vec_ptype2(<POSIXt>, NA) is symmetric (#687)", {
  time <- Sys.time()
  expect_identical(
    vec_ptype2(time, NA),
    vec_ptype2(NA, time)
  )
})

test_that("vec_ptype2(<difftime>, NA) is symmetric (#687)", {
  dtime <- Sys.time() - Sys.time()
  expect_identical(
    vec_ptype2(dtime, NA),
    vec_ptype2(NA, dtime)
  )
})


# cast: dates ---------------------------------------------------------------

test_that("safe casts work as expected", {
  date <- as.Date("2018-01-01")
  datetime_ct <- as.POSIXct(as.character(date))
  datetime_lt <- as.POSIXlt(datetime_ct)

  expect_identical(vec_cast(NULL, date), NULL)
  expect_identical(vec_cast(date, date), date)
  expect_identical(vec_cast(datetime_ct, date), date)
  expect_identical(vec_cast(datetime_lt, date), date)

  missing_date <- new_date(NA_real_)

  expect_identical(vec_cast(missing_date, missing_date), missing_date)
  expect_identical(vec_cast(as.POSIXct(missing_date), missing_date), missing_date)
  expect_identical(vec_cast(as.POSIXlt(missing_date), missing_date), missing_date)

  # These used to be allowed
  expect_error(vec_cast(17532, date), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast("2018-01-01", date), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(date), date), class = "vctrs_error_incompatible_type")
})

test_that("date - datetime cast can be roundtripped", {
  date <- as.Date("2018-01-01")
  datetime <- as.POSIXct("2018-01-01", tz = "America/New_York")

  expect_identical(vec_cast(vec_cast(date, datetime), date), date)
  expect_identical(vec_cast(vec_cast(datetime, date), datetime), datetime)
})

test_that("lossy casts generate error", {
  date <- as.Date("2018-01-01")
  datetime <- as.POSIXct(as.character(date)) + c(0, 3600)
  expect_lossy(vec_cast(datetime, date), vec_c(date, date), x = datetime, to = date)
})

test_that("invalid casts generate error", {
  date <- as.Date("2018-01-01")
  expect_error(vec_cast(integer(), date), class = "vctrs_error_incompatible_type")
})

test_that("can cast NA and unspecified to Date", {
  expect_identical(vec_cast(NA, new_date()), new_date(NA_real_))
  expect_identical(vec_cast(unspecified(2), new_date()), new_date(dbl(NA, NA)))
})

test_that("casting an integer date to another date returns a double date", {
  x <- structure(0L, class = "Date")
  expect_true(is.double(vec_cast(x, x)))
})

test_that("casting an integer POSIXct to a Date returns a double Date", {
  x <- .POSIXct(18000L, tz = "America/New_York")
  expect <- new_date(0)
  expect_identical(vec_cast(x, new_date()), expect)
})

# cast: datetimes -----------------------------------------------------------

test_that("safe casts work as expected", {
  datetime_c <- as.POSIXct("1970-02-01", tz = "UTC")
  datetime_l <- as.POSIXlt("1970-02-01", tz = "UTC")

  expect_identical(vec_cast(NULL, datetime_c), NULL)
  expect_identical(vec_cast(datetime_c, datetime_c), datetime_c)
  expect_identical(vec_cast(datetime_l, datetime_c), datetime_c)
  expect_identical(vec_cast(as.Date(datetime_c), datetime_c), datetime_c)

  expect_identical(vec_cast(NULL, datetime_l), NULL)
  expect_identical(vec_cast(datetime_c, datetime_l), datetime_l)
  expect_identical(vec_cast(datetime_l, datetime_l), datetime_l)
  expect_identical(vec_cast(as.Date(datetime_l), datetime_l), datetime_l)
  expect_error(vec_cast(raw(), datetime_l), class = "vctrs_error_incompatible_type")

  missing_c <- new_datetime(NA_real_, tzone = "UTC")
  missing_l <- as.POSIXlt(missing_c)

  expect_identical(vec_cast(missing_c, missing_c), missing_c)
  expect_identical(vec_cast(missing_l, missing_c), missing_c)
  expect_identical(vec_cast(as.Date(missing_c), missing_c), missing_c)

  expect_identical(vec_cast(missing_l, missing_l), missing_l)
  expect_identical(vec_cast(missing_c, missing_l), missing_l)
  expect_identical(vec_cast(as.Date(missing_l), missing_l), missing_l)

  # These used to be allowed
  expect_error(vec_cast(2678400, datetime_c), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast("1970-02-01", datetime_c), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(datetime_c), datetime_c), class = "vctrs_error_incompatible_type")

  expect_error(vec_cast(2678400, datetime_l), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast("1970-02-01", datetime_l), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(datetime_l), datetime_l), class = "vctrs_error_incompatible_type")
})

test_that("invalid casts generate error", {
  datetime <- as.POSIXct("1970-02-01", tz = "UTC")
  expect_error(vec_cast(integer(), datetime), class = "vctrs_error_incompatible_type")
})

test_that("dates become midnight in date-time tzone", {
  date1 <- as.Date("2010-01-01")
  datetime_c <- as.POSIXct(character(), tz = "Pacific/Auckland")
  datetime_l <- as.POSIXlt(character(), tz = "Pacific/Auckland")

  date2_c <- vec_cast(date1, datetime_c)
  expect_identical(tzone(date2_c), "Pacific/Auckland")
  expect_identical(format(date2_c, "%H:%M"), "00:00")

  date2_l <- vec_cast(date1, datetime_l)
  expect_identical(tzone(date2_l), "Pacific/Auckland")
  expect_identical(format(date2_l, "%H:%M"), "00:00")
})

test_that("can cast NA and unspecified to POSIXct and POSIXlt", {
  dtc <- as.POSIXct("2020-01-01")
  dtl <- as.POSIXlt("2020-01-01")
  expect_identical(vec_cast(NA, dtc), vec_init(dtc))
  expect_identical(vec_cast(NA, dtl), vec_init(dtl))
  expect_identical(vec_cast(unspecified(2), dtc), vec_init(dtc, 2))
  expect_identical(vec_cast(unspecified(2), dtl), vec_init(dtl, 2))
})

test_that("changing time zones retains the underlying moment in time", {
  x_ct <- as.POSIXct("2019-01-01", tz = "America/New_York")
  x_lt <- as.POSIXlt(x_ct)

  to_ct <- new_datetime(tzone = "America/Los_Angeles")
  to_lt <- as.POSIXlt(to_ct)

  expect_ct <- x_ct
  attr(expect_ct, "tzone") <- "America/Los_Angeles"
  expect_lt <- as.POSIXlt(expect_ct)

  expect_identical(vec_cast(x_ct, to_ct), expect_ct)
  expect_identical(vec_cast(x_ct, to_lt), expect_lt)
  expect_identical(vec_cast(x_lt, to_ct), expect_ct)
  expect_identical(vec_cast(x_lt, to_lt), expect_lt)
})

test_that("casting to date always retains the zoned year-month-day value", {
  x <- as.POSIXct("2019-01-01", tz = "Asia/Shanghai")
  expect_identical(vec_cast(x, new_date()), as.Date("2019-01-01"))
})

# cast: durations ------------------------------------------------------------

test_that("safe casts work as expected", {
  dt1 <- as.difftime(600, units = "secs")
  dt2 <- as.difftime(10, units = "mins")

  expect_identical(vec_cast(NULL, dt1), NULL)
  expect_identical(vec_cast(dt1, dt1), dt1)
  expect_identical(vec_cast(dt1, dt2), dt2)

  # These used to be allowed
  expect_error(vec_cast(600, dt1), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(dt1), dt1), class = "vctrs_error_incompatible_type")
})

test_that("invalid casts generate error", {
  dt <- as.difftime(600, units = "secs")
  expect_error(vec_cast(integer(), dt), class = "vctrs_error_incompatible_type")
})

test_that("can cast NA and unspecified to duration", {
  expect_identical(vec_cast(NA, new_duration()), new_duration(na_dbl))
  expect_identical(vec_cast(unspecified(2), new_duration()), new_duration(dbl(NA, NA)))
})

# proxy/restore: dates ---------------------------------------------------

test_that("restoring an integer to an integer Date converts to double", {
  x <- structure(0L, class = "Date")
  expect_true(is.double(vec_restore(x, x)))
})

test_that("vec_proxy() returns a double for Dates with int representation", {
  x <- structure(0L, class = "Date")
  expect_true(is.double(vec_proxy(x)))
})

# proxy/restore: datetimes ------------------------------------------------

test_that("restoring an integer to an integer POSIXct converts to double", {
  x <- structure(0L, class = c("POSIXct", "POSIXt"))
  expect_true(is.double(vec_restore(x, x)))
})

test_that("restoring to a POSIXct with no time zone standardizes to an empty string (#561)", {
  x <- structure(0L, class = c("POSIXct", "POSIXt"))
  expect_identical(attr(vec_restore(x, x), "tzone"), "")
})

test_that("restoring to a POSIXlt with no time zone standardizes to an empty string", {
  # Manually create a POSIXlt without a `tzone` attribute.
  # This is just:
  # `x <- as.POSIXlt("1970-01-01")`
  # which usually won't add a `tzone` attribute, but platforms where the local
  # time is UTC attach a `tzone` attribute automatically.
  x <- structure(
    list(
      sec = 0, min = 0L, hour = 0L, mday = 1L,
      mon = 0L, year = 70L, wday = 4L, yday = 0L,
      isdst = 0L, zone = "EST", gmtoff = NA_integer_
    ),
    class = c("POSIXlt", "POSIXt")
  )

  proxy <- vec_proxy(x)

  expect_identical(attr(vec_restore(proxy, x), "tzone"), "")
})

test_that("proxying a POSIXct with no time zone standardizes to an empty string", {
  x <- structure(0L, class = c("POSIXct", "POSIXt"))
  expect_identical(attr(vec_proxy(x), "tzone"), "")
})

test_that("vec_proxy() returns a double for POSIXct with int representation", {
  x <- structure(0L, class = c("POSIXct", "POSIXt"))
  expect_true(is.double(vec_proxy(x)))
})

test_that("POSIXlt roundtrips through proxy and restore", {
  x <- as_posixlt("2020-01-03")
  out <- vec_restore(vec_proxy(x), x)
  expect_identical(out, x)
})

test_that("subclassed Dates / POSIXct / POSIXlt can be restored (#1015)", {
  x <- subclass(new_date(0))
  proxy <- vec_proxy(x)
  expect_identical(vec_restore(proxy, x), x)

  y <- subclass(new_datetime(0))
  proxy <- vec_proxy(y)
  expect_identical(vec_restore(proxy, y), y)

  z <- subclass(as.POSIXlt(new_datetime(0)))
  proxy <- vec_proxy(z)
  expect_identical(vec_restore(proxy, z), z)
})

# arithmetic --------------------------------------------------------------

test_that("default is error", {
  d <- as.Date("2018-01-01")
  dt <- as.POSIXct("2018-01-02 12:00")
  lt <- as.POSIXlt(dt)
  t <- as.difftime(12, units = "hours")
  f <- factor("x")

  expect_error(vec_arith("+", d, f), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", dt, f), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", lt, f), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("+", t, f), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("*", dt, t), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("*", lt, t), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("*", d, t), class = "vctrs_error_incompatible_op")
  expect_error(vec_arith("!", t, MISSING()), class = "vctrs_error_incompatible_op")
})

test_that("date-time vs date-time", {
  d <- as.Date("2018-01-01")
  dt <- as.POSIXct(d)
  lt <- as.POSIXlt(dt)

  expect_error(vec_arith("+", d, d), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", d, d), d - d)

  expect_error(vec_arith("+", dt, dt), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", dt, dt), dt - dt)

  expect_error(vec_arith("+", lt, lt), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", lt, lt), lt - lt)


  expect_error(vec_arith("+", d, dt), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", d, dt), difftime(d, dt))

  expect_error(vec_arith("+", dt, d), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", dt, d), difftime(dt, d))


  expect_error(vec_arith("+", d, lt), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", d, lt), difftime(d, lt))

  expect_error(vec_arith("+", lt, d), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", lt, d), difftime(lt, d))


  expect_error(vec_arith("+", dt, lt), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", dt, lt), difftime(dt, lt))

  expect_error(vec_arith("+", lt, dt), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("-", lt, dt), difftime(lt, dt))
})

test_that("date-time vs integer/double", {
   d <- as.Date("2018-01-01")
   dt <- as.POSIXct("2018-01-01", tz = "America/New_York")
   lt <- as.POSIXlt(dt)

   expect_identical(vec_arith("+", d, 1L), d + 1L)
   expect_identical(vec_arith("+", 1L, d), d + 1L)
   expect_identical(vec_arith("-", d, 1L), d - 1L)
   expect_error(vec_arith("-", 1L, d), class = "vctrs_error_incompatible_op")

   expect_identical(vec_arith("+", d, 1), d + 1)
   expect_identical(vec_arith("+", 1, d), d + 1)
   expect_identical(vec_arith("-", d, 1), d - 1)
   expect_error(vec_arith("-", 1, d), class = "vctrs_error_incompatible_op")


   expect_identical(vec_arith("+", dt, 1L), dt + 1L)
   expect_identical(vec_arith("+", 1L, dt), dt + 1L)
   expect_identical(vec_arith("-", dt, 1L), dt - 1L)
   expect_error(vec_arith("-", 1L, dt), class = "vctrs_error_incompatible_op")

   expect_identical(vec_arith("+", dt, 1), dt + 1)
   expect_identical(vec_arith("+", 1, dt), dt + 1)
   expect_identical(vec_arith("-", dt, 1), dt - 1)
   expect_error(vec_arith("-", 1, dt), class = "vctrs_error_incompatible_op")


   expect_identical(vec_arith("+", lt, 1L), lt + 1L)
   expect_identical(vec_arith("+", 1L, lt), lt + 1L)
   expect_identical(vec_arith("-", lt, 1L), lt - 1L)
   expect_error(vec_arith("-", 1L, lt), class = "vctrs_error_incompatible_op")

   expect_identical(vec_arith("+", lt, 1), lt + 1)
   expect_identical(vec_arith("+", 1, lt), lt + 1)
   expect_identical(vec_arith("-", lt, 1), lt - 1)
   expect_error(vec_arith("-", 1, lt), class = "vctrs_error_incompatible_op")


   expect_error(vec_arith("*", 1L, d), class = "vctrs_error_incompatible_op")
   expect_error(vec_arith("*", d, 1L), class = "vctrs_error_incompatible_op")

   expect_error(vec_arith("*", 1, d), class = "vctrs_error_incompatible_op")
   expect_error(vec_arith("*", d, 1), class = "vctrs_error_incompatible_op")


   expect_error(vec_arith("*", 1L, dt), class = "vctrs_error_incompatible_op")
   expect_error(vec_arith("*", dt, 1L), class = "vctrs_error_incompatible_op")

   expect_error(vec_arith("*", 1, dt), class = "vctrs_error_incompatible_op")
   expect_error(vec_arith("*", dt, 1), class = "vctrs_error_incompatible_op")


   expect_error(vec_arith("*", 1L, lt), class = "vctrs_error_incompatible_op")
   expect_error(vec_arith("*", lt, 1L), class = "vctrs_error_incompatible_op")

   expect_error(vec_arith("*", 1, lt), class = "vctrs_error_incompatible_op")
   expect_error(vec_arith("*", lt, 1), class = "vctrs_error_incompatible_op")
})

test_that("POSIXlt + integer/double = POSIXct", {
  lt <- as.POSIXlt("2018-01-01", tz = "America/New_York")

  expect_s3_class(vec_arith("+", lt, 1L), "POSIXct")
  expect_s3_class(vec_arith("+", 1L, lt), "POSIXct")

  expect_s3_class(vec_arith("+", lt, 1), "POSIXct")
  expect_s3_class(vec_arith("+", 1, lt), "POSIXct")
})

test_that("vec_arith() standardizes the `tzone` attribute", {
  dt <- structure(0, class = c("POSIXct", "POSIXt"))
  x <- vec_arith("+", dt, 1)
  expect_identical(attr(x, "tzone"), "")
})

test_that("date-time vs difftime", {
  d <- as.Date("2018-01-01")
  dt <- as.POSIXct("2018-01-01", tz = "UTC")
  lt <- as.POSIXlt(dt)
  t <- as.difftime(1, units = "days")
  th <- as.difftime(c(1, 24), units = "hours")

  expect_identical(vec_arith("+", d, t), d + t)
  expect_identical(vec_arith("+", t, d), t + d)

  expect_identical(vec_arith("+", dt, t), dt + t)
  expect_identical(vec_arith("+", t, dt), t + dt)

  expect_identical(vec_arith("+", lt, t), lt + t)
  expect_identical(vec_arith("+", t, lt), t + lt)


  expect_lossy(vec_arith("+", d, th), d + th, x = t, to = d)
  expect_lossy(vec_arith("+", th, d), th + d, x = t, to = d)

  expect_identical(vec_arith("+", dt, th), dt + th)
  expect_identical(vec_arith("+", th, dt), th + dt)

  expect_identical(vec_arith("+", lt, th), lt + th)
  expect_identical(vec_arith("+", th, lt), th + lt)


  expect_identical(vec_arith("-", d, t), d - t)
  expect_error(vec_arith("-", t, d), class = "vctrs_error_incompatible_op")

  expect_identical(vec_arith("-", dt, t), dt - t)
  expect_error(vec_arith("-", t, dt), class = "vctrs_error_incompatible_op")

  expect_identical(vec_arith("-", lt, t), lt - t)
  expect_error(vec_arith("-", t, lt), class = "vctrs_error_incompatible_op")


  expect_lossy(vec_arith("-", d, th), d - th, x = t, to = d)
  expect_error(vec_arith("-", th, d), class = "vctrs_error_incompatible_op")

  expect_identical(vec_arith("-", dt, th), dt - th)
  expect_error(vec_arith("-", th, dt), class = "vctrs_error_incompatible_op")

  expect_identical(vec_arith("-", lt, th), lt - th)
  expect_error(vec_arith("-", th, lt), class = "vctrs_error_incompatible_op")
})

test_that("difftime vs difftime/integer/double", {
  t <- as.difftime(12, units = "hours")

  expect_identical(vec_arith("-", t, MISSING()), -t)
  expect_identical(vec_arith("+", t, MISSING()), t)

  expect_identical(vec_arith("-", t, t), t - t)
  expect_identical(vec_arith("-", t, 1L), t - 1L)
  expect_identical(vec_arith("-", 1L, t), 1L - t)
  expect_identical(vec_arith("-", t, 1), t - 1)
  expect_identical(vec_arith("-", 1, t), 1 - t)

  expect_identical(vec_arith("+", t, t), 2 * t)
  expect_identical(vec_arith("+", t, 1L), t + 1L)
  expect_identical(vec_arith("+", 1L, t), t + 1L)
  expect_identical(vec_arith("+", t, 1), t + 1)
  expect_identical(vec_arith("+", 1, t), t + 1)

  expect_identical(vec_arith("*", 2L, t), 2L * t)
  expect_identical(vec_arith("*", t, 2L), 2L * t)
  expect_identical(vec_arith("*", 2, t), 2 * t)
  expect_identical(vec_arith("*", t, 2), 2 * t)
  expect_error(vec_arith("*", t, t), class = "vctrs_error_incompatible_op")

  expect_identical(vec_arith("/", t, 2L), t / 2L)
  expect_error(vec_arith("/", 2L, t), class = "vctrs_error_incompatible_op")
  expect_identical(vec_arith("/", t, 2), t / 2)
  expect_error(vec_arith("/", 2, t), class = "vctrs_error_incompatible_op")

  expect_identical(vec_arith("/", t, t), 1)
  expect_identical(vec_arith("%/%", t, t), 1)
  expect_identical(vec_arith("%%", t, t), 0)
})


# Math --------------------------------------------------------------------

test_that("date and date times don't support math", {
  expect_error(vec_math("sum", new_date()), class = "vctrs_error_unsupported")
  expect_error(vec_math("sum", new_datetime()), class = "vctrs_error_unsupported")
  expect_error(vec_math("sum", as.POSIXlt(new_datetime())), class = "vctrs_error_unsupported")
})
