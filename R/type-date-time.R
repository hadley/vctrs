#' Date, date-time, and duration S3 classes
#'
#' * A `date` ([Date]) is a double vector. Its value represent the number
#'   of days since the Unix "epoch", 1970-01-01. It has no attributes.
#' * A `datetime` ([POSIXct] is a double vector. Its value represents the
#'   number of seconds since the Unix "Epoch", 1970-01-01. It has a single
#'   attribute: the timezone (`tzone`))
#' * A `duration` ([difftime])
#'
#' These function help the base `Date`, `POSIXct`, and `difftime` classes fit
#' into the vctrs type system by providing constructors, coercion functions,
#' and casting functions.
#'
#' @param x A double vector representing the number of days since UNIX
#'   epoch for `new_date()`, number of seconds since UNIX epoch for
#'   `new_datetime()`, and number of `units` for `new_duration()`.
#' @param tzone Time zone. A character vector of length 1. Either `""` for
#'   the local time zone, or a value from [OlsonNames()]
#' @param units Units of duration.
#' @export
#' @keywords internal
#' @examples
#' new_date(0)
#' new_datetime(0, tzone = "UTC")
#' new_duration(1, "hours")
new_date <- function(x = double()) {
  .Call(vctrs_new_date, x)
}

#' @export
#' @rdname new_date
new_datetime <- function(x = double(), tzone = "") {
  .Call(vctrs_new_datetime, x, tzone)
}

#' @export
#' @rdname new_date
new_duration <- function(x = double(), units = c("secs", "mins", "hours", "days", "weeks")) {
  stopifnot(is.double(x))
  units <- arg_match0(units, c("secs", "mins", "hours", "days", "weeks"))

  structure(
    x,
    units = units,
    class = "difftime"
  )
}

#' @export
vec_proxy.Date <- function(x, ...) {
  date_validate(x)
}

#' @export
vec_proxy.POSIXct <- function(x, ...) {
  datetime_validate(x)
}

#' @export
vec_proxy.POSIXlt <- function(x, ...) {
  new_data_frame(unclass(x))
}
#' @export
vec_proxy_equal.POSIXlt <- function(x, ...) {
  x <- vec_cast(x, new_datetime(tzone = tzone(x)))
  vec_proxy_equal(x, ...)
}
#' @export
vec_proxy_compare.POSIXlt <- function(x, ...) {
  x <- vec_cast(x, new_datetime(tzone = tzone(x)))
  vec_proxy_compare(x)
}

#' @export
vec_restore.Date <- function(x, to, ...) {
  NextMethod()
}

#' @export
vec_restore.POSIXct <- function(x, to, ...) {
  NextMethod()
}

#' @export
vec_restore.POSIXlt <- function(x, to, ...) {
  NextMethod()
}

# Print ------------------------------------------------------------------

#' @export
vec_ptype_full.Date <- function(x, ...) {
  "date"
}

#' @export
vec_ptype_abbr.Date <- function(x, ...) {
  "date"
}

#' @export
vec_ptype_full.POSIXct <- function(x, ...) {
  tzone <- if (tzone_is_local(x)) "local" else tzone(x)
  paste0("datetime<", tzone, ">")
}

#' @export
vec_ptype_full.POSIXlt <- function(x, ...) {
  tzone <- if (tzone_is_local(x)) "local" else tzone(x)
  paste0("POSIXlt<", tzone, ">")
}

#' @export
vec_ptype_abbr.POSIXt <- function(x, ...) {
  "dttm"
}

#' @export
vec_ptype_full.difftime <- function(x, ...) {
  paste0("duration<", attr(x, "units"), ">")
}

#' @export
vec_ptype_abbr.difftime <- function(x, ...) {
  "drtn"
}

# Coerce ------------------------------------------------------------------

#' @rdname new_date
#' @export vec_ptype2.Date
#' @method vec_ptype2 Date
#' @export
vec_ptype2.Date <- function(x, y, ...) {
  UseMethod("vec_ptype2.Date")
}
#' @method vec_ptype2.Date Date
#' @export
vec_ptype2.Date.Date <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.Date.Date")
}
#' @method vec_ptype2.Date POSIXct
#' @export
vec_ptype2.Date.POSIXct <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.Date.POSIXct")
}
#' @method vec_ptype2.Date POSIXlt
#' @export
vec_ptype2.Date.POSIXlt <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.Date.POSIXlt")
}

#' @rdname new_date
#' @export vec_ptype2.POSIXct
#' @method vec_ptype2 POSIXct
#' @export
vec_ptype2.POSIXct <- function(x, y, ...) {
  UseMethod("vec_ptype2.POSIXct")
}
#' @method vec_ptype2.POSIXct POSIXct
#' @export
vec_ptype2.POSIXct.POSIXct <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.POSIXct.POSIXct")
}
#' @method vec_ptype2.POSIXct Date
#' @export
vec_ptype2.POSIXct.Date <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.POSIXct.Date")
}
#' @method vec_ptype2.POSIXct POSIXlt
#' @export
vec_ptype2.POSIXct.POSIXlt <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.POSIXct.POSIXlt")
}

#' @rdname new_date
#' @export vec_ptype2.POSIXlt
#' @method vec_ptype2 POSIXlt
#' @export
vec_ptype2.POSIXlt <- function(x, y, ...) {
  UseMethod("vec_ptype2.POSIXlt")
}
#' @method vec_ptype2.POSIXlt POSIXlt
#' @export
vec_ptype2.POSIXlt.POSIXlt <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.POSIXlt.POSIXlt")
}
#' @method vec_ptype2.POSIXlt Date
#' @export
vec_ptype2.POSIXlt.Date <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.POSIXlt.Date")
}
#' @method vec_ptype2.POSIXlt POSIXct
#' @export
vec_ptype2.POSIXlt.POSIXct <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.POSIXlt.POSIXct")
}

#' @rdname new_date
#' @export vec_ptype2.difftime
#' @method vec_ptype2 difftime
#' @export
vec_ptype2.difftime <- function(x, y, ...) UseMethod("vec_ptype2.difftime")
#' @method vec_ptype2.difftime difftime
#' @export
vec_ptype2.difftime.difftime <- function(x, y, ...) new_duration(units = units_union(x, y))

# Cast --------------------------------------------------------------------

#' @rdname new_date
#' @export vec_cast.Date
#' @method vec_cast Date
#' @export
vec_cast.Date <- function(x, to, ...) {
  UseMethod("vec_cast.Date")
}
#' @export
#' @method vec_cast.Date Date
vec_cast.Date.Date <- function(x, to, ...) {
  stop_native_implementation("vec_cast.Date.Date")
}
#' @export
#' @method vec_cast.Date POSIXct
vec_cast.Date.POSIXct <- function(x, to, ...) {
  # TODO: Mark with `stop_native_implementation()` when we use lazy errors
  date_cast(x, to, ...)
}
#' @export
#' @method vec_cast.Date POSIXlt
vec_cast.Date.POSIXlt <- function(x, to, ...) {
  # TODO: Mark with `stop_native_implementation()` when we use lazy errors
  date_cast(x, to, ...)
}

# TODO: Remove when we have lazy errors
date_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- as.Date(x, tz = tzone(x))

  x_ct <- as.POSIXct(x)
  out_ct <- as.POSIXct(as.character(out), tz = tzone(x))
  lossy <- abs(x_ct - out_ct) > 1e-9 & !is.na(x)

  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

#' @rdname new_date
#' @export vec_cast.POSIXct
#' @method vec_cast POSIXct
#' @export
vec_cast.POSIXct <- function(x, to, ...) {
  UseMethod("vec_cast.POSIXct")
}
#' @export
#' @method vec_cast.POSIXct Date
vec_cast.POSIXct.Date <- function(x, to, ...) {
  stop_native_implementation("vec_cast.POSIXct.Date")
}
#' @export
#' @method vec_cast.POSIXct POSIXlt
vec_cast.POSIXct.POSIXlt <- function(x, to, ...) {
  stop_native_implementation("vec_cast.POSIXct.POSIXlt")
}
#' @export
#' @method vec_cast.POSIXct POSIXct
vec_cast.POSIXct.POSIXct <- function(x, to, ...) {
  stop_native_implementation("vec_cast.POSIXct.POSIXct")
}

#' @rdname new_date
#' @export vec_cast.POSIXlt
#' @method vec_cast POSIXlt
#' @export
vec_cast.POSIXlt <- function(x, to, ...) {
  UseMethod("vec_cast.POSIXlt")
}
#' @export
#' @method vec_cast.POSIXlt Date
vec_cast.POSIXlt.Date <- function(x, to, ...) {
  stop_native_implementation("vec_cast.POSIXlt.Date")
}
#' @export
#' @method vec_cast.POSIXlt POSIXlt
vec_cast.POSIXlt.POSIXlt <- function(x, to, ...) {
  stop_native_implementation("vec_cast.POSIXlt.POSIXlt")
}
#' @export
#' @method vec_cast.POSIXlt POSIXct
vec_cast.POSIXlt.POSIXct <- function(x, to, ...) {
  stop_native_implementation("vec_cast.POSIXlt.POSIXct")
}


#' @rdname new_date
#' @export vec_cast.difftime
#' @method vec_cast difftime
#' @export
vec_cast.difftime <- function(x, to, ...) {
  UseMethod("vec_cast.difftime")
}
#' @export
#' @method vec_cast.difftime difftime
vec_cast.difftime.difftime <- function(x, to, ...) {
  if (identical(units(x), units(to))) {
    x
  } else {
    # Hack: I can't see any obvious way of changing the units
    origin <- as.POSIXct(0, origin = "1970-01-01")
    difftime(origin, origin - x, units = units(to))
  }
}


# Arithmetic --------------------------------------------------------------

#' @rdname new_date
#' @export vec_arith.Date
#' @method vec_arith Date
#' @export
vec_arith.Date <- function(op, x, y, ...) UseMethod("vec_arith.Date", y)
#' @rdname new_date
#' @export vec_arith.POSIXct
#' @method vec_arith POSIXct
#' @export
vec_arith.POSIXct <- function(op, x, y, ...) UseMethod("vec_arith.POSIXct", y)
#' @rdname new_date
#' @export vec_arith.POSIXlt
#' @method vec_arith POSIXlt
#' @export
vec_arith.POSIXlt <- function(op, x, y, ...) UseMethod("vec_arith.POSIXlt", y)
#' @rdname new_date
#' @export vec_arith.difftime
#' @method vec_arith difftime
#' @export
vec_arith.difftime <- function(op, x, y, ...) UseMethod("vec_arith.difftime", y)

#' @method vec_arith.Date default
#' @export
vec_arith.Date.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @method vec_arith.POSIXct default
#' @export
vec_arith.POSIXct.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @method vec_arith.POSIXlt default
#' @export
vec_arith.POSIXlt.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @method vec_arith.difftime default
#' @export
vec_arith.difftime.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)

#' @method vec_arith.Date Date
#' @export
vec_arith.Date.Date <- function(op, x, y, ...) {
  switch(op,
    `-` = difftime(x, y, units = "days"),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXct POSIXct
#' @export
vec_arith.POSIXct.POSIXct <- function(op, x, y, ...) {
  switch(op,
    `-` = difftime(x, y, units = "secs"),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXlt POSIXlt
#' @export
vec_arith.POSIXlt.POSIXlt <- vec_arith.POSIXct.POSIXct
#' @method vec_arith.POSIXct Date
#' @export
vec_arith.POSIXct.Date <- vec_arith.POSIXct.POSIXct
#' @method vec_arith.Date POSIXct
#' @export
vec_arith.Date.POSIXct <- vec_arith.POSIXct.POSIXct
#' @method vec_arith.POSIXlt Date
#' @export
vec_arith.POSIXlt.Date <- vec_arith.POSIXct.POSIXct
#' @method vec_arith.Date POSIXlt
#' @export
vec_arith.Date.POSIXlt <- vec_arith.POSIXct.POSIXct
#' @method vec_arith.POSIXlt POSIXct
#' @export
vec_arith.POSIXlt.POSIXct <- vec_arith.POSIXct.POSIXct
#' @method vec_arith.POSIXct POSIXlt
#' @export
vec_arith.POSIXct.POSIXlt <- vec_arith.POSIXct.POSIXct

#' @method vec_arith.Date integer
#' @export
vec_arith.Date.integer <- function(op, x, y, ...) {
  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.integer.Date <- function(op, x, y, ...) {
#   switch(op,
#     `+` = vec_restore(vec_arith_base(op, x, y), y),
#     stop_incompatible_op(op, x, y)
#   )
# }
#' @method vec_arith.Date double
#' @export
vec_arith.Date.double <- vec_arith.Date.integer
# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.double.Date <- vec_arith.integer.Date
# FIXME: Remove numeric method after custom double dispatch
#' @method vec_arith.numeric Date
#' @export
vec_arith.numeric.Date <- function(op, x, y, ...) {
  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXct integer
#' @export
vec_arith.POSIXct.integer <- vec_arith.Date.integer
# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.integer.POSIXct <- vec_arith.integer.Date
#' @method vec_arith.POSIXct double
#' @export
vec_arith.POSIXct.double <- vec_arith.Date.double
# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.double.POSIXct <- vec_arith.double.Date
# FIXME: Remove numeric method after custom double dispatch
#' @method vec_arith.numeric POSIXct
#' @export
vec_arith.numeric.POSIXct <- vec_arith.numeric.Date
#' @method vec_arith.POSIXlt integer
#' @export
vec_arith.POSIXlt.integer <- function(op, x, y, ...) {
  vec_arith.POSIXct.integer(op, as.POSIXct(x), y, ...)
}
# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.integer.POSIXlt <- function(op, x, y, ...) {
#   vec_arith.integer.POSIXct(op, x, as.POSIXct(y), ...)
# }
#' @method vec_arith.POSIXlt double
#' @export
vec_arith.POSIXlt.double <- function(op, x, y, ...) {
  vec_arith.POSIXct.double(op, as.POSIXct(x), y, ...)
}
# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.double.POSIXlt <- function(op, x, y, ...) {
#   vec_arith.double.POSIXct(op, x, as.POSIXct(y), ...)
# }
# FIXME: Remove numeric method after custom double dispatch
#' @method vec_arith.numeric POSIXlt
#' @export
vec_arith.numeric.POSIXlt <- function(op, x, y, ...) {
  vec_arith.numeric.POSIXct(op, x, as.POSIXct(y), ...)
}

#' @method vec_arith.Date difftime
#' @export
vec_arith.Date.difftime <- function(op, x, y, ...) {
  y <- vec_cast(y, new_duration(units = "days"))

  switch(op,
    `+` = ,
    `-` = vec_restore(vec_arith_base(op, x, lossy_floor(y, x)), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.difftime Date
#' @export
vec_arith.difftime.Date <- function(op, x, y, ...) {
  x <- vec_cast(x, new_duration(units = "days"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, lossy_floor(x, y), y), y),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXct difftime
#' @export
vec_arith.POSIXct.difftime <- function(op, x, y, ...) {
  y <- vec_cast(y, new_duration(units = "secs"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), x),
    `-` = vec_restore(vec_arith_base(op, x, y), x),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.difftime POSIXct
#' @export
vec_arith.difftime.POSIXct <- function(op, x, y, ...) {
  x <- vec_cast(x, new_duration(units = "secs"))

  switch(op,
    `+` = vec_restore(vec_arith_base(op, x, y), y),
    stop_incompatible_op(op, x, y)
  )
}
#' @method vec_arith.POSIXlt difftime
#' @export
vec_arith.POSIXlt.difftime <- function(op, x, y, ...) {
  vec_arith.POSIXct.difftime(op, as.POSIXct(x), y, ...)
}
#' @method vec_arith.difftime POSIXlt
#' @export
vec_arith.difftime.POSIXlt <- function(op, x, y, ...) {
  vec_arith.difftime.POSIXct(op, x, as.POSIXct(y), ...)
}

#' @method vec_arith.difftime difftime
#' @export
vec_arith.difftime.difftime <- function(op, x, y, ...) {
  # Ensure x and y have same units
  args <- vec_cast_common(x, y)
  x <- args[[1L]]
  y <- args[[2L]]

  switch(op,
    `+`   = vec_restore(vec_arith_base(op, x, y), x),
    `-`   = vec_restore(vec_arith_base(op, x, y), x),
    `/`   = vec_arith_base(op, x, y),
    `%/%` = vec_arith_base(op, x, y),
    `%%`  = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.difftime MISSING
#' @export
vec_arith.difftime.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = vec_restore(-vec_data(x), x),
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.difftime integer
#' @export
vec_arith.difftime.integer <- function(op, x, y, ...) {
  vec_restore(vec_arith_base(op, x, y), x)
}
# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.integer.difftime <- function(op, x, y, ...) {
#   switch(op,
#     `/` = stop_incompatible_op(op, x, y),
#     vec_restore(vec_arith_base(op, x, y), y)
#   )
# }
#' @method vec_arith.difftime double
#' @export
vec_arith.difftime.double <- function(op, x, y, ...) {
  vec_restore(vec_arith_base(op, x, y), x)
}
# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.double.difftime <- function(op, x, y, ...) {
#   switch(op,
#     `/` = stop_incompatible_op(op, x, y),
#     vec_restore(vec_arith_base(op, x, y), y)
#   )
# }
# FIXME: Remove numeric method after custom double dispatch
#' @method vec_arith.numeric difftime
#' @export
vec_arith.numeric.difftime <- function(op, x, y, ...) {
  switch(op,
    `/` = stop_incompatible_op(op, x, y),
    vec_restore(vec_arith_base(op, x, y), y)
  )
}

# Helpers -----------------------------------------------------------------

# The tz attribute for POSIXlt can have 3 components
# (time zone name, abbreviated name, abbreviated DST name)
tzone <- function(x) {
  attr(x, "tzone")[[1]] %||% ""
}

tzone_is_local <- function(x) {
  identical(tzone(x), "")
}

tzone_union <- function(x, y) {
  if (tzone_is_local(x)) {
    tzone(y)
  } else {
    tzone(x)
  }
}

units_union <- function(x, y) {
  if (identical(units(x), units(y))) {
    units(x)
  } else {
    "secs"
  }
}

date_validate <- function(x) {
  .Call(vctrs_date_validate, x)
}

datetime_validate <- function(x) {
  .Call(vctrs_datetime_validate, x)
}

# as.character.Date() calls format() which tries to guess a simplified format.
# Supplying a known format is faster and much more memory efficient.
date_as_character <- function(x) {
  format(x, format = "%Y-%m-%d")
}

# `as.POSIXlt.character()` tries multiple formats. Supplying
# a known format is much faster and more memory efficient.
chr_date_as_posixlt <- function(x, tzone) {
  as.POSIXlt.character(x, tz = tzone, format = "%Y-%m-%d")
}

# `as.POSIXct.default()` for characters goes through `as.POSIXlt.character()`
chr_date_as_posixct <- function(x, tzone) {
  out <- chr_date_as_posixlt(x, tzone)
  as.POSIXct.POSIXlt(out, tzone)
}

lossy_floor <- function(x, to, x_arg = "", to_arg = "") {
  x_floor <- floor(x)
  lossy <- x != x_floor
  maybe_lossy_cast(x_floor, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

# Guarantees the presence of a `tzone` attribute
# by going through `as.POSIXlt.POSIXct()`.
# Useful for testing, since we always try to restore a `tzone`.
as_posixlt <- function(x, tz = "") {
  as.POSIXlt(as.POSIXct(x, tz))
}

# Math --------------------------------------------------------------------

#' @export
vec_math.Date <- function(.fn, .x, ...) {
  stop_unsupported(.x, .fn)
}
#' @export
vec_math.POSIXct <- function(.fn, .x, ...) {
  stop_unsupported(.x, .fn)
}
#' @export
vec_math.POSIXlt <- function(.fn, .x, ...) {
  stop_unsupported(.x, .fn)
}
