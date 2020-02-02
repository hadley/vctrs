#' Is a vector empty
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("defunct")}
#'
#' This function is defunct, please use [vec_is_empty()].
#'
#' @param x An object.
#'
#' @keywords internal
#' @export
vec_empty <- function(x) {
  stop_defunct(paste_line(
    "`vec_empty()` is defunct as of vctrs 0.2.0.",
    "Please use `vec_is_empty()` instead."
  ))
}

#' Deprecated type functions
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("deprecated")}
#'
#' These functions have been renamed:
#'
#' * `vec_type()` => [vec_ptype()]
#' * `vec_type2()` => [vec_ptype2()]
#' * `vec_type_common()` => [vec_ptype_common()]
#'
#' @param x,y,...,.ptype Arguments for deprecated functions.
#'
#' @keywords internal
#' @export
vec_type <- function(x) {
  warn_deprecated(c("`vec_type()` has been renamed to `vec_ptype()`."))
  vec_ptype(x)
}
#' @rdname vec_type
#' @export
vec_type_common <- function(..., .ptype = NULL) {
  warn_deprecated(c("`vec_type_common()` has been renamed to `vec_ptype_common()`."))
  vec_ptype_common(..., .ptype = .ptype)
}
#' @rdname vec_type
#' @export
vec_type2 <- function(x, y, ...) {
  warn_deprecated(c("`vec_type2()` has been renamed to `vec_ptype2()`."))
  vec_ptype2(x, y, ...)
}

#' Convert to an index vector
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("soft-deprecated")}
#'
#' `vec_as_index()` has been renamed to [vec_as_location()] and is
#' soft-deprecated as of vctrs 0.2.2.
#'
#' @inheritParams vec_as_location
#'
#' @keywords internal
#' @export
vec_as_index <- function(i, n, names = NULL) {
  signal_soft_deprecated(paste_line(
    "`vec_as_index()` is deprecated as of vctrs 0.2.2.",
    "Please use `vec_as_location() instead.`"
  ))
  n <- vec_coercible_cast(n, integer())
  vec_assert(n, integer(), 1L)
  i <- vec_as_subscript(i)

  .Call(
    vctrs_as_location,
    i = i,
    n = n,
    names = names,
    loc_negative = "invert",
    loc_oob = "error",
    missing = "propagate",
    arg = NULL
  )
}

#' Detect if any values are duplicates
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("soft-deprecated")}
#'
#' `vec_duplicate_any()` has been renamed to [vec_any_duplicate()] and is
#' soft-deprecated as of vctrs 0.3.0.
#'
#' @inheritParams vec_any_duplicate
#'
#' @keywords internal
#' @export
vec_duplicate_any <- function(x) {
  signal_soft_deprecated(paste_line(
    "`vec_duplicate_any()` is deprecated as of vctrs 0.3.0.",
    "Please use `vec_any_duplicate()` instead."
  ))

  vec_any_duplicate(x)
}
