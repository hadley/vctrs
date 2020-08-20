#' Partially specify a factor
#'
#' This special class can be passed as a `ptype` in order to specify that the
#' result should be a factor that contains at least the specified levels.
#'
#' @inheritParams new_factor
#' @keywords internal
#' @export
#' @examples
#' # Assert that `x` is a factor
#' vec_assert(factor("x"), partial_factor())
#'
#' # Testing with `factor()` is too strict,
#' # because it tries to match the levels exactly
#' # rather than learning them from the data.
#' try(vec_assert(factor("x"), factor()))
#'
#' # You can also enforce a minimum set of levels
#' try(vec_assert(factor("x"), partial_factor("y")))
#'
#' vec_assert(factor(c("x", "y")), partial_factor("y"))
#'
#' pf <- partial_factor(levels = c("x", "y"))
#' pf
#'
#' vec_ptype_common(factor("v"), factor("w"), .ptype = pf)
#'
partial_factor <- function(levels = character()) {
  partial <- new_factor(levels = levels)
  new_partial_factor(partial)
}

new_partial_factor <- function(partial = factor(), learned = factor()) {
  stopifnot(
    is.factor(partial),
    is.factor(learned)
  )

  # Fails if `learned` is not compatible with `partial`
  vec_ptype2(partial, learned)

  new_partial(
    partial = partial,
    learned = learned,
    class = "vctrs_partial_factor"
  )
}

#' @export
vec_ptype_full.vctrs_partial_factor <- function(x, ...) {
  empty <- ""

  levels <- map(unclass(x), levels)
  hashes <- map_chr(levels, hash_label)

  needs_indent <- hashes != empty
  hashes[needs_indent] <- map_chr(hashes[needs_indent], function(x) paste0("  ", x))

  source <- rep_named(names(hashes), empty)
  if (hashes["partial"] != empty) {
    source["partial"] <- " {partial}"
  }

  details <- paste0(hashes, source)
  details <- details[details != empty]

  paste0(
    "partial_factor<\n",
    paste0(details, collapse = "\n"),
    "\n>"
  )
}

#' @export
vec_ptype_abbr.vctrs_partial_factor <- function(x, ...) {
  "prtl_fctr"
}

#' @method vec_ptype2 vctrs_partial_factor
#' @export
vec_ptype2.vctrs_partial_factor <- function(x, y, ...) {
  UseMethod("vec_ptype2.vctrs_partial_factor")
}

#' @method vec_ptype2.vctrs_partial_factor vctrs_partial_factor
#' @export
vec_ptype2.vctrs_partial_factor.vctrs_partial_factor <- function(x, y, ...) {
  partial <- vec_ptype2(x$partial, y$partial)
  learned <- vec_ptype2(x$learned, y$learned)
  new_partial_factor(partial, learned)
}

#' @method vec_ptype2.vctrs_partial_factor factor
#' @export
vec_ptype2.vctrs_partial_factor.factor <- function(x, y, ...) {
  new_partial_factor(x$partial, vec_ptype2(x$learned, y))
}

#' @method vec_ptype2.factor vctrs_partial_factor
#' @export
vec_ptype2.factor.vctrs_partial_factor <- function(x, y, ...) {
  new_partial_factor(y$partial, vec_ptype2(y$learned, x))
}

#' @export
vec_ptype_finalise.vctrs_partial_factor <- function(x, ...) {
  vec_ptype2(x$learned, x$partial)
}
