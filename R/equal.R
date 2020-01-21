#' Equality proxy
#'
#' Returns a proxy object (i.e. an atomic vector or data frame of atomic
#' vectors). For [vctr]s, this determines the behaviour of `==` and
#' `!=` (via [vec_equal()]); [unique()], [duplicated()] (via
#' [vec_unique()] and [vec_duplicate_flg()]); [is.na()] and [anyNA()]
#' (via [vec_equal_na()]).
#'
#' The default method calls [vec_proxy()], as the default underlying
#' vector data should be equal-able in most cases. If your class is
#' not equal-able, provide a `vec_proxy_equal()` method that throws an
#' error.
#'
#' If the proxy for `x` is a data frame, `vec_proxy_equal()` is
#' recursively applied on all columns as well.
#'
#' @param x A vector x.
#' @inheritParams ellipsis::dots_empty
#'
#' @return A 1d atomic vector or a data frame.
#' @keywords internal
#'
#' @export
vec_proxy_equal <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_proxy_equal, x))
  UseMethod("vec_proxy_equal")
}
vec_proxy_equal_dispatch <- function(x, ...) {
  UseMethod("vec_proxy_equal")
}
#' @export
vec_proxy_equal.default <- function(x, ...) {
  vec_proxy(x)
}

#' Test if two vectors are equal
#'
#' `vec_equal_na()` tests a special case: equality with `NA`. It is similar to
#' [is.na] but:
#' * Considers the missing element of a list to be `NULL`.
#' * Considered data frames and records to be missing if every component
#'   is missing.
#' This preserves the invariant that `vec_equal_na(x)` is equal to
#' `vec_equal(x, vec_init(x), na_equal = TRUE)`.
#'
#' @inheritParams vec_compare
#' @return A logical vector the same size as. Will only contain `NA`s if `na_equal` is `FALSE`.
#' @export
#' @examples
#' vec_equal(c(TRUE, FALSE, NA), FALSE)
#' vec_equal(c(TRUE, FALSE, NA), FALSE, na_equal = TRUE)
#' vec_equal_na(c(TRUE, FALSE, NA))
#'
#' vec_equal(5, 1:10)
#' vec_equal("d", letters[1:10])
#'
#' df <- data.frame(x = c(1, 1, 2, 1, NA), y = c(1, 2, 1, NA, NA))
#' vec_equal(df, data.frame(x = 1, y = 2))
#' vec_equal_na(df)
vec_equal <- function(x, y, na_equal = FALSE, .ptype = NULL) {
  vec_assert(na_equal, ptype = logical(), size = 1L)
  args <- vec_recycle_common(x, y)
  args <- vec_cast_common(!!!args, .to = .ptype)
  .Call(vctrs_equal, args[[1]], args[[2]], na_equal)
}

#' @export
#' @rdname vec_equal
vec_equal_na <- function(x) {
  .Call(vctrs_equal_na, x)
}

#' @export
#' @rdname vec_duplicate
vec_duplicate_all <- function(x) {
  .Call(vctrs_duplicate_all, x)
}

obj_equal <- function(x, y) {
  .Call(vctrs_equal_object, x, y)
}
