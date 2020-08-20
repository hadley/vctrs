# proxies -----------------------------------------------------------------

#' Comparison and order proxy
#'
#' @description
#' `vec_proxy_compare()` and `vec_proxy_order()` return proxy objects, i.e.
#' an atomic vector or data frame of atomic vectors.
#'
#' For [`vctrs_vctr`][vctr] objects:
#'
#' - `vec_proxy_compare()` determines the behavior of `<`, `>`, `>=`
#'   and `<=` (via [vec_compare()]); and [min()], [max()], [median()], and
#'   [quantile()].
#'
#' - `vec_proxy_order()` determines the behavior of `order()` and `sort()`
#'   (via `xtfrm()`).
#'
#' @details
#' The default method of `vec_proxy_compare()` assumes that all classes built
#' on top of atomic vectors or records are comparable. Internally the default
#' calls [vec_proxy_equal()]. If your class is not comparable, you will need
#' to provide a `vec_proxy_compare()` method that throws an error.
#'
#' The behavior of `vec_proxy_order()` is identical to `vec_proxy_compare()`,
#' with the exception of lists. Lists are not comparable, as comparing
#' elements of different types is undefined. However, to allow ordering of
#' data frames containing list-columns, the ordering proxy of a list is
#' generated as an integer vector that can be used to order list elements
#' by first appearance.
#'
#' @param x A vector x.
#' @inheritParams ellipsis::dots_empty
#' @return A 1d atomic vector or a data frame.
#'
#' @section Dependencies:
#' - [vec_proxy_equal()] called by default in `vec_proxy_compare()`
#' - [vec_proxy_compare()] called by default in `vec_proxy_order()`
#'
#' @keywords internal
#' @export
#' @examples
#' # Lists are not comparable
#' x <- list(1:2, 1, 1:2, 3)
#' try(vec_compare(x, x))
#'
#' # But lists are orderable by first appearance to allow for
#' # ordering data frames with list-cols
#' df <- new_data_frame(list(x = x))
#' vec_sort(df)
vec_proxy_compare <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_proxy_compare, x))
  UseMethod("vec_proxy_compare")
}
#' @export
vec_proxy_compare.default <- function(x, ...) {
  stop_native_implementation("vec_proxy_compare.default")
}

#' @rdname vec_proxy_compare
#' @export
vec_proxy_order <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_proxy_order, x))
  UseMethod("vec_proxy_order")
}

#' @export
vec_proxy_order.default <- function(x, ...) {
  stop_native_implementation("vec_proxy_order.default")
}

# compare -----------------------------------------------------------------

#' Compare two vectors
#'
#' @section S3 dispatch:
#' `vec_compare()` is not generic for performance; instead it uses
#' [vec_proxy_compare()] to
#'
#' @param x,y Vectors with compatible types and lengths.
#' @param na_equal Should `NA` values be considered equal?
#' @param .ptype Override to optionally specify common type
#' @return An integer vector with values -1 for `x < y`, 0 if `x == y`,
#'    and 1 if `x > y`. If `na_equal` is `FALSE`, the result will be `NA`
#'    if either `x` or `y` is `NA`.
#'
#' @section Dependencies:
#' - [vec_cast_common()] with fallback
#' - [vec_recycle_common()]
#' - [vec_proxy_compare()]
#'
#' @export
#' @examples
#' vec_compare(c(TRUE, FALSE, NA), FALSE)
#' vec_compare(c(TRUE, FALSE, NA), FALSE, na_equal = TRUE)
#'
#' vec_compare(1:10, 5)
#' vec_compare(runif(10), 0.5)
#' vec_compare(letters[1:10], "d")
#'
#' df <- data.frame(x = c(1, 1, 1, 2), y = c(0, 1, 2, 1))
#' vec_compare(df, data.frame(x = 1, y = 1))
vec_compare <- function(x, y, na_equal = FALSE, .ptype = NULL) {
  vec_assert(x)
  vec_assert(y)
  vec_assert(na_equal, ptype = logical(), size = 1L)

  args <- vec_recycle_common(x, y)
  args <- vec_cast_common_params(
    !!!args,
    .to = .ptype,
    .df_fallback = DF_FALLBACK_quiet
  )

  .Call(vctrs_compare, vec_proxy_compare(args[[1]]), vec_proxy_compare(args[[2]]), na_equal)
}


# order/sort --------------------------------------------------------------

#' Order and sort vectors
#'
#' @param x A vector
#' @param direction Direction to sort in. Defaults to `asc`ending.
#' @param na_value Should `NA`s be treated as the largest or smallest values?
#' @return
#' * `vec_order()` an integer vector the same size as `x`.
#' * `vec_sort()` a vector with the same size and type as `x`.
#'
#' @section Dependencies of `vec_order()`:
#' * [vec_proxy_order()]
#'
#' @section Dependencies of `vec_sort()`:
#' * [vec_proxy_order()]
#' * [vec_order()]
#' * [vec_slice()]
#' @export
#' @examples
#' x <- round(c(runif(9), NA), 3)
#' vec_order(x)
#' vec_sort(x)
#' vec_sort(x, "desc")
#'
#' # Can also handle data frames
#' df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
#' vec_order(df)
#' vec_sort(df)
#' vec_sort(df, "desc")
vec_order <- function(x,
                      direction = c("asc", "desc"),
                      na_value = c("largest", "smallest")) {
  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

  order_proxy(vec_proxy_order(x), direction = direction, na_value = na_value)
}

#' @export
#' @rdname vec_order
vec_sort <- function(x,
                     direction = c("asc", "desc"),
                     na_value = c("largest", "smallest")) {
  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

  idx <- vec_order(x, direction = direction, na_value = na_value)
  vec_slice(x, idx)
}

order_proxy <- function(proxy, direction = "asc", na_value = "largest") {
  decreasing <- !identical(direction, "asc")
  na.last <- identical(na_value, "largest")
  if (decreasing) {
    na.last <- !na.last
  }

  if (is.data.frame(proxy)) {
    # Work around type-instability in `base::order()`
    if (vec_size(proxy) == 0L) {
      return(integer(0L))
    }
    args <- map(unstructure(proxy), function(.x) {
      if (is.data.frame(.x)) {
        .x <- order(vec_order(.x, direction = direction, na_value = na_value))
      }
      .x
    })
    exec("order", !!!args, decreasing = decreasing, na.last = na.last)
  } else if (is_character(proxy) || is_logical(proxy) || is_integer(proxy) || is_double(proxy)) {
    if (is.object(proxy)) {
      proxy <- unstructure(proxy)
    }
    order(proxy, decreasing = decreasing, na.last = na.last)
  } else {
    abort("Invalid type returned by `vec_proxy_compare()`.")
  }
}

# Helpers -----------------------------------------------------------------

# Used for testing
cmp <- function(x, y) (x > y) - (x < y)
