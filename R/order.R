#' Order and sort vectors
#'
#' @description
#' `vec_order()` computes the order of `x`. For data frames, the order is
#' computed along the rows by computing the order of the first column and
#' using subsequent columns to break ties.
#'
#' `vec_sort()` sorts `x`. It is equivalent to `vec_slice(x, vec_order(x))`.
#'
#' @inheritParams ellipsis::dots_empty
#'
#' @param x A vector
#' @param direction Direction to sort in.
#'   - A single `"asc"` or `"desc"` for ascending or descending order
#'     respectively.
#'   - For data frames, a length `1` or `ncol(x)` character vector containing
#'     only `"asc"` or `"desc"`, specifying the direction for each column.
#' @param na_value Treatment of missing values.
#'   - A single `"largest"` or `"smallest"` for treating missing values as the
#'     largest or smallest values respectively.
#'   - For data frames, a length `1` or `ncol(x)` character vector containing
#'     only `"largest"` or `"smallest"`, specifying how missing values should
#'     be treated in each column.
#' @param nan_distinct A single logical specifying whether or not `NaN` should
#'   be considered distinct from `NA` for double and complex vectors. If `TRUE`,
#'   `NaN` will always be ordered between `NA` and non-missing numbers.
#' @param chr_transform Transformation of character vectors for sorting in
#'   alternate locales.
#'   - If `NULL`, no transformation is done.
#'   - Otherwise, this must be a function of one argument. The function will be
#'     invoked with `x`, if it is a character vector, after it has been
#'     translated to UTF-8, and should return a character vector with the same
#'     length as `x`. The result should sort as expected in the C-locale,
#'     regardless of encoding.
#'   - For data frames, `chr_transform` will be applied to all character
#'     columns.
#'
#'   Common transformation functions include: `tolower()` for case-insensitive
#'   ordering and `stringi::str_sort_key()` for locale-aware ordering.
#'
#' @return
#' * `vec_order()` an integer vector the same size as `x`.
#' * `vec_sort()` a vector with the same size and type as `x`.
#'
#' @section Differences with `order()`:
#'
#' Unlike the `na.last` argument of `order()` which decides the positions of
#' missing values irrespective of the `decreasing` argument, the `na_value`
#' argument of `vec_order()` interacts with `direction`. If missing values
#' are considered the largest value, they will appear last in ascending order,
#' and first in descending order.
#'
#' Character vectors are ordered in the C-locale. This is different from
#' `base::order()`, which respects `base::Sys.setlocale()`. Sorting in a
#' consistent locale can produce more reproducible results between different
#' sessions and platforms, however, the results of sorting in the C-locale
#' can be surprising. For example, capital letters sort before lower case
#' letters. Sorting `c("b", "C", "a")` with `vec_sort()` will return
#' `c("C", "a", "b")`, but with `base::order()` will return `c("a", "b", "C")`
#' unless `base::order(method = "radix")` is explicitly set, which also uses
#' the C-locale. While sorting with the C-locale can be useful for
#' algorithmic efficiency, in many real world uses it can be the cause of
#' data analysis mistakes. To balance these trade-offs, you can supply a
#' `chr_transform` to transform character vectors into an alternative
#' representation that orders in the C-locale in a less surprising way. For
#' example, providing [base::tolower()] as a transform will order the original
#' vector in a case-insensitive manner. Locale-aware ordering can be achieved
#' by providing `stringi::stri_sort_key()` as a transform, setting the
#' collation options as appropriate for your locale.
#'
#' Character vectors are always translated to UTF-8 before ordering, and before
#' any transform is applied by `chr_transform`.
#'
#' @section Dependencies of `vec_order()`:
#' * [vec_proxy_order()]
#'
#' @section Dependencies of `vec_sort()`:
#' * [vec_order()]
#' * [vec_slice()]
#'
#' @export
#' @examples
#' x <- round(sample(runif(5), 9, replace = TRUE), 3)
#' x <- c(x, NA)
#'
#' vec_order(x)
#' vec_sort(x)
#' vec_sort(x, direction = "desc")
#'
#' # Can also handle data frames
#' df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
#' vec_order(df)
#' vec_sort(df)
#' vec_sort(df, direction = "desc")
#'
#' # For data frames, `direction` and `na_value` are allowed to be vectors
#' # with length equal to the number of columns in the data frame
#' vec_sort(
#'   df,
#'   direction = c("desc", "asc"),
#'   na_value = c("largest", "smallest")
#' )
#'
#' # Character vectors are ordered in the C locale, which orders capital letters
#' # below lowercase ones
#' y <- c("B", "A", "a")
#' vec_sort(y)
#'
#' # To order in a case-insensitive manner, provide a `chr_transform` that
#' # transforms the strings to all lowercase
#' vec_sort(y, chr_transform = tolower)
vec_order <- function(x,
                      ...,
                      direction = "asc",
                      na_value = "largest",
                      nan_distinct = FALSE,
                      chr_transform = NULL) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  .Call(vctrs_order, x, direction, na_value, nan_distinct, chr_transform)
}

#' @export
#' @rdname vec_order
vec_sort <- function(x,
                     ...,
                     direction = "asc",
                     na_value = "largest",
                     nan_distinct = FALSE,
                     chr_transform = NULL) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  idx <- vec_order(
    x = x,
    direction = direction,
    na_value = na_value,
    nan_distinct = nan_distinct,
    chr_transform = chr_transform
  )

  vec_slice(x, idx)
}

#' Identify ordered groups
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `vec_order_locs()` returns a data frame containing a `key` column with
#' sorted unique groups, and a `loc` column with the locations of each
#' group in `x`. It is similar to [vec_group_loc()], except the groups are
#' returned sorted rather than by first appearance.
#'
#' @inheritParams vec_order
#'
#' @return
#' A two column data frame with size equal to `vec_size(vec_unique(x))`.
#'   * A `key` column of type `vec_ptype(x)`.
#'   * A `loc` column of type list, with elements of type integer.
#'
#' @section Dependencies of `vec_order_locs()`:
#' * [vec_proxy_order()]
#'
#' @examples
#' df <- data.frame(
#'   g = sample(2, 10, replace = TRUE),
#'   x = c(NA, sample(5, 9, replace = TRUE))
#' )
#'
#' # `vec_order_locs()` is similar to `vec_group_loc()`, except keys are
#' # returned ordered rather than by first appearance.
#' vec_order_locs(df)
#'
#' vec_group_loc(df)
#' @noRd
vec_order_locs <- function(x,
                           ...,
                           direction = "asc",
                           na_value = "largest",
                           nan_distinct = FALSE,
                           chr_transform = NULL) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  .Call(vctrs_order_locs, x, direction, na_value, nan_distinct, chr_transform)
}

vec_order_info <- function(x,
                           ...,
                           direction = "asc",
                           na_value = "largest",
                           nan_distinct = FALSE,
                           chr_transform = NULL,
                           chr_ordered = TRUE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  .Call(vctrs_order_info, x, direction, na_value, nan_distinct, chr_transform, chr_ordered)
}

# ------------------------------------------------------------------------------

#' Order vectors with base compatibility
#'
#' @description
#' `vec_order_base()` orders vectors using [base::order()], but can handle
#' more complex types, like data frames and [`vctrs_vctr`][vctr] objects, using
#' vctrs principles.
#'
#' `vec_order_base()` is mainly provided for backwards compatibility with vctrs
#' <= 0.3.7. New code should instead use [vec_order()], which has more
#' capabilities. The main difference between the two is that `vec_order()`
#' orders character vectors in the C locale (which is highly performant), while
#' `vec_order_base()` respects the system locale.
#'
#' @param x A vector
#' @param direction Direction to sort in. Defaults to `asc`ending.
#' @param na_value Should `NA`s be treated as the largest or smallest values?
#' @return An integer vector the same size as `x`.
#'
#' @section Differences with `order()`:
#' Unlike the `na.last` argument of `order()` which decides the positions of
#' missing values irrespective of the `decreasing` argument, the `na_value`
#' argument of `vec_order_base()` interacts with `direction`. If missing values
#' are considered the largest value, they will appear last in ascending order,
#' and first in descending order.
#'
#' @section Dependencies of `vec_order_base()`:
#' * [vec_proxy_order()]
#'
#' @export
#' @keywords internal
#' @examples
#' x <- round(c(runif(9), NA), 3)
#' vec_order_base(x)
#' vec_order_base(x, "desc")
#'
#' # Can also handle data frames
#' df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
#' vec_order_base(df)
#' vec_order_base(df, "desc")
#'
#' # Missing values interpreted as largest values are last when
#' # in increasing order:
#' vec_order_base(c(1, NA), na_value = "largest", direction = "asc")
#' vec_order_base(c(1, NA), na_value = "largest", direction = "desc")
vec_order_base <- function(x,
                           direction = c("asc", "desc"),
                           na_value = c("largest", "smallest")) {
  direction <- arg_match0(direction, c("asc", "desc"))
  na_value <- arg_match0(na_value, c("largest", "smallest"))

  decreasing <- !identical(direction, "asc")
  na.last <- identical(na_value, "largest")
  if (decreasing) {
    na.last <- !na.last
  }

  proxy <- vec_proxy_order(x)

  if (is.data.frame(proxy)) {
    # Work around type-instability in `base::order()`
    if (vec_size(proxy) == 0L) {
      return(integer(0L))
    }
    args <- map(unstructure(proxy), function(.x) {
      if (is.data.frame(.x)) {
        .x <- order(vec_order_base(.x, direction = direction, na_value = na_value))
      }
      .x
    })
    exec("order", !!!args, decreasing = decreasing, na.last = na.last)
  } else if (is_character(proxy) || is_logical(proxy) || is_integer(proxy) || is_double(proxy) || is.complex(proxy)) {
    if (is.object(proxy)) {
      proxy <- unstructure(proxy)
    }
    order(proxy, decreasing = decreasing, na.last = na.last)
  } else {
    abort("Invalid type returned by `vec_proxy_order()`.")
  }
}
