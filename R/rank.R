#' Rank vectors
#'
#' `vec_rank()` computes the sample ranks of a vector. For data frames, ranks
#' are computed along the rows, using all columns after the first to break
#' ties.
#'
#' @details
#' Unlike [base::rank()], when `na_propagate = FALSE` all `NA` values are
#' given the same rank, rather than an increasing sequence of ranks. When
#' `nan_distinct = FALSE`, `NaN` values are given the same rank as `NA`,
#' otherwise they are given a rank that differentiates them from `NA`.
#'
#' Like [vec_order()], ordering is done in the C-locale. This can affect
#' the ranks of character vectors, especially regarding how uppercase and
#' lowercase letters are ranked. See the documentation of [vec_order()]
#' for more information.
#'
#' @inheritParams vec_order
#' @inheritParams ellipsis::dots_empty
#'
#' @param ties Treatment of duplicate values.
#'   - `"min"`: Use the current rank for all duplicates. The next non-duplicate
#'   value will have a rank incremented by the number of duplicates present.
#'
#'   - `"max"`: Use the current rank `+ n_duplicates - 1` for all duplicates.
#'   The next non-duplicate value will have a rank incremented by the number of
#'   duplicates present.
#'
#'   - `"sequential"`: Use an increasing sequence of ranks starting at the
#'   current rank, applied to duplicates in order of appearance.
#'
#'   - `"dense"`: Use the current rank for all duplicates. The next
#'   non-duplicate value will have a rank incremented by `1`, effectively
#'   removing any gaps in the ranking.
#'
#' @param na_propagate A single logical specifying whether or not missing
#'   values should be propagated. If `TRUE`, all missing values are given
#'   the rank `NA`.
#'
#' @section Dependencies of `vec_rank()`:
#'
#' - `vec_order()`
#'
#' - `vec_slice()`
#'
#' @examples
#' x <- c(5L, 6L, 3L, 3L, 5L, 3L)
#'
#' vec_rank(x, ties = "min")
#' vec_rank(x, ties = "max")
#'
#' # Sequential ranks use an increasing sequence for duplicates
#' vec_rank(x, ties = "sequential")
#'
#' # Dense ranks remove gaps between distinct values,
#' # even if there are duplicates
#' vec_rank(x, ties = "dense")
#'
#' y <- c(NA, x, NA, NaN)
#'
#' # Missing values match other missing values
#' vec_rank(y, na_value = "largest")
#' vec_rank(y, na_value = "smallest")
#'
#' # NaN can be ranked separately from NA if required
#' vec_rank(y, nan_distinct = TRUE)
#'
#' # Rank in descending order. Since missing values are the largest value,
#' # they are given a rank of `1` when ranking in descending order.
#' vec_rank(y, direction = "desc", na_value = "largest")
#'
#' # Can also rank data frames, using columns after the first to break ties
#' z <- c(2L, 3L, 4L, 4L, 5L, 2L)
#' df <- data_frame(x = x, z = z)
#' df
#'
#' vec_rank(df)
#' @noRd
vec_rank <- function(x,
                     ...,
                     ties = c("min", "max", "sequential", "dense"),
                     na_propagate = FALSE,
                     direction = "asc",
                     na_value = "largest",
                     nan_distinct = FALSE,
                     chr_transform = NULL) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }

  ties <- arg_match0(ties, c("min", "max", "sequential", "dense"), "ties")

  .Call(
    vctrs_rank,
    x,
    ties,
    na_propagate,
    direction,
    na_value,
    nan_distinct,
    chr_transform
  )
}
