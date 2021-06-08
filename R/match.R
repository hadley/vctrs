#' Find observations matching specified conditions
#'
#' @description
#' `vec_matches()` is a more flexible version of [vec_match()] used to determine
#' locations where each observation of `needles` matches one or multiple
#' observations in `haystack`. Unlike `vec_match()`, `vec_matches()` returns all
#' matches by default, and can match on binary conditions other than equality,
#' such as `>`, `>=`, `<`, and `<=`.
#'
#' @details
#' [vec_match()] is identical to (but often slightly faster than)
#' `vec_matches(needles, haystack, condition = "==", multiple = "first",
#' nan_distinct = TRUE)`.
#'
#' `vec_matches()` is extremely similar to a SQL left join between `needles` and
#' `haystack`. Using `condition` is analogous to specifying a SQL ON statement,
#' and `condition = NULL` is identical to specifying a left join without an ON
#' statement.
#'
#' When `na_equal = TRUE`, missing values are allowed to exactly match other
#' missing values, but will not match any other values. This implies that
#' `NA >= NA` is a match, but `NA > NA` is not. Similarly, both `NA >= 1` and
#' `1 >= NA` result in an unmatched needle.
#'
#' Be very careful when specifying match `condition`s. If a condition is
#' mis-specified, it is very easy to accidentally generate an exponentially
#' large number of matches.
#'
#' @section Dependencies of `vec_matches()`:
#' * [vec_rank()]
#' * [vec_slice()]
#' * [vec_c()]
#' * [vec_detect_complete()]
#'
#' @inheritParams ellipsis::dots_empty
#' @inheritParams vec_order_radix
#'
#' @param needles,haystack Vectors used for matching.
#'   - `needles` represents the vector to search for.
#'   - `haystack` represents the vector to search in.
#'
#'   Prior to comparison, `needles` and `haystack` are coerced to the same type.
#'
#' @param condition Condition used to match on.
#'   - One of `"=="`, `">"`, `">="`, `"<"`, or `"<="` controlling how `needles`
#'     should be compared against `haystack` to determine a successful match.
#'   - For data frames, a length `1` or `ncol(needles)` character vector
#'     containing only the above options, specifying how matching is determined
#'     for each column.
#'   - Alternatively, specify `NULL` to perform a cross match. In this case,
#'     every observation of `needles` matches against every observation of
#'     `haystack`, regardless of the specific values.
#'
#' @param na_equal Treatment of missing values.
#'   - If `TRUE`, missing values in `needles` are allowed to match against
#'     missing values in `haystack`.
#'   - If `FALSE`, missing values in `needles` _propagate_, and are represented
#'     by `NA` in the `haystack` column of the result (regardless of the value
#'     of `no_match`). For data frame input, if _any_ column in a particular row
#'     contains a missing value, then that missing value will propagate.
#'
#' @param no_match Handling of `needles` without a match.
#'   - If a single integer is provided, this represents the value returned in
#'     the `haystack` column for observations of `needles` that have zero
#'     matches. The default represents an unmatched needle with `NA`.
#'   - If `"error"`, an error will be thrown if any `needles` have zero matches.
#'
#' @param multiple Handling of `needles` with multiple matches. For each needle:
#'   - `"all"` returns every match detected in `haystack`.
#'   - `"first"` returns the first match detected in `haystack` (this is similar
#'     to how [vec_match()] works).
#'   - `"last"` returns the last match detected in `haystack`.
#'   - `"warning"` throws a warning if multiple matches are detected, but
#'     otherwise falls back to `"all"`.
#'   - `"error"` throws an error if multiple matches are detected.
#'
#' @param needles_arg,haystack_arg Argument tags for `needles` and `haystack`
#'   used in error messages.
#'
#' @return A two column data frame containing the locations of the matches.
#'   - `needles` is an integer vector containing the location of
#'     the needle currently being matched.
#'   - `haystack` is an integer vector containing the location of the
#'     corresponding match in the haystack for the current needle.
#'
#' @examples
#' x <- c(1, 2, NA, 3, NaN)
#' y <- c(2, 1, 4, NA, 1, 2, NaN)
#'
#' # By default, for each element of `x`, all matching locations in `y` are
#' # returned
#' matches <- vec_matches(x, y)
#' matches
#'
#' # The result can be used to slice the inputs to align them
#' data_frame(
#'   x = vec_slice(x, matches$needles),
#'   y = vec_slice(y, matches$haystack)
#' )
#'
#' # If multiple matches are present, control which is returned with `multiple`
#' vec_matches(x, y, multiple = "first")
#' vec_matches(x, y, multiple = "last")
#' try(vec_matches(x, y, multiple = "error"))
#'
#' # By default, NA is allowed to match other NA values, and NA is treated
#' # as being identical to NaN. Using `nan_distinct = TRUE` treats NA and NaN as
#' # different values, so NA can only match NA, and NaN can only match NaN.
#' vec_matches(x, y, nan_distinct = TRUE)
#'
#' # If you don't want any missing values in `needles` to match missing values
#' # in `haystack`, set `na_equal = FALSE` to propagate missing values in
#' # `needles` as NA in the result
#' vec_matches(x, y, na_equal = FALSE)
#'
#' # `no_match` allows you to specify the returned value for a needle with
#' # zero matches. Note that this is different from a propagated missing value,
#' # so specifying `no_match` allows you to differentiate between propagated
#' # missing values and unmatched values.
#' vec_matches(x, y, na_equal = FALSE, no_match = 0L)
#'
#' # If you want to require that every `needle` has at least 1 match, set
#' # `no_match` to `"error"`:
#' try(vec_matches(x, y, na_equal = FALSE, no_match = "error"))
#'
#' # By default, `vec_matches()` detects equality between `needles` and
#' # `haystack`. Using `condition`, you can detect where an inequality holds
#' # true instead. For example, to find every location where `x[[i]] >= y`:
#' matches <- vec_matches(x, y, condition = ">=")
#'
#' data_frame(
#'   x = vec_slice(x, matches$needles),
#'   y = vec_slice(y, matches$haystack)
#' )
#'
#' # You can also specify `condition = NULL` to generate a cross match where
#' # every observation in `needles` matches all observations in `haystack`.
#' # This ignores the actual values, and depends only on the size of the inputs.
#' matches <- vec_matches(x, y, condition = NULL)
#' vec_size(x) * vec_size(y)
#' nrow(matches)
#' head(matches, n = 10)
#'
#' # You can also use data frames for `needles` and `haystack`. The
#' # `condition` will be recycled to the number of columns in `needles`, or
#' # you can specify varying conditions per column. In this example, we take
#' # a vector of date `values` and find all locations where each value is
#' # between lower and upper bounds specified by the `haystack`.
#' values <- as.Date("2019-01-01") + 0:9
#' needles <- data_frame(lower = values, upper = values)
#'
#' set.seed(123)
#' lower <- as.Date("2019-01-01") + sample(10, 10, replace = TRUE)
#' upper <- lower + sample(3, 10, replace = TRUE)
#' haystack <- data_frame(lower = lower, upper = upper)
#'
#' # (values >= lower) & (values <= upper)
#' matches <- vec_matches(needles, haystack, condition = c(">=", "<="))
#'
#' data_frame(
#'   lower = vec_slice(lower, matches$haystack),
#'   value = vec_slice(values, matches$needle),
#'   upper = vec_slice(upper, matches$haystack)
#' )
#' @noRd
vec_matches <- function(needles,
                        haystack,
                        ...,
                        condition = "==",
                        na_equal = TRUE,
                        no_match = NA_integer_,
                        multiple = "all",
                        nan_distinct = FALSE,
                        chr_transform = NULL,
                        needles_arg = "",
                        haystack_arg = "") {
  if (!missing(...)) {
    check_dots_empty()
  }

  .Call(
    vctrs_matches,
    needles,
    haystack,
    condition,
    na_equal,
    no_match,
    multiple,
    nan_distinct,
    chr_transform,
    needles_arg,
    haystack_arg
  )
}

# ------------------------------------------------------------------------------

stop_matches <- function(class = NULL, ...) {
  stop_vctrs(
    class = c(class, "vctrs_error_matches"),
    ...
  )
}

# ------------------------------------------------------------------------------

stop_matches_nothing <- function(i, needles_arg, haystack_arg) {
  stop_matches(
    class = "vctrs_error_matches_nothing",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg
  )
}

#' @export
cnd_header.vctrs_error_matches_nothing <- function(cnd, ...) {
  if (nzchar(cnd$needles_arg)) {
    needles_name <- glue::glue(" of `{cnd$needles_arg}` ")
  } else {
    needles_name <- " "
  }

  if (nzchar(cnd$haystack_arg)) {
    haystack_name <- glue::glue(" in `{cnd$haystack_arg}`")
  } else {
    haystack_name <- ""
  }

  glue::glue("Each element{needles_name}must have a match{haystack_name}.")
}

#' @export
cnd_body.vctrs_error_matches_nothing <- function(cnd, ...) {
  bullet <- glue::glue("The element at location {cnd$i} does not have a match.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

# ------------------------------------------------------------------------------

stop_matches_multiple <- function(i, needles_arg, haystack_arg) {
  stop_matches(
    class = "vctrs_error_matches_multiple",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg
  )
}

#' @export
cnd_header.vctrs_error_matches_multiple <- function(cnd, ...) {
  if (nzchar(cnd$needles_arg)) {
    needles_name <- glue::glue(" of `{cnd$needles_arg}` ")
  } else {
    needles_name <- " "
  }

  if (nzchar(cnd$haystack_arg)) {
    haystack_name <- glue::glue(" from `{cnd$haystack_arg}`")
  } else {
    haystack_name <- ""
  }

  glue::glue("Each element{needles_name}can match at most 1 observation{haystack_name}.")
}

#' @export
cnd_body.vctrs_error_matches_multiple <- function(cnd, ...) {
  bullet <- glue::glue("The element at location {cnd$i} has multiple matches.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}
