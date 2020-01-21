#' Count unique values in a vector
#'
#' Count the number of unique values in a vector. `vec_count()` has two
#' important differences to `table()`: it returns a data frame, and when
#' given multiple inputs (as a data frame), it only counts combinations that
#' appear in the input.
#'
#' @param x A vector (including a data frame).
#' @param sort One of "count", "key", "location", or "none".
#'  * "count", the default, puts most frequent values at top
#'  * "key", orders by the output key column (i.e. unique values of `x`)
#'  * "location", orders by location where key first seen. This is useful
#'     if you want to match the counts up to other unique/duplicated functions.
#'  * "none", leaves unordered.
#' @return A data frame with columns `key` (same type as `x`) and
#'   `count` (an integer vector).
#' @export
#' @examples
#' vec_count(mtcars$vs)
#' vec_count(iris$Species)
#'
#' # If you count a data frame you'll get a data frame
#' # column in the output
#' str(vec_count(mtcars[c("vs", "am")]))
#'
#' # Sorting ---------------------------------------
#'
#' x <- letters[rpois(100, 6)]
#' # default is to sort by frequency
#' vec_count(x)
#'
#' # by can sort by key
#' vec_count(x, sort = "key")
#'
#' # or location of first value
#' vec_count(x, sort = "location")
#' head(x)
#'
#' # or not at all
#' vec_count(x, sort = "none")
vec_count <- function(x, sort = c("count", "key", "location", "none")) {
  sort <- match.arg(sort)

  # Returns key-value pair giving index of first occurrence value and count
  kv <- .Call(vctrs_count, vec_proxy(x))

  # rep_along() to support zero-length vectors!
  df <- data_frame(key = rep_along(kv$val, NA), count = kv$val)
  df$key <- vec_slice(x, kv$key) # might be a dataframe

  if (sort == "none") {
    return(df)
  }

  idx <- switch(sort,
    location = order(kv$key),
    key = vec_order(df$key),
    count = order(-kv$val)
  )

  df <- vec_slice(df, idx)
  reset_rownames(df)
}

reset_rownames <- function(x) {
  rownames(x) <- NULL

  is_df <- map_lgl(x, is.data.frame)
  x[is_df] <- lapply(x[is_df], reset_rownames)

  x
}

# Duplicates --------------------------------------------------------------

#' Find duplicated values
#'
#' * `vec_duplicate_any()`: detects the presence of duplicated values,
#'   similar to [anyDuplicated()].
#' * `vec_duplicate_all()`: detects if all values are equivalent.
#' * `vec_duplicate_flg()`: returns a vector of logical flags describing if each
#'   element of the vector is a duplicate.
#' * `vec_duplicate_loc()`: returns an integer vector giving the location of
#'   the duplicates of each value.
#'
#' @section Missing values:
#' In most cases, missing values are not considered to be equal, i.e.
#' `NA == NA` is not `TRUE`. This behaviour would be unappealing here,
#' so these functions consider all `NAs` to be equal. (Similarly,
#' all `NaN` are also considered to be equal.)
#'
#' @param x A vector (including a data frame).
#' @param first A logical indicating if the first occurrence of a value should
#'   be considered a duplicate.
#' @return
#'   * `vec_duplicate_any()`: a logical vector of length 1.
#'   * `vec_duplicate_all()`: a logical vector of length 1.
#'   * `vec_duplicate_flg()`: a logical vector the same size as `x`.
#'   * `vec_duplicate_loc()`: an integer vector of the locations of the
#'     duplicates of each value.
#' @seealso [vec_unique()] for functions that work with the dual of duplicated
#'   values: unique values.
#' @name vec_duplicate
#' @examples
#' vec_duplicate_any(1:10)
#' vec_duplicate_any(c(1, 1:10))
#'
#' vec_duplicate_all(c(1, 1))
#' vec_duplicate_all(c(1, 2))
#' vec_duplicate_all(c(NA, NA))
#'
#' x <- c(10, 10, 20, 30, 30, 40)
#'
#' # The default of `vec_duplicate_flg()` is similar to `duplicated()`. Both
#' # don't consider the first occurrence to be a duplicate.
#' vec_duplicate_flg(x)
#' duplicated(x)
#'
#' # Use `first` to control whether or not the first occurrence should be
#' # considered a duplicate
#' vec_duplicate_flg(x, first = TRUE)
#'
#' # `vec_duplicate_loc()` returns the locations corresponding to
#' # the flags that `vec_duplicate_flg()` returns
#' vec_duplicate_loc(x)
#' which(vec_duplicate_flg(x))
#'
#' # When `first = FALSE`, it can be considered the
#' # complement of `vec_unique_loc()`
#' vec_unique_loc(x)
NULL

#' @rdname vec_duplicate
#' @export
vec_duplicate_any <- function(x) {
  .Call(vctrs_duplicated_any, x)
}

#' @rdname vec_duplicate
#' @export
vec_duplicate_flg <- function(x, first = FALSE) {
  vec_assert(first, ptype = logical(), size = 1L)
  .Call(vctrs_duplicate_flg, x, first)
}

#' @rdname vec_duplicate
#' @export
vec_duplicate_loc <- function(x, first = FALSE) {
  vec_assert(first, ptype = logical(), size = 1L)
  .Call(vctrs_duplicate_loc, x, first)
}

# Unique values -----------------------------------------------------------

#' Find and count unique values
#'
#' * `vec_unique()`: the unique values. Equivalent to [unique()].
#' * `vec_unique_loc()`: the locations of the unique values.
#' * `vec_unique_count()`: the number of unique values.
#'
#' @inherit vec_duplicate sections
#' @param x A vector (including a data frame).
#' @return
#' * `vec_unique()`: a vector the same type as `x` containing only unique
#'    values.
#' * `vec_unique_loc()`: an integer vector, giving locations of unique values.
#' * `vec_unique_count()`: an integer vector of length 1, giving the
#'   number of unique values.
#' @seealso [vec_duplicate] for functions that work with the dual of
#'   unique values: duplicated values.
#' @export
#' @examples
#' x <- rpois(100, 8)
#' vec_unique(x)
#' vec_unique_loc(x)
#' vec_unique_count(x)
#'
#' # `vec_unique()` returns values in the order that encounters them
#' # use sort = "location" to match to the result of `vec_count()`
#' head(vec_unique(x))
#' head(vec_count(x, sort = "location"))
#'
#' # Normally missing values are not considered to be equal
#' NA == NA
#'
#' # But they are for the purposes of considering uniqueness
#' vec_unique(c(NA, NA, NA, NA, 1, 2, 1))
vec_unique <- function(x) {
  vec_slice(x, vec_unique_loc(x))
}

#' @rdname vec_unique
#' @export
vec_unique_loc <- function(x) {
  .Call(vctrs_unique_loc, x)
}

#' @rdname vec_unique
#' @export
vec_unique_count <- function(x) {
  .Call(vctrs_n_distinct, x)
}

# First location ----------------------------------------------------------

#' Locate first occurrences
#'
#' `vec_first_loc()` returns an integer vector giving, for each element of `x`,
#' the location of its first occurrence.
#'
#' While `vec_unique_loc()` and `vec_duplicate_loc()` return _only_ the
#' locations of the unique and duplicate values respectively, `vec_first_loc()`
#' returns a vector the same size as `x` with locations corresponding to the
#' first occurrence of each value.
#'
#' @inherit vec_duplicate sections
#' @param x A vector (including a data frame).
#' @return An integer vector the same size as `x`.
#'
#' @export
#' @examples
#' x <- c(10, 10, 20, 30, 30, 40)
#'
#' vec_first_loc(x)
#'
#' # Compare with `vec_unique_loc()`, which returns locations
#' # for only the unique values
#' vec_unique_loc(x)
#'
#' # And `vec_duplicate_loc()`, which returns locations for only the
#' # duplicate values
#' vec_duplicate_loc(x)
#'
#' vec_first_loc(c(NA, NA, NaN, NaN))
vec_first_loc <- function(x) {
  .Call(vctrs_first_loc, x)
}

# Matching ----------------------------------------------------------------

#' Find matching observations across vectors
#'
#' `vec_in()` returns a logical vector based on whether `needle` is found in
#' haystack. `vec_match()` returns an integer vector giving location of
#' `needle` in `haystack`, or `NA` if it's not found.
#'
#' `vec_in()` is equivalent to [%in%]; `vec_match()` is equivalent to `match()`.
#'
#' @inherit vec_duplicate sections
#' @param needles,haystack Vector of `needles` to search for in vector haystack.
#'   `haystack` should usually be unique; if not `vec_match()` will only
#'   return the location of the first match.
#'
#'   `needles` and `haystack` are coerced to the same type prior to
#'   comparison.
#' @return A vector the same length as `needles`. `vec_in()` returns a
#'   logical vector; `vec_match()` returns an integer vector.
#' @export
#' @examples
#' hadley <- strsplit("hadley", "")[[1]]
#' vec_match(hadley, letters)
#'
#' vowels <- c("a", "e", "i", "o", "u")
#' vec_match(hadley, vowels)
#' vec_in(hadley, vowels)
#'
#' # Only the first index of duplicates is returned
#' vec_match(c("a", "b"), c("a", "b", "a", "b"))
vec_match <- function(needles, haystack) {
  .Call(vctrs_match, needles, haystack)
}

#' @export
#' @rdname vec_match
vec_in <- function(needles, haystack) {
  .Call(vctrs_in, needles, haystack)
}
