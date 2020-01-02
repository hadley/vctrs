#' Identify groups
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'
#' * `vec_group_id()` returns an identifier for the group that each element of
#'   `x` falls in, constructed in the order that they appear. The number of
#'   groups is also returned as an attribute, `n`.
#'
#' * `vec_group_pos()` returns a data frame containing a `key` column with the
#'   unique groups, and a `pos` column with the positions of each group in `x`.
#'
#' * `vec_group_rle()` locates groups in `x` and returns them run length
#'   encoded in the order that they appear. The return value is a rcrd object
#'   with fields for the `group` identifiers and the run `length` of the
#'   corresponding group. The number of groups is also returned as an
#'   attribute, `n`.
#'
#' @param x A vector
#' @return
#'   * `vec_group_id()`: An integer vector with the same size as `x`.
#'   * `vec_group_pos()`: A two column data frame with size equal to
#'     `vec_size(vec_unique(x))`.
#'     * A `key` column of type `vec_ptype(x)`
#'     * A `pos` column of type list, with elements of type integer.
#'   * `vec_group_rle()`: A `vctrs_group_rle` rcrd object with two integer
#'     vector fields: `group` and `length`.
#'
#'   Note that when using `vec_group_pos()` for complex types, the default
#'   `data.frame` print method will be suboptimal, and you will want to coerce
#'   into a tibble to better understand the output.
#' @name vec_group
#' @examples
#' purrr <- c("p", "u", "r", "r", "r")
#' vec_group_id(purrr)
#' vec_group_rle(purrr)
#'
#' groups <- mtcars[c("vs", "am")]
#' vec_group_id(groups)
#'
#' group_rle <- vec_group_rle(groups)
#' group_rle
#'
#' # Access fields with `field()`
#' field(group_rle, "group")
#' field(group_rle, "length")
#'
#' # `vec_group_id()` is equivalent to
#' vec_match(groups, vec_unique(groups))
#'
#' vec_group_pos(mtcars$vs)
#' vec_group_pos(mtcars[c("vs", "am")])
#'
#' if (require("tibble")) {
#'   as_tibble(vec_group_pos(mtcars[c("vs", "am")]))
#' }
NULL

#' @rdname vec_group
#' @export
vec_group_id <- function(x) {
  .Call(vctrs_group_id, x)
}

#' @rdname vec_group
#' @export
vec_group_pos <- function(x) {
  .Call(vctrs_group_pos, x)
}
vec_group_pos_as_id <- function(x) {
  stopifnot(is_list(x), every(x, is_integer))

  n <- sum(lengths(x))
  out <- vec_init(integer(), n)

  for (i in seq_along(x)) {
    loc <- x[[i]]
    vec_slice(out, loc) <- i
  }

  out
}
vec_group_id_as_pos <- function(groups, n_groups) {
  groups <- vec_split(seq_along(groups), groups)

  # Remove list-of class. This should be unnecessary soon.
  groups$val <- as.list(groups$val)

  out <- rep(list(int()), n_groups)
  vec_assign(out, groups$key, groups$val)
}

#' @rdname vec_group
#' @export
vec_group_rle <- function(x) {
  .Call(vctrs_group_rle, x)
}

#' @export
format.vctrs_group_rle <- function(x, ...) {
  group <- field(x, "group")
  length <- field(x, "length")
  paste0(group, "x", length)
}

#' @export
obj_print_header.vctrs_group_rle <- function(x, ...) {
  size <- vec_size(x)
  n <- attr(x, "n")
  cat_line("<", vec_ptype_full(x), "[", size, "][n = ", n, "]>")
  invisible(x)
}

# For testing
new_group_rle <- function(group, length, n) {
  vec_assert(group, integer())
  vec_assert(length, integer())
  vec_assert(n, integer(), 1L)

  if (vec_size(group) != vec_size(length)) {
    abort("`group` and `length` must have the same size.")
  }

  new_rcrd(list(group = group, length = length), n = n, class = "vctrs_group_rle")
}
