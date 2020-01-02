#' Generics for tabular vectors
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'
#' These generics make it possible to work with tables, i.e. vectors
#' of dimension 2 like data frames and matrices. The main motivation
#' of this API is to support colwise operations such as slicing
#' columns. Operating across columns requires slightly different
#' semantics and concepts than rowwise operations:
#'
#' - A tabular prototype is a zero-cols table. In order for your
#'   tabular class to be compatible with vctrs, it needs to support
#'   instances with zero columns without devolving to a base data
#'   frame or throwing an error.
#'
#' - The row structure is part of tabular prototypes. For base data
#'   frames, this includes the number of rows and the row names. This
#'   is in contrast to vector prototypes which contain the column
#'   structure but not the row structure.
#'
#' - The vector type of a data frame includes its columns (their names
#'   and their types). The tabular type of a data frame doesn't. Hence
#'   `tbl_ptype2()` and `tbl_cast()` accept inputs of any shape,
#'   whereas `vec_ptype2()` and `vec_cast()` require congruent shapes
#'   and names.
#'
#' The details and theory of tabular operations are in development and
#' likely to change in the future.
#'
#' @param x A tabular vector.
#'
#' @export
tbl_is <- function(x) {
  .Call(vctrs_tbl_is, x)
}
#' @rdname tbl_is
#' @inheritParams vec_assert
#' @export
tbl_assert <- function(x, arg = as_label(substitute(x))) {
  .Call(vctrs_tbl_assert, x, arg)
}

#' @rdname tbl_is
#' @export
tbl_size <- function(x) {
  .Call(vctrs_tbl_size, x)
}

#' @rdname tbl_is
#' @inheritParams vec_slice
tbl_slice <- function(x, i) {
  .Call(vctrs_tbl_slice, x, i)
}
#' @rdname tbl_is
tbl_ptype <- function(x) {
  .Call(vctrs_tbl_ptype, x)
}


#' @rdname tbl_is
#' @inheritParams vec_ptype2
#' @export
tbl_ptype2 <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_tbl_ptype2, x, y, x_arg, y_arg))
  UseMethod("tbl_ptype2")
}
tbl_ptype2_dispatch <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  tbl_assert(x, x_arg)
  UseMethod("tbl_ptype2")
}
#' @export
tbl_ptype2.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (has_same_type(x, y)) {
    return(tbl_ptype(x))
  }
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @rdname tbl_is
#' @inheritParams vec_ptype_common
#' @export
tbl_ptype_common <- function(..., .ptype = NULL) {
  .External(vctrs_tbl_ptype_common, .ptype, environment())
}


#' @rdname tbl_is
#' @inheritParams vec_cast
#' @export
tbl_cast <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_tbl_cast, x, to, x_arg, to_arg))
  UseMethod("tbl_cast", to)
}
tbl_cast_dispatch <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  UseMethod("tbl_cast", to)
}
#' @export
tbl_cast.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (has_same_type(x, to)) {
    return(x)
  }
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
