#' Data frame class
#'
#' A `data.frame` [data.frame()] is a list with "row.names" attribute. Each
#' element of the list must be named, and of the same length. These functions
#' help the base data.frame classes fit in to the vctrs type system by
#' providing constructors, coercion functions, and casting functions.
#'
#' @param x A named list of equal-length vectors. The lengths are not
#'   checked; it is responsibility of the caller to make sure they are
#'   equal.
#' @param n Number of rows. If `NULL`, will be computed from the length of
#'   the first element of `x`.
#' @param ...,class Additional arguments for creating subclasses.
#' @export
#' @keywords internal
#' @examples
#' new_data_frame(list(x = 1:10, y = 10:1))
new_data_frame <- function(x = list(), n = NULL, ..., class = character()) {
  if (!is.list(x)) {
    abort("`x` must be a list.")
  }

  if (is.null(n)) {
    n <- df_size(x)
  } else if (!is.integer(n) || length(n) != 1L) {
    abort("`n` must be an integer of size 1.")
  }

  # names() should always be a character vector, but we can't enforce that
  # because as.data.frame() returns a data frame with NULL names to indicate
  # that outer names should be used
  if (length(x) == 0) {
    names(x) <- character()
  }

  new_attributes <- list(
    names = names(x),
    ...,
    class = c(class, "data.frame"),
    row.names = .set_row_names(n)
  )

  attributes(x) <- new_attributes

  x
}

# Light weight constructor used for tests - avoids having to repeatedly do
# stringsAsFactors = FALSE etc. Should not be used in internal code as is
# not a real helper as it lacks value checks.
data_frame <- function(..., n = NULL) {
  cols <- list2(...)
  new_data_frame(cols, n = n)
}

#' @export
vec_ptype_full.data.frame <- function(x, ...) {
  if (length(x) == 0) {
    return(paste0(class(x)[[1]], "<>"))
  } else if (length(x) == 1) {
    return(paste0(class(x)[[1]], "<", names(x), ":", vec_ptype_full(x[[1]]), ">"))
  }

  # Needs to handle recursion with indenting
  types <- map_chr(x, vec_ptype_full)
  needs_indent <- grepl("\n", types)
  types[needs_indent] <- map(types[needs_indent], function(x) indent(paste0("\n", x), 4))

  names <- paste0("  ", format(names(x)))

  paste0(
    class(x)[[1]], "<\n",
    paste0(names, ": ", types, collapse = "\n"),
    "\n>"
  )
}

#' @export
vec_ptype_abbr.data.frame <- function(x, ...) {
  paste0("df", vec_ptype_shape(x))
}

#' @export
vec_proxy_compare.data.frame <- function(x, ..., relax = FALSE) {
  out <- lapply(as.list(x), vec_proxy_compare, relax = TRUE)
  new_data_frame(out, nrow(x))
}


# Coercion ----------------------------------------------------------------

#' @rdname new_data_frame
#' @export vec_ptype2.data.frame
#' @method vec_ptype2 data.frame
#' @export
vec_ptype2.data.frame <- function(x, y, ...) UseMethod("vec_ptype2.data.frame", y)
#' @method vec_ptype2.data.frame data.frame
#' @export
vec_ptype2.data.frame.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  .Call(vctrs_type2_df_df, x, y, x_arg, y_arg)
}
#' @method vec_ptype2.data.frame default
#' @export
vec_ptype2.data.frame.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @rdname new_data_frame
#' @export tbl_ptype2.data.frame
#' @method tbl_ptype2 data.frame
#' @export
tbl_ptype2.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  tbl_assert(y, y_arg)
  if (inherits_only(x, "data.frame")) {
    UseMethod("tbl_ptype2.data.frame", y)
  } else {
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}
#' @method tbl_ptype2.data.frame data.frame
#' @export
tbl_ptype2.data.frame.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (inherits_only(y, "data.frame")) {
    tbl_ptype2_base(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}
#' @method tbl_ptype2.data.frame default
#' @export
tbl_ptype2.data.frame.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# Works with any subclasses of data frame
tbl_ptype2_base <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  recycled <- vec_recycle_common(x = x, y = y)

  rows_x <- row_names(recycled$x)
  rows_y <- row_names(recycled$y)
  if (!identical(rows_x, rows_y) &&
        !identical(sort(rows_x), sort(rows_y))) {
    abort(c(
      "Can't find common rectangle type for these data frames.",
      x = "The row names must be compatible."
    ))
  }

  tbl_ptype(recycled$x)
}

# Like row.names() but returns NULL for unset row names
row_names <- function(x) {
  if (.row_names_info(x) < 0L) {
    NULL
  } else {
    attr(x, "row.names")
  }
}


# Cast --------------------------------------------------------------------

#' @rdname new_data_frame
#' @export vec_cast.data.frame
#' @method vec_cast data.frame
#' @export
vec_cast.data.frame <- function(x, to, ...) {
  UseMethod("vec_cast.data.frame")
}
#' @export
#' @method vec_cast.data.frame data.frame
vec_cast.data.frame.data.frame <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  .Call(vctrs_df_as_dataframe, x, to, x_arg, to_arg)
}
#' @export
#' @method vec_cast.data.frame default
vec_cast.data.frame.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
vec_restore.data.frame <- function(x, to, ..., n = NULL) {
  .Call(vctrs_df_restore, x, to, n)
}


#' @rdname new_data_frame
#' @export tbl_cast.data.frame
#' @method tbl_cast data.frame
#' @export
tbl_cast.data.frame <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (inherits_only(to, "data.frame")) {
    UseMethod("tbl_cast.data.frame")
  } else {
    vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }
}
#' @export
#' @method tbl_cast.data.frame data.frame
tbl_cast.data.frame.data.frame <- function(x, to, ...) {
  # This is a departure from vector casting which is currently more
  # liberal. Casting requires existence of a common type,
  # e.g. compatible row names.
  ptype <- tbl_ptype2(x, to)

  x <- as.data.frame.data.frame(x)
  x <- vec_recycle(x, vec_size(to))

  x
}
#' @export
#' @method tbl_cast.data.frame default
tbl_cast.data.frame.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

# AsIS --------------------------------------------------------------------

# Arises with base df ctor: `data.frame(x = I(list(1, 2:3)))`

#' @export
vec_proxy.AsIs <- function(x, ...) {
  class(x) <- setdiff(class(x), "AsIs")
  vec_proxy(x)
}
#' @export
vec_restore.AsIs <- function(x, to, ...) {
  I(x)
}

# Helpers -----------------------------------------------------------------

df_size <- function(x) {
  .Call(vctrs_df_size, x)
}

df_lossy_cast <- function(out, x, to) {
  extra <- setdiff(names(x), names(to))

  maybe_lossy_cast(
    result = out,
    x = x,
    to = to,
    lossy = length(extra) > 0,
    locations = int(),
    details = inline_list("Dropped variables: ", extra, quote = "`"),
    .subclass = "vctrs_error_cast_lossy_dropped"
  )
}
