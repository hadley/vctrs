#' Data frame class
#'
#' A `data.frame` is a list with a "row.names" attribute. Each
#' element of the list must be named, and of the same length. These functions
#' help the base data.frame class fit in to the vctrs type system by
#' providing constructors, coercion functions, and casting functions.
#'
#' @param .x A named list of equal-length vectors. The lengths are not
#'   checked; it is responsibility of the caller to make sure they are
#'   equal.
#' @param .size Number of rows. If `NULL`, will be computed from the length of
#'   the first element of `.x`.
#' @param ...,.class Additional arguments for creating subclasses.
#'   The `"names"` and `"row.names"` attributes override input in `.x` and
#'   `.size`, respectively:
#'
#'   - `"names"` is used if provided, overriding existing names in `.x`.
#'   - `"row.names"` is used if provided, if `.size` is provided it must be
#'     consistent.
#'
#' @export
#' @keywords internal
#' @examples
#' new_data_frame(list(x = 1:10, y = 10:1))
new_data_frame <- function(.x = list(), ..., .size = NULL, .class = NULL) {
  .External(vctrs_new_data_frame, .x, .size, .class, ...)
}
new_data_frame <- fn_inline_formals(new_data_frame, ".x")


# Light weight constructor used for tests - avoids having to repeatedly do
# stringsAsFactors = FALSE etc. Should not be used in internal code as is
# not a real helper as it lacks value checks.
data_frame <- function(...) {
  cols <- list(...)
  new_data_frame(cols)
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
  if (inherits_only(x, "data.frame")) {
    abbr <- "df"
  } else {
    abbr <- class(x)[[1]]
  }
  paste0(abbr, vec_ptype_shape(x))
}

#' @export
vec_proxy_compare.data.frame <- function(x, ..., relax = FALSE) {
  out <- lapply(as.list(x), vec_proxy_compare, relax = TRUE)
  new_data_frame(out, .size = nrow(x))
}

df_is_coercible <- function(x, y) {
  vec_is_coercible(
    new_data_frame(x),
    new_data_frame(y)
  )
}


# Coercion ----------------------------------------------------------------

#' @rdname new_data_frame
#' @export vec_ptype2.data.frame
#' @method vec_ptype2 data.frame
#' @export
vec_ptype2.data.frame <- function(x, y, ...) {
  UseMethod("vec_ptype2.data.frame")
}
#' @method vec_ptype2.data.frame data.frame
#' @export
vec_ptype2.data.frame.data.frame <- function(x, y, ...) {
  df_ptype2(x, y, ...)
}
# Returns a `data.frame` no matter the input classes
df_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  .Call(vctrs_df_ptype2, x, y, x_arg, y_arg)
}

vec_ptype2_df_fallback_normalise <- function(x, y) {
  x_orig <- x
  y_orig <- y

  ptype <- df_ptype2(x, y)

  # Empty columns first to avoid name repairs
  x <- x[0, 0, drop = FALSE]
  y <- y[0, 0, drop = FALSE]

  x[seq_along(ptype)] <- ptype
  y[seq_along(ptype)] <- ptype

  # Restore attributes if no `[` method is implemented
  if (df_has_base_subset(x)) {
    x <- vec_restore(x, x_orig)
  }
  if (df_has_base_subset(y)) {
    y <- vec_restore(y, y_orig)
  }

  list(x = x, y = y)
}
vec_cast_df_fallback_normalise <- function(x, to) {
  orig <- x

  cast <- df_cast(x, to)

  # Empty columns first to avoid name repairs
  x <- x[0]

  # Seq-assign should be more widely implemented than empty-assign?
  x[seq_along(to)] <- cast

  # Restore attributes if no `[` method is implemented
  if (df_has_base_subset(x)) {
    x <- vec_restore(x, orig)
  }

  x
}

# Fallback for data frame subclasses (#981)
vec_ptype2_df_fallback <- function(x, y, x_arg = "", y_arg = "") {
  ptype <- vec_ptype2(
    new_data_frame(x),
    new_data_frame(y)
  )

  classes <- NULL
  if (is_df_fallback(x)) {
    classes <- c(classes, known_classes(x))
    x <- new_data_frame(x)
  }
  if (is_df_fallback(y)) {
    classes <- c(classes, known_classes(y))
    y <- new_data_frame(y)
  }
  x_class <- class(x)[[1]]
  y_class <- class(y)[[1]]

  if (!all(c(x_class, y_class) %in% classes)) {
    msg <- cnd_type_message(
      x, y,
      x_arg, y_arg,
      NULL,
      "combine",
      NULL,
      fallback = "<data.frame>"
    )

    if (identical(x_class, y_class)) {
      msg <- c(
        msg,
        incompatible_attrib_bullets()
      )
    } else {
      msg <- c(
        msg,
        i = "Convert all inputs to the same class to avoid this warning.",
        i = "See <https://vctrs.r-lib.org/reference/faq-warning-convert-inputs.html>."
      )
    }

    warn(msg)
  }

  # Return a fallback class so we don't warn multiple times. This
  # fallback class is stripped in `vec_ptype_finalise()`.
  new_fallback_df(
    ptype,
    known_classes = unique(c(classes, x_class, y_class))
  )
}

is_df_subclass <- function(x) {
  inherits(x, "data.frame") && !identical(class(x), "data.frame")
}
is_df_fallback <- function(x) {
  inherits(x, "vctrs:::df_fallback")
}
new_fallback_df <- function(x, known_classes, size = nrow(x)) {
  new_data_frame(
    x,
    .size = size,
    known_classes = known_classes,
    .class = "vctrs:::df_fallback"
  )
}

#' @export
`[.vctrs:::df_fallback` <- function(x, i, ...) {
  classes <- known_classes(x)
  x <- NextMethod()
  new_fallback_df(x, known_classes = classes)
}

known_classes <- function(x) {
  if (is_df_fallback(x)) {
    attr(x, "known_classes")
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
vec_cast.data.frame.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
df_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  .Call(vctrs_df_cast, x, to, x_arg, to_arg)
}

#' @export
vec_restore.data.frame <- function(x, to, ..., n = NULL) {
  .Call(vctrs_bare_df_restore, x, to, n)
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
    class = "vctrs_error_cast_lossy_dropped"
  )
}
