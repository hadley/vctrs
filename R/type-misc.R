
# `numeric_version` from base ----------------------------------------

#' @export
vec_proxy.numeric_version <- function(x, ...) {
  x
}

#' @export
vec_proxy_compare.numeric_version <- function(x, ...) {
  # Encodes `x` as a character vector representing the numeric versions.
  # In the C locale, this compare the same as you'd expect `x` to.
  x <- .encode_numeric_version(x)
  x <- unstructure(x)
  x
}

# `omit` from base ---------------------------------------------------

#' @export
vec_proxy.omit <- function(x, ...) {
  x
}
#' @export
vec_restore.omit <- function(x, ...) {
  structure(x, class = "omit")
}

#' @export
vec_ptype2.omit.omit <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.integer.omit <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.omit.integer <- function(x, y, ...) {
  y
}
#' @export
vec_ptype2.double.omit <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.omit.double <- function(x, y, ...) {
  y
}

#' @export
vec_cast.omit.omit <- function(x, to, ...) {
  x
}
#' @export
vec_cast.integer.omit <- function(x, to, ...) {
  vec_cast(vec_data(x), to, ...)
}
#' @export
vec_cast.omit.integer <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.double.omit <- function(x, to, ...) {
  vec_cast(vec_data(x), to, ...)
}
#' @export
vec_cast.omit.double <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}


# `exclude` from base ------------------------------------------------

#' @export
vec_proxy.exclude <- function(x, ...) {
  x
}
#' @export
vec_restore.exclude <- function(x, ...) {
  structure(x, class = "exclude")
}

#' @export
vec_ptype2.exclude.exclude <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.integer.exclude <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.exclude.integer <- function(x, y, ...) {
  y
}
#' @export
vec_ptype2.double.exclude <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.exclude.double <- function(x, y, ...) {
  y
}

#' @export
vec_cast.exclude.exclude <- function(x, to, ...) {
  x
}
#' @export
vec_cast.integer.exclude <- function(x, to, ...) {
  vec_cast(vec_data(x), to, ...)
}
#' @export
vec_cast.exclude.integer <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.double.exclude <- function(x, to, ...) {
  vec_cast(vec_data(x), to, ...)
}
#' @export
vec_cast.exclude.double <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}


# `data.table` -------------------------------------------------------

delayedAssign("as.data.table", {
  if (is_installed("data.table")) {
    env_get(ns_env("data.table"), "as.data.table")
  } else {
    function(...) abort("`data.table` must be installed.")
  }
})

dt_ptype2 <- function(x, y, ...) {
  as.data.table(df_ptype2(x, y, ...))
}
dt_cast <- function(x, to, ...) {
  as.data.table(df_cast(x, to, ...))
}

#' @export
vec_ptype2.data.table.data.table <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.table.data.frame <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.data.table <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}

#' @export
vec_cast.data.table.data.table <- function(x, to, ...) {
  dt_cast(x, to, ...)
}
#' @export
vec_cast.data.table.data.frame <- function(x, to, ...) {
  dt_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.data.table <- function(x, to, ...) {
  df_cast(x, to, ...)
}
