# Coercion ----------------------------------------------------------------

df_as_tibble <- function(df) {
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

# Conditionally registered in .onLoad()
vec_ptype2.tbl_df <- function(x, y, ...) {
  UseMethod("vec_ptype2.tbl_df", y)
}
vec_ptype2.tbl_df.default <- function(x, y, ...) {
  # FIXME: Do we need some sort of `next_vec_type2()`?
  vec_ptype2.data.frame(x, y, ...)
}

vec_ptype2.tbl_df.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  df_as_tibble(.Call(vctrs_type2_df_df, x, y, x_arg, y_arg))
}
vec_ptype2.data.frame.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  df_as_tibble(.Call(vctrs_type2_df_df, x, y, x_arg, y_arg))
}


tbl_ptype2.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (inherits_only(x, c("tbl_df", "tbl", "data.frame"))) {
    UseMethod("tbl_ptype2.tbl_df", y)
  } else {
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}
tbl_ptype2.tbl_df.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

tbl_ptype2.tbl_df.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (inherits_only(y, "data.frame")) {
    tibble::tibble()
  } else {
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}
tbl_ptype2.data.frame.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (inherits_only(y, c("tbl_df", "tbl", "data.frame"))) {
    tibble::tibble()
  } else {
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}
