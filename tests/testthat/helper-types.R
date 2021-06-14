# Don't call tibble::tibble() to avoid catch-22, because tibble now uses vctrs
bare_tibble <- structure(data.frame(), class = c("tbl_df", "tbl", "data.frame"))

base_empty_types <- list(
  null = NULL,
  logical = lgl(),
  integer = int(),
  double = dbl(),
  complex = cpl(),
  character = chr(),
  raw = bytes(),
  list = list(),
  dataframe = data.frame()
)

base_s3_empty_types <- list(
  bare_factor = new_factor(),
  bare_ordered = new_ordered(),
  bare_date = new_date(),
  bare_posixct = new_datetime(tzone = "UTC"),
  bare_posixlt = as.POSIXlt(new_datetime(tzone = "UTC")),
  bare_tibble = bare_tibble
)

proxied_empty_types <- list(
  double = new_hidden(),
  dataframe = bare_tibble,
  dataframe = structure(data.frame(), class = c("vctrs_foobar", "data.frame"))
)

empty_types <- c(
  base_empty_types,
  proxied_empty_types,
  # Non proxied type
  scalar = foobar(list()),
  scalar = function() NULL
)

atomics <- list(TRUE, 1L, 1.0, 1i, "foo", bytes(1))
vectors <- c(atomics, list(list()))
records <- list(
  df = data.frame(x = 1),
  rcrd = new_rcrd(list(x = 1)),
  posixlt = as.POSIXlt("2020-01-01")
)

tuple <- function(x = integer(), y = integer()) {
  fields <- vec_recycle_common(
    x = vec_cast(x, integer()),
    y = vec_cast(y, integer())
  )
  new_rcrd(fields, class = "vctrs_tuple")
}

tuple_methods <- list(
  format.vctrs_tuple = function(x, ...) {
    paste0("(", field(x, "x"), ",", field(x, "y"), ")")
  },
  vec_ptype2.vctrs_tuple.vctrs_tuple = function(x, y, ...) x,
  vec_cast.vctrs_tuple.vctrs_tuple = function(x, to, ...) x
)

local_tuple_methods <- function(frame = caller_env()) {
  local_methods(.frame = frame, !!!tuple_methods)
}
set_tuple_methods <- function(env = global_env()) {
  env_bind(env, !!!tuple_methods)
}


local_comparable_tuple <- function(frame = caller_env()) {
  local_tuple_methods(frame = frame)

  # Compare only on first field
  local_methods(
    .frame = frame,
    vec_proxy_equal.vctrs_tuple = function(x, ...) field(x, "x"),
    vec_proxy_compare.vctrs_tuple = function(x, ...) {
      # override vec_proxy_compare.vctrs_rcrd
      field(x, "x")
    }
  )
}

c_na <- function(...) {
  x <- c(...)
  names(x)[names(x) == ""] <- NA_character_
  x
}
