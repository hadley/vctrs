
new_ctor <- function(base_class) {
  function(x = list(), ..., class = NULL) {
    if (inherits(x, "tbl_df")) {
      tibble::new_tibble(x, class = c(class, base_class), nrow = nrow(x))
    } else if (is.data.frame(x)) {
      structure(x, class = c(class, base_class, "data.frame"), ...)
    } else {
      structure(x, class = c(class, base_class), ...)
    }
  }
}

foobar <- new_ctor("vctrs_foobar")
foobaz <- new_ctor("vctrs_foobaz")
quux <- new_ctor("vctrs_quux")

expect_foobar <- function(x) expect_is({{ x }}, "vctrs_foobar")
expect_foobaz <- function(x) expect_is({{ x }}, "vctrs_foobaz")
expect_quux <- function(x) expect_is({{ x }}, "vctrs_quux")

with_c_foobar <- function(expr) {
  with_methods(
    expr,
    c.vctrs_foobar = function(...) foobar(NextMethod())
  )
}

unrownames <- function(x) {
  row.names(x) <- NULL
  x
}

local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}
with_methods <- function(.expr, ...) {
  local_methods(...)
  .expr
}

local_proxy <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_proxy.vctrs_proxy = function(x, ...) proxy_deref(x),
    vec_restore.vctrs_proxy = function(x, to, ...) new_proxy(x),

    vec_ptype2.vctrs_proxy = function(x, y, ...) UseMethod("vec_ptype2.vctrs_proxy"),
    vec_ptype2.vctrs_proxy.vctrs_proxy = function(x, y, ...) new_proxy(vec_ptype(proxy_deref(x))),

    vec_cast.vctrs_proxy = function(x, to, ...) UseMethod("vec_cast.vctrs_proxy"),
    vec_cast.vctrs_proxy.vctrs_proxy = function(x, to, ...) x
  )
}

new_proxy <- function(x) {
  structure(list(env(x = x)), class = "vctrs_proxy")
}
proxy_deref <- function(x) {
  x[[1]]$x
}
local_env_proxy <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_proxy.vctrs_proxy = proxy_deref,
    vec_restore.vctrs_proxy = function(x, ...) new_proxy(x),
    vec_cast.vctrs_proxy = function(x, to, ...) UseMethod("vec_cast.vctrs_proxy"),
    vec_cast.vctrs_proxy.vctrs_proxy = function(x, to, ...) x,
    vec_ptype2.vctrs_proxy = function(x, y, ...) UseMethod("vec_ptype2.vctrs_proxy"),
    vec_ptype2.vctrs_proxy.vctrs_proxy = function(x, y, ...) new_proxy(proxy_deref(x)[0])
  )
}

local_no_stringsAsFactors <- function(frame = caller_env()) {
  local_options(.frame = frame, stringsAsFactors = FALSE)
}

tibble <- function(...) {
  tibble::tibble(...)
}

local_foobar_proxy <- function(frame = caller_env()) {
  local_methods(.frame = frame, vec_proxy.vctrs_foobar = identity)
}

subclass <- function(x) {
  class(x) <- c("vctrs_foo", "vctrs_foobar", class(x))
  x
}


# Subclass promoted to logical
new_lgl_subtype <- function(x) {
  stopifnot(is_logical(x))
  structure(x, class = "vctrs_lgl_subtype")
}
local_lgl_subtype <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_ptype2.vctrs_lgl_subtype = function(x, y, ...) UseMethod("vec_ptype2.vctrs_lgl_subtype"),
    vec_ptype2.vctrs_lgl_subtype.vctrs_lgl_subtype = function(x, y, ...) x,
    vec_ptype2.vctrs_lgl_subtype.logical = function(x, y, ...) y,
    vec_ptype2.logical.vctrs_lgl_subtype = function(x, y, ...) x,

    vec_cast.vctrs_lgl_subtype = function(x, to, ...) UseMethod("vec_cast.vctrs_lgl_subtype"),
    vec_cast.vctrs_lgl_subtype.vctrs_lgl_subtype = function(x, to, ...) x,
    vec_cast.vctrs_lgl_subtype.logical = function(x, to, ...) new_lgl_subtype(x),
    vec_cast.logical.vctrs_lgl_subtype = function(x, to, ...) unstructure(x)
  )
}
with_lgl_subtype <- function(expr) {
  local_lgl_subtype()
  expr
}

# Logical promoted to subclass
new_lgl_supertype <- function(x) {
  stopifnot(is_logical(x))
  structure(x, class = "vctrs_lgl_supertype")
}
local_lgl_supertype <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_ptype2.vctrs_lgl_supertype = function(x, y, ...) UseMethod("vec_ptype2.vctrs_lgl_supertype"),
    vec_ptype2.vctrs_lgl_supertype.vctrs_lgl_supertype = function(x, y, ...) x,
    vec_ptype2.vctrs_lgl_supertype.logical = function(x, y, ...) x,
    vec_ptype2.logical.vctrs_lgl_supertype = function(x, y, ...) y,

    vec_cast.vctrs_lgl_supertype = function(x, to, ...) UseMethod("vec_cast.vctrs_lgl_supertype"),
    vec_cast.vctrs_lgl_supertype.vctrs_lgl_supertype = function(x, to, ...) x,
    vec_cast.vctrs_lgl_supertype.logical = function(x, to, ...) new_lgl_subtype(x),
    vec_cast.logical.vctrs_lgl_supertype = function(x, to, ...) unstructure(x)
  )
}
with_lgl_supertype <- function(expr) {
  local_lgl_supertype()
  expr
}

foobar_df_ptype2 <- function(x, y, ...) {
  foobar(df_ptype2(x, y, ...))
}
foobar_df_cast <- function(x, y, ...) {
  foobar(df_cast(x, y, ...))
}
local_foobar_df_methods <- function(expr, frame = caller_env()) {
  local_methods(
    .frame = frame,
    vec_ptype2.vctrs_foobar.vctrs_foobar = foobar_df_ptype2,
    vec_ptype2.data.frame.vctrs_foobar = foobar_df_ptype2,
    vec_ptype2.vctrs_foobar.data.frame = foobar_df_ptype2,
    vec_cast.vctrs_foobar.vctrs_foobar = foobar_df_cast,
    vec_cast.data.frame.vctrs_foobar = foobar_df_cast,
    vec_cast.vctrs_foobar.data.frame = foobar_df_cast
  )
}
with_foobar_df_methods <- function(expr) {
  local_foobar_df_methods()
  expr
}

# Stores number of elements in an attribute
new_count_list <- function(x = list()) {
  stopifnot(vec_is_list(x))
  structure(
    x,
    class = c("vctrs_count_list", "list"),
    count = length(x)
  )
}
local_count_list_methods <- function(frame = caller_env()) {
  local_methods(
    .frame = frame,
    vec_restore.vctrs_count_list = function(x, ...) new_count_list(x),
    vec_ptype2.vctrs_count_list.vctrs_count_list = function(x, y, ...) x,
    vec_ptype2.list.vctrs_count_list = function(x, y, ...) y,
    vec_ptype2.vctrs_count_list.list = function(x, y, ...) x,
    vec_cast.vctrs_count_list.vctrs_count_list = function(x, to, ...) x,
    vec_cast.list.vctrs_count_list = function(x, to, ...) unstructure(x),
    vec_cast.vctrs_count_list.list = function(x, to, ...) new_count_list(x)
  )
}
