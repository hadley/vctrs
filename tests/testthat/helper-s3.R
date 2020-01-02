
foobar <- function(x = list()) structure(x, class = "vctrs_foobar")
tbl_foobar <- function(x = list()) structure(x, class = c("vctrs_foobar", "data.frame"))

unrownames <- function(x) {
  row.names(x) <- NULL
  x
}

local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}
local_proxy <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_proxy.vctrs_proxy = function(x, ...) proxy_deref(x),
    vec_restore.vctrs_proxy = function(x, to, ...) new_proxy(x),

    vec_ptype2.vctrs_proxy = function(x, y, ...) UseMethod("vec_ptype2.vctrs_proxy", y),
    vec_ptype2.vctrs_proxy.vctrs_proxy = function(x, y, ...) new_proxy(vec_ptype(proxy_deref(x))),

    vec_cast.vctrs_proxy = function(x, to, ...) UseMethod("vec_cast.vctrs_proxy"),
    vec_cast.vctrs_proxy.default = function(x, to, ...) stop_incompatible_cast(x, to),
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
    vec_restore.vctrs_proxy = function(x, ...) new_proxy(x)
  )
}

local_no_stringsAsFactors <- function(frame = caller_env()) {
  local_options(.frame = frame, stringsAsFactors = FALSE)
}

tibble <- tibble::tibble

local_foobar_proxy <- function(frame = caller_env()) {
  local_methods(.frame = frame, vec_proxy.vctrs_foobar = identity)
}

subclass <- function(x) {
  class(x) <- c("vctrs_foo", "vctrs_foobar", class(x))
  x
}


local_vcols_methods <- function(frame = caller_env()) {
  local_vcols_memory_methods(frame)
  local_vcols_ptype2_methods(frame)
  local_vcols_cast_methods(frame)
}
new_vcols <- function(x, groups) {
  structure(x, class = c("vctrs_virtual", "data.frame"), groups = groups)
}
is_bare_vcols <- function(x) {
  inherits_only(x, c("vctrs_virtual", "data.frame"))
}

local_vcols_memory_methods <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_proxy.vctrs_virtual = function(x, ...) {
      vec_proxy_push_vcols(x, `mypkg::groups` = attr(x, "groups"))
    },
    vec_restore.vctrs_virtual = function(x, ...) {
      parts <- vec_proxy_pop_vcols(x, "mypkg::groups")
      new_vcols(
        parts$proxy,
        groups = parts$vcols$`mypkg::groups`
      )
    }
  )
}
local_vcols_ptype2_methods <- function(frame = caller_env()) {
  local_methods(
    .frame = frame,
    vec_ptype2.vctrs_virtual = function(x, y, ...) {
      if (is_bare_vcols(x)) {
        UseMethod("vec_ptype2.vctrs_virtual")
      } else {
        stop_incompatible_type(x, y)
      }
    },
    vec_ptype2.vctrs_virtual.vctrs_virtual = function(x, y, ...) {
      if (!is_bare_vcols(y)) {
        stop_incompatible_type(x, y)
      }
      vec_ptype(x)
    },
    vec_ptype2.data.frame.vctrs_virtual = function(x, y, ...) {
      stop_incompatible_type(x, y)
    },

    tbl_ptype2.vctrs_virtual = function(x, y, ...) {
      if (is_bare_vcols(x)) {
        UseMethod("tbl_ptype2.vctrs_virtual")
      } else {
        stop_incompatible_type(x, y)
      }
    },
    tbl_ptype2.vctrs_virtual.vctrs_virtual = function(x, y, ...) {
      if (!is_bare_vcols(y)) {
        stop_incompatible_type(x, y)
      }
      tbl_ptype(x)
    }
  )
}
local_vcols_cast_methods <- function(frame = caller_env()) {
  local_methods(
    .frame = frame,
    tbl_cast.vctrs_virtual = function(x, to, ..., x_arg = "x", to_arg = "to") {
      if (is_bare_vcols(to)) {
        UseMethod("tbl_cast.vctrs_virtual")
      } else {
        vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
      }
    },
    tbl_cast.vctrs_virtual.data.frame = function(x, to, ...) {
      groups <- attr(x, "groups") %||% vec_init(int(), vec_size(x))
      new_vcols(x, groups = groups)
    }
  )
}
