str_dup <- function(x, times) {
  paste0(rep(x, times = times), collapse = "")
}

indent <- function(x, n) {
  pad <- str_dup(" ", n)
  map_chr(x, gsub, pattern = "(\n+)", replacement = paste0("\\1", pad))
}

ones <- function(...) {
  array(1, dim = c(...))
}

vec_coerce_bare <- function(x, type) {
  # Unexported wrapper around Rf_coerceVector()
  coerce <- env_get(ns_env("rlang"), "vec_coerce")
  coerce(x, type)
}


# Matches the semantics of c() - based on experimenting with the output
# of c(), not reading the source code.
outer_names <- function(names, outer, n) {
  .Call(vctrs_outer_names, names, outer, vec_cast(n, int()))
}

has_inner_names <- function(x) {
  !all(map_lgl(map(x, vec_names), is.null))
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}


set_partition <- function(x, y) {
  list(
    both = intersect(x, y),
    only_x = setdiff(x, y),
    only_y = setdiff(y, x)
  )
}

all_equal <- function(x) all(x == x[[1]])

inline_list <- function(title, x, width = getOption("width"), quote = "") {
  label_width <- width - nchar(title)
  x <- glue::glue_collapse(
    encodeString(x, quote = quote),
    sep = ", ",
    width = label_width
  )
  paste0(title, x)
}

has_unique_names <- function(x) {
  nms <- names(x)

  if (length(nms) != length(x)) {
    return(FALSE)
  }

  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }

  !anyDuplicated(nms)
}

compact <- function(x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

paste_line <- function (...) {
  paste(chr(...), collapse = "\n")
}

has_dim <- function(x) {
  !is.null(attr(x, "dim"))
}

# Experimental
maybe <- function(value = NULL, error = NULL) {
  structure(
    list(value = value, error = error),
    class = "rlang_maybe"
  )
}
maybe_get <- function(x) {
  if (!is_null(x$error)) {
    cnd_signal(x$error)
  }
  x$value
}

obj_type <- function(x) {
  if (vec_is(x)) {
    vec_ptype_full(x)
  } else if (is.object(x)) {
    paste(class(x), collapse = "/")
  } else {
    typeof(x)
  }
}

new_opts <- function(x, opts, subclass = NULL, arg = NULL) {
  if (!all(x %in% opts)) {
    if (is_null(arg)) {
      arg <- "Argument"
    } else {
      arg <- glue::glue("`{arg}`")
    }
    opts <- encodeString(opts, quote = "\"")
    opts <- glue::glue_collapse(opts, sep = ", ", last = " or ")
    abort(glue::glue("{arg} must be one of {opts}."))
  }

  structure(
    set_names(opts %in% x, opts),
    class = c(subclass, "vctrs_opts")
  )
}

unstructure <- function(x) {
  attributes(x) <- NULL
  x
}
