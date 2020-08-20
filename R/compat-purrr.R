# nocov start --- compat-purrr-vctrs --- 2020-08-18

map <- function(.x, .f, ...) {
  if (is.data.frame(.x)) {
    .x <- unclass(.x)
  }
  vctrs::vec_map(.x, .f, ...)
}
map_lgl <- function(.x, .f, ...) {
  map(.x, .f, ..., .ptype = lgl())
}
map_int <- function(.x, .f, ...) {
  map(.x, .f, ..., .ptype = int())
}
map_dbl <- function(.x, .f, ...) {
  map(.x, .f, ..., .ptype = dbl())
}
map_chr <- function(.x, .f, ...) {
  map(.x, .f, ..., .ptype = chr())
}
map_cpl <- function(.x, .f, ...) {
  map(.x, .f, ..., .ptype = cpl())
}
map_raw <- function(.x, .f, ...) {
  map(.x, .f, ..., .ptype = raw())
}

walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

pluck <- function(.x, .f) {
  map(.x, `[[`, .f)
}
pluck_lgl <- function(.x, .f) {
  map_lgl(.x, `[[`, .f)
}
pluck_int <- function(.x, .f) {
  map_int(.x, `[[`, .f)
}
pluck_dbl <- function(.x, .f) {
  map_dbl(.x, `[[`, .f)
}
pluck_chr <- function(.x, .f) {
  map_chr(.x, `[[`, .f)
}
pluck_cpl <- function(.x, .f) {
  map_cpl(.x, `[[`, .f)
}
pluck_raw <- function(.x, .f) {
  map_raw(.x, `[[`, .f)
}

map2 <- function(.x, .y, .f, ...) {
  Map(.f, .x, .y, ...)
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
map2_cpl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "complex")
}

args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}
pmap <- function(.l, .f, ...) {
  args <- args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}

probe <- function(.x, .p, ...) {
  as_predicate <- function(.fn, ..., .allow_na = FALSE) {
    .fn <- as_function(.fn, ...)

    function(...) {
      out <- .fn(...)

      if (!is_bool(out)) {
        if (is_na(out) && .allow_na) {
          # Always return a logical NA
          return(NA)
        }
        abort("Predicate functions must return a single `TRUE` or `FALSE`.")
      }

      out
    }
  }

  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_predicate(.p, ...)
    map_lgl(.x, .p, ...)
  }
}

keep <- function(.x, .f, ...) {
  .x[probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
map_if <- function(.x, .p, .f, ..., .else = NULL) {
  sel <- probe(.x, .p)

  out <- rep_along(.x, list(NULL))
  out[sel]  <- map(.x[sel], .f, ...)

  if (is_null(.else)) {
    out[!sel] <- .x[!sel]
  } else {
    out[!sel]  <- map(.x[!sel], .else, ...)
  }

  set_names(out, names(.x))
}

map_at <- function(.x, .at, .f, ...) {
  at_selection <- function(nm, .at){
    if (is_quosures(.at)){
      if (!is_installed("tidyselect")) {
        abort("Using tidyselect in `map_at()` requires tidyselect.")
      }
      .at <- tidyselect::vars_select(.vars = nm, !!!.at)
    }
    .at
  }
  inv_which <- function(x, sel) {
    if (is.character(sel)) {
      names <- names(x)
      if (is.null(names)) {
        stop("character indexing requires a named object", call. = FALSE)
      }
      names %in% sel
    } else if (is.numeric(sel)) {
      if (any(sel < 0)) {
        !seq_along(x) %in% abs(sel)
      } else {
        seq_along(x) %in% sel
      }

    } else {
      stop("unrecognised index type", call. = FALSE)
    }
  }

  where <- at_selection(names(.x), .at)
  sel <- inv_which(.x, where)

  out <- rep_along(.x, list(NULL))
  out[sel]  <- map(.x[sel], .f, ...)
  out[!sel] <- .x[!sel]

  set_names(out, names(.x))
}

transpose <- function(.l) {
  inner_names <- names(.l[[1]])
  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
  }

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

every <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}


# nocov end
