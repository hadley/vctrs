# Constructor and basic methods  ---------------------------------------------

#' rcrd (record) S3 class
#'
#' The rcrd class extends [vctr]. A rcrd is composed of 1 or more [field]s,
#' which must be vectors of the same length. Is designed specifically for
#' classes that can naturally be decomposed into multiple vectors of the same
#' length, like [POSIXlt], but where the organisation should be considered
#' an implementation detail invisible to the user (unlike a [data.frame]).
#'
#' @param fields A list. It must possess the following properties:
#'   * no attributes (apart from names)
#'   * syntactic names
#'   * length 1 or greater
#'   * elements are vectors
#'   * elements have equal length
#' @param ... Additional attributes
#' @param class Name of subclass.
#' @export
#' @aliases ses rcrd
#' @keywords internal
new_rcrd <- function(fields, ..., class = character()) {
  check_fields(fields)
  structure(fields, ..., class = c(class, "vctrs_rcrd", "vctrs_vctr"))
}

check_fields <- function(fields) {
  if (!is.list(fields) || length(fields) == 0) {
    abort("`fields` must be a list of length 1 or greater.")
  }

  if (!has_unique_names(fields)) {
    abort("`fields` must have unique names.")
  }

  if (!identical(names(attributes(fields)), "names")) {
    abort("`fields` must have no attributes (apart from names).")
  }

  is_vector <- map_lgl(fields, is_vector)
  if (!all(is_vector)) {
    abort("Every field must be a vector.")
  }

  lengths <- map_int(fields, length)
  if (!all_equal(lengths)) {
    abort("Every field must be the same length.")
  }

  invisible(fields)
}

#' @export
vec_proxy.vctrs_rcrd <- function(x, ...) {
  new_data_frame(unclass(x))
}
#' @export
vec_restore.vctrs_rcrd <- function(x, to, ...) {
  x <- NextMethod()
  attr(x, "row.names") <- NULL
  x
}

#' @export
length.vctrs_rcrd <- function(x) {
  .Call(vctrs_size, x)
}

#' @export
names.vctrs_rcrd <- function(x) {
  NULL
}

#' @export
format.vctrs_rcrd <- function(x, ...) {
  # nocov start
  stop_unimplemented(x, "format")
  # nocov end
}

#' @export
obj_str_data.vctrs_rcrd <- function(x, ...) {
  obj_str_leaf(x, ...)
}

#' @method vec_cast vctrs_rcrd
#' @export
vec_cast.vctrs_rcrd <- function(x, to, ...) UseMethod("vec_cast.vctrs_rcrd")

#' @method vec_cast.vctrs_rcrd vctrs_rcrd
#' @export
vec_cast.vctrs_rcrd.vctrs_rcrd <- function(x, to, ...) {
  # This assumes that we don't have duplicate field names,
  # which is verified even in the constructor.
  if (!setequal(fields(x), fields(to))) {
    stop_incompatible_cast(x, to)
  }

  new_data <- map2(
    vec_data(x)[fields(to)],
    vec_data(to),
    vec_cast
  )

  new_rcrd(new_data)
}


# Subsetting --------------------------------------------------------------

#' @export
`[.vctrs_rcrd` <-  function(x, i, ...) {
  vec_index(x, i, ...)
}

#' @export
`[[.vctrs_rcrd` <- function(x, i, ...) {
  out <- lapply(vec_data(x), `[[`, i, ...)
  vec_restore(out, x)
}

#' @export
`$.vctrs_rcrd` <- function(x, i, ...) {
  stop_unsupported(x, "subsetting with $")
}

#' @export
rep.vctrs_rcrd <- function(x, ...) {
  out <- lapply(vec_data(x), rep, ...)
  vec_restore(out, x)
}

#' @export
`length<-.vctrs_rcrd` <- function(x, value) {
  out <- lapply(vec_data(x), `length<-`, value)
  vec_restore(out, x)
}

#' @export
as.list.vctrs_rcrd <- function(x, ...) {
  vec_cast(x, list())
}

# Replacement -------------------------------------------------------------

#' @export
`[[<-.vctrs_rcrd` <- function(x, i, value) {
  value <- vec_cast(value, x)
  out <- map2(vec_data(x), vec_data(value), function(x, value) {
    x[[i]] <- value
    x
  })
  vec_restore(out, x)
}

#' @export
`$<-.vctrs_rcrd` <- function(x, i, value) {
  stop_unsupported(x, "subset assignment with $")
}

#' @export
`[<-.vctrs_rcrd` <- function(x, i, value) {
  value <- vec_cast(value, x)

  if (missing(i)) {
    replace <- function(x, value) {
      x[] <- value
      x
    }
  } else {
    replace <- function(x, value) {
      x[i] <- value
      x
    }
  }
  out <- map2(vec_data(x), vec_data(value), replace)
  vec_restore(out, x)
}

# Equality and ordering ---------------------------------------------------

# FIXME

#' @export
vec_proxy_compare.vctrs_rcrd <- function(x, ..., relax = FALSE) {
  new_data_frame(vec_data(x), .size = length(x))
}

#' @export
vec_math.vctrs_rcrd <- function(.fn, .x, ...) {
  stop_unsupported(.x, "vec_math")
}
