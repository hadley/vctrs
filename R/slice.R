#' Get or set observations in a vector
#'
#' This provides a common interface to extracting and modifying observations
#' for all vector types, regardless of dimensionality. It is an analog to `[`
#' that matches [vec_size()] instead of `length()`.
#'
#' @param x A vector
#' @param i An integer, character or logical vector specifying the positions or
#'   names of the observations to get/set.
#'   Specify `TRUE` to index all elements (as in `x[]`), or `NULL`, `FALSE` or
#'   `integer()` to index none (as in `x[NULL]`).
#' @param value Replacement values. `value` is cast to the type of
#'   `x`, but only if they have a common type. See below for examples
#'   of this rule.
#' @return A vector of the same type as `x`.
#'
#' @section Genericity:
#'
#' Support for S3 objects depends on whether the object implements a
#' [vec_proxy()] method.
#'
#' * When a `vec_proxy()` method exists, the proxy is sliced and
#'   `vec_restore()` is called on the result.
#'
#' * Otherwise `vec_slice()` falls back to the base generic `[`.
#'
#' Note that S3 lists are treated as scalars by default, and will
#' cause an error if they don't implement a [vec_proxy()] method.
#'
#' @section Differences with base R subsetting:
#'
#' * `vec_slice()` only slices along one dimension. For
#'   two-dimensional types, the first dimension is subsetted.
#'
#' * `vec_slice()` preserves attributes by default.
#'
#' * `vec_slice<-()` is type-stable and always returns the same type
#'   as the LHS.
#'
#' @export
#' @keywords internal
#' @examples
#' x <- sample(10)
#' x
#' vec_slice(x, 1:3)
#'
#' # You can assign with the infix variant:
#' vec_slice(x, 2) <- 100
#' x
#'
#' # Or with the regular variant that doesn't modify the original input:
#' y <- vec_assign(x, 3, 500)
#' y
#' x
#'
#'
#' # Slicing objects of higher dimension:
#' vec_slice(mtcars, 1:3)
#'
#' # Type stability --------------------------------------------------
#'
#' # The assign variant is type stable. It always returns the same
#' # type as the input.
#' x <- 1:5
#' vec_slice(x, 2) <- 20.0
#'
#' # `x` is still an integer vector because the RHS was cast to the
#' # type of the LHS:
#' vec_ptype(x)
#'
#' # Compare to `[<-`:
#' x[2] <- 20.0
#' vec_ptype(x)
#'
#'
#' # Note that the types must be coercible for the cast to happen.
#' # For instance, you can cast a character vector to an integer:
#' vec_cast("1", integer())
#'
#' # But these types are not coercible:
#' try(vec_ptype2("1", integer()))
#'
#' # Hence you cannot assign character values to an integer or double
#' # vector:
#' try(vec_slice(x, 2) <- "20")
vec_slice <- function(x, i) {
  .Call(vctrs_slice, x, i)
}

# Called when `x` has dimensions
vec_slice_fallback <- function(x, i) {
  out <- unclass(vec_proxy(x))
  vec_assert(out)

  d <- vec_dim_n(out)
  if (d == 2) {
    out <- out[i, , drop = FALSE]
  } else {
    miss_args <- rep(list(missing_arg()), d - 1)
    out <- eval_bare(expr(out[i, !!!miss_args, drop = FALSE]))
  }

  vec_restore(out, x)
}

#' @rdname vec_slice
#' @export
`vec_slice<-` <- function(x, i, value) {
  .Call(vctrs_assign, x, i, value)
}
#' @rdname vec_slice
#' @export
vec_assign <- function(x, i, value) {
  .Call(vctrs_assign, x, i, value)
}
vec_assign_fallback <- function(x, i, value) {
  # Work around bug in base `[<-`
  existing <- !is.na(i)
  i <- vec_slice(i, existing)
  value <- vec_slice(value, existing)

  d <- vec_dim_n(x)
  miss_args <- rep(list(missing_arg()), d - 1)
  eval_bare(expr(x[i, !!!miss_args] <- value))
  x
}

vec_get <- function(x, i) {
  i <- vec_as_position(i, vec_size(x), vec_names(x))
  .Call(vctrs_get, x, i)
}

#' Create an index vector or a position
#'
#' @description
#'
#' These helpers provide a means of standardizing common indexing
#' methods such as integer, character or logical indexing.
#'
#' * `vec_as_index()` accepts integer, character, or logical vectors
#'   of any size. The output is always an integer vector that is
#'   suitable for subsetting with `[` or [vec_slice()]. It might be a
#'   different size than the input because negative selections are
#'   transformed to positive ones and logical vectors are transformed
#'   to a vector of indices for the `TRUE` positions.
#'
#' * `vec_as_position()` accepts a single number or string. It returns
#'   a single position as a integer vector of size 1. This is suitable
#'   for extracting with `[[`.
#'
#' @inheritParams vec_slice
#' @param n A single integer representing the total size of the
#'   object that `i` is meant to index into.
#' @param names If `i` is a character vector, `names` should be a character
#'   vector that `i` will be matched against to construct the index. Otherwise,
#'   not used. The default value of `NULL` will result in an error
#'   if `i` is a character vector.
#' @param arg The argument name to be displayed in error messages when
#'   `vec_as_index()` and `vec_as_position()` are used to check the
#'   type of a function input.
#' @param convert_values Experimental. Character vector indicating
#'   what types of values should be converted. Can currently only be
#'   set to `"negative"`.
#' @param allow_types Experimental. Character vector indicating one or
#'   several types of position to be allowed as input: `"indicator"`,
#'   `"position"`, or `"name"`. Indicators must be subtypes of
#'   logical. Locations must be coercible to integers (possibly from
#'   a larger type like double). Names must be subtypes of character.
#'
#' @return `vec_as_index()` returns an integer vector that can be used
#'   as an index in a subsetting operation. `vec_as_position()`
#'   returns an integer of size 1 that can be used a scalar index for
#'   extracting an element.
#'
#' @examples
#' x <- array(1:6, c(2, 3))
#' dimnames(x) <- list(c("r1", "r2"), c("c1", "c2", "c3"))
#'
#' # The most common use case validates row indices
#' vec_as_index(1, vec_size(x))
#'
#' # Negative indices can be used to index from the back
#' vec_as_index(-1, vec_size(x))
#'
#' # Character vectors can be used if `names` are provided
#' vec_as_index("r2", vec_size(x), rownames(x))
#'
#' # You can also construct an index for dimensions other than the first
#' vec_as_index(c("c2", "c1"), ncol(x), colnames(x))
#'
#' @keywords internal
#' @export
vec_as_index <- function(i, n,
                         names = NULL,
                         ...,
                         allow_types = c("indicator", "position", "name"),
                         convert_values = "negative",
                         arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()

  n <- vec_coercible_cast(n, integer())
  vec_assert(n, integer(), 1L)
  i <- vec_coerce_index(i, arg = arg, allow_types = allow_types)

  convert_values <- as_opts_index_convert_values(convert_values, arg = arg)
  .Call(vctrs_as_index, i, n, names, convert_values)
}
#' @rdname vec_as_index
#' @param allow_values Experimental. Character vector indicating zero,
#'   one or several types of values to be allowed as input:
#'   `"negative"` or `"missing"`. By default, positions can't be
#'   negative or missing.
#' @export
vec_as_position <- function(i, n,
                            names = NULL,
                            ...,
                            allow_values = NULL,
                            arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  maybe_get(vec_maybe_as_position(
    i,
    n = n,
    names = names,
    allow_values = allow_values,
    arg = arg
  ))
}

#' Coerce to the base type of a position
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("experimental")}
#'
#' This coerces `i` to the base type expected by [vec_as_index()] or
#' [vec_as_position()]. The resulting vector is not checked in any
#' way (length, missingness, negative elements).
#'
#' @inheritParams vec_as_index
#' @keywords internal
#' @export
vec_coerce_index <- function(i,
                             ...,
                             allow_types = c("indicator", "position", "name"),
                             arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  maybe_get(vec_maybe_coerce_index(
    i,
    arg = arg,
    allow_types = allow_types
  ))
}
#' @rdname vec_coerce_index
#' @export
vec_coerce_position <- function(i,
                                ...,
                                allow_types = c("position", "name"),
                                arg = "i") {
  if (!missing(...)) ellipsis::check_dots_empty()
  maybe_get(vec_maybe_coerce_position(i, arg, allow_types = allow_types))
}

vec_maybe_coerce_index <- function(i, arg, allow_types) {
  allow_types <- as_opts_index_type(allow_types, arg = arg)

  if (!vec_is(i)) {
    return(maybe(error = new_error_index_bad_type(
      i = i,
      .arg = arg,
      allow_types = allow_types
    )))
  }

  nms <- names(i)

  if (is.object(i)) {
    if (vec_is_subtype(i, lgl())) {
      i <- vec_cast(i, lgl())
    } else if (vec_is_subtype(i, int())) {
      i <- vec_cast(i, int())
    } else if (vec_is_subtype(i, chr())) {
      i <- vec_cast(i, chr())
    } else {
      return(maybe(error = new_error_index_bad_type(
        i,
        .arg = arg,
        allow_types = allow_types
      )))
    }
  } else if (is_double(i)) {
    maybe <- tryCatch(
    {
      i <- vec_coercible_cast(i, int(), x_arg = arg, to_arg = "")
      names(i) <- nms
      maybe(i)
    },
    vctrs_error_cast_lossy = function(err) {
      maybe(error = new_error_index_bad_type(
        i = i,
        parent = err,
        body = cnd_bullets_index_lossy_cast,
        allow_types = allow_types
      ))
    })
    return(maybe)
  }

  # Coerce unspecified vectors to integer only if logical indices
  # are not allowed
  if (!allow_types[["indicator"]] && is_unspecified(i)) {
    if (allow_types[["position"]]) {
      i <- vec_cast(i, int())
    } else {
      i <- vec_cast(i, chr())
    }
  }

  allowed <- switch(typeof(i),
    logical = allow_types[["indicator"]],
    integer = allow_types[["position"]],
    character = allow_types[["name"]],
    FALSE
  )
  if (!allowed) {
    return(maybe(error = new_error_index_bad_type(
      i = i,
      .arg = arg,
      allow_types = allow_types
    )))
  }

  # FIXME: Work around lack of name restoration in `vec_cast()`
  names(i) <- nms

  maybe(i)
}

vec_maybe_coerce_position <- function(i, arg, allow_types) {
  allow_types <- as_opts_position_type(allow_types, arg = arg)
  maybe <- vec_maybe_coerce_index(i, arg, allow_types = allow_types)

  # Return a subclass of index error
  if (!is_null(maybe$error)) {
    parent <- maybe$error$parent
    if (inherits(parent, "vctrs_error_cast_lossy")) {
      bullets <- cnd_bullets_index_lossy_cast
    } else {
      bullets <- cnd_bullets_position_bad_base_type
    }

    maybe$error <- new_error_position_bad_type(
      i = maybe$error$i,
      allow_types = allow_types,
      .arg = arg,
      body = bullets,
      parent = maybe$error$parent
    )

    return(maybe)
  }

  i <- maybe$value

  if (typeof(i) == "logical") {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      allow_types = allow_types,
      .arg = arg,
      body = cnd_bullets_position_bad_base_type
    )))
  }

  maybe
}
vec_maybe_as_position <- function(i,
                                  n,
                                  names,
                                  allow_values,
                                  arg) {
  allow_types <- c("position", "name")

  allow_values <- as_opts_position_values(allow_values, arg = arg)
  allow_missing <- allow_values[["missing"]]
  allow_negative <- allow_values[["negative"]]

  maybe <- vec_maybe_coerce_position(
    i = i,
    arg = arg,
    allow_types = allow_types
  )

  if (!is_null(maybe$error)) {
    return(maybe)
  }

  # Positions must be size 1, can't be NA, and must be positive
  i <- maybe$value

  if (length(i) != 1L) {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      allow_types = allow_types,
      .arg = arg,
      body = cnd_bullets_position_need_scalar
    )))
  }

  neg <- typeof(i) == "integer" && !is.na(i) && i < 0L
  if (allow_negative && neg) {
    i <- -i
  }

  if (is.na(i)) {
    if (!allow_missing && is.na(i)) {
      maybe <- maybe(error = new_error_position_bad_type(
        i = i,
        allow_types = allow_types,
        .arg = arg,
        body = cnd_bullets_position_need_present
      ))
    } else {
      maybe <- maybe(i)
    }
    return(maybe)
  }

  if (i == 0L) {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      allow_types = allow_types,
      .arg = arg,
      body = cnd_bullets_position_need_non_zero
    )))
  }

  if (!allow_negative && neg) {
    return(maybe(error = new_error_position_bad_type(
      i = i,
      allow_types = allow_types,
      .arg = arg,
      body = cnd_bullets_position_need_non_negative
    )))
  }

  # FIXME: Use maybe approach in internal implementation?
  err <- NULL
  i <- tryCatch(
    vec_as_index(i, n, names = names, arg = arg),
    vctrs_error_index_bad_type = function(err) {
      err <<- err
      i
    }
  )

  if (neg) {
    i <- -i
  }

  if (is_null(err)) {
    maybe(i)
  } else {
    maybe(error = new_error_position_bad_type(
      i = i,
      allow_types = allow_types,
      parent = err,
      .arg = arg
    ))
  }
}

index_type_opts <- c("indicator", "position", "name")
index_type_opts_indefinite_singular <- c("an indicator", "a position", "a name")
index_type_opts_indefinite_plural <- c("indicators", "positions", "names")

collapse_index_type <- function(allow_types, plural = FALSE) {
  if (plural) {
    types <- index_type_opts_indefinite_plural
  } else {
    types <- index_type_opts_indefinite_singular
  }
  types <- types[force(allow_types)]
  types <- glue::glue_collapse(types, sep = ", ", last = " or ")
  types
}

as_opts_index_type <- function(x, arg = NULL) {
  if (inherits(x, "vctrs_opts_index_type")) {
    return(x)
  }
  new_opts(
    x,
    index_type_opts,
    subclass = "vctrs_opts_index_type",
    arg = arg
  )
}
as_opts_position_type <- function(x, arg = NULL) {
  if ("indicator" %in% x) {
    abort("Indicators can't be position.")
  }
  as_opts_index_type(x, arg = arg)
}


position_values_opts <- c("missing", "negative")

as_opts_position_values <- function(x, arg = NULL) {
  if (inherits(x, "vctrs_opts_position_values")) {
    return(x)
  }
  new_opts(
    x,
    position_values_opts,
    subclass = "vctrs_opts_position_values",
    arg = arg
  )
}


index_convert_values_opts <- "negative"

as_opts_index_convert_values <- function(x, arg = NULL) {
  if (inherits(x, "vctrs_opts_index_convert_values")) {
    return(x)
  }
  new_opts(
    x,
    index_convert_values_opts,
    subclass = "vctrs_opts_index_convert_values",
    arg = arg
  )
}


new_index_error <- function(.subclass = NULL, i, ..., .arg = "i") {
  error_cnd(
    .subclass = c(.subclass, "vctrs_error_index"),
    i = i,
    .arg = .arg,
    ...
  )
}
new_error_index_bad_type <- function(i,
                                     allow_types,
                                     ...,
                                     .arg = "i",
                                     .subclass = NULL) {
  new_index_error(
    .subclass = c(.subclass, "vctrs_error_index_bad_type"),
    i = i,
    allow_types = allow_types,
    .arg = .arg,
    ...
  )
}
new_error_position_bad_type <- function(i,
                                        allow_types,
                                        ...,
                                        .arg = "i",
                                        .subclass = NULL) {
  new_error_index_bad_type(
    .subclass = c(.subclass, "vctrs_error_position_bad_type"),
    i = i,
    allow_types = allow_types,
    .arg = .arg,
    ...
  )
}

#' @export
cnd_header.vctrs_error_index_bad_type <- function(cnd) {
  "Must subset with an index vector."
}
#' @export
cnd_body.vctrs_error_index_bad_type <- function(cnd) {
  arg <- cnd$.arg %||% "i"
  type <- obj_type(cnd$i)
  expected_types <- collapse_index_type(cnd$allow_types, plural = TRUE)

  format_error_bullets(c(
    x = glue::glue("`{arg}` has the wrong type `{type}`."),
    i = glue::glue("These indices must be {expected_types}.")
  ))
}
cnd_bullets_index_lossy_cast <- function(cnd, ...) {
  format_error_bullets(c(x = cnd_header(cnd$parent)))
}

#' @export
cnd_header.vctrs_error_position_bad_type <- function(cnd) {
  "Must extract with a single index."
}

cnd_bullets_position_bad_base_type <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  type <- obj_type(cnd$i)
  expected_types <- collapse_index_type(cnd$allow_types)

  format_error_bullets(c(
    x = glue::glue("`{arg}` has the wrong type `{type}`."),
    i = glue::glue("This index must be {expected_types}.")
  ))
}
cnd_bullets_position_need_scalar <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  size <- length(cnd$i)
  format_error_bullets(c(
    x = glue::glue("`{arg}` has the wrong size {size}."),
    i = "This index must be size 1."
  ))
}
cnd_bullets_position_need_present <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  format_error_bullets(c(
    x = glue::glue("`{arg}` can't be `NA`."),
    i = "This index can't be missing."
  ))
}
cnd_bullets_position_need_non_zero <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  format_error_bullets(c(
    x = glue::glue("`{arg}` can't be zero."),
    i = "This index must be a positive integer."
  ))
}
cnd_bullets_position_need_non_negative <- function(cnd, ...) {
  arg <- cnd$.arg %||% "i"
  format_error_bullets(c(
    x = glue::glue("`{arg}` (with value {cnd$i}) has the wrong sign."),
    i = "This index must be a positive integer."
  ))
}

vec_index <- function(x, i, ...) {
  i <- maybe_missing(i, TRUE)

  if (!dots_n(...)) {
    return(vec_slice(x, i))
  }

  # Need to unclass to avoid infinite recursion through `[`
  proxy <- unclass(vec_proxy(x))
  vec_assert(proxy)

  i <- vec_as_index(i, vec_size(x), vec_names(x))
  out <- proxy[i, ..., drop = FALSE]

  vec_restore(out, x, n = length(i))
}

#' Initialize a vector
#'
#' @param x Template of vector to initialize.
#' @param n Desired size of result.
#' @export
#' @examples
#' vec_init(1:10, 3)
#' vec_init(Sys.Date(), 5)
#' vec_init(mtcars, 2)
vec_init <- function(x, n = 1L) {
  n <- vec_cast(n, integer())
  vec_assert(n, size = 1L)

  .Call(vctrs_init, x, n)
}

#' Repeatedly slice a vector
#'
#' `vec_chop()` provides an efficient method to repeatedly slice a vector. It
#' captures the pattern of `map(indices, vec_slice, x = x)`.
#'
#' @param x A vector
#' @param indices A list of index values to slice `x` with, or `NULL`. Each
#'   element of the list must be an integer, character or logical vector that
#'   would be valid as an index in [vec_slice()]. If `NULL`, `x` is split into
#'   its individual elements, equivalent to using an `indices` of
#'   `as.list(vec_seq_along(x))`.
#' @return A list of size `vec_size(indices)` or, if `indices == NULL`,
#'   `vec_size(x)`.
#' @export
#' @examples
#' vec_chop(1:5)
#' vec_chop(1:5, list(1, 1:2))
#' vec_chop(mtcars, list(1:3, 4:6))
vec_chop <- function(x, indices = NULL) {
  .Call(vctrs_chop, x, indices)
}

# Exposed for testing  (`starts` is 0-based)
vec_chop_seq <- function(x, starts, sizes, increasings = TRUE) {
  args <- vec_recycle_common(starts, sizes, increasings)
  .Call(vctrs_chop_seq, x, args[[1]], args[[2]], args[[3]])
}

# Exposed for testing (`start` is 0-based)
vec_slice_seq <- function(x, start, size, increasing = TRUE) {
  .Call(vctrs_slice_seq, x, start, size, increasing)
}

# Exposed for testing (`i` is 1-based)
vec_slice_rep <- function(x, i, n) {
  .Call(vctrs_slice_rep, x, i, n)
}
