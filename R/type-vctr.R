#' vctr (vector) S3 class
#'
#' @description
#' This abstract class provides a set of useful default methods that makes it
#' considerably easier to get started with a new S3 vector class. See
#' `vignette("s3-vector")` to learn how to use it to create your own S3
#' vector classes.
#'
#' @details
#' List vctrs are special cases. When created through `new_vctr()`, the
#' resulting list vctr should always be recognized as a list by
#' `vec_is_list()`. Because of this, if `inherit_base_type` is `FALSE`
#' an error is thrown.
#'
#' @section Base methods:
#' The vctr class provides methods for many base generics using a smaller
#' set of generics defined by this package. Generally, you should think
#' carefully before overriding any of the methods that vctrs implements for
#' you as they've been carefully planned to be internally consistent.
#'
#' * `[[` and `[` use `NextMethod()` dispatch to the underlying base function,
#'    then restore attributes with `vec_restore()`.
#'    `rep()` and `length<-` work similarly.
#'
#' * `[[<-` and `[<-` cast `value` to same type as `x`, then call
#'   `NextMethod()`.
#'
#' * `as.logical()`, `as.integer()`, `as.numeric()`, `as.character()`,
#'   `as.Date()` and `as.POSIXct()` methods call `vec_cast()`.
#'   The `as.list()` method calls `[[` repeatedly, and the `as.data.frame()`
#'   method uses a standard technique to wrap a vector in a data frame.
#'
#' * `as.factor()`, `as.ordered()` and `as.difftime()` are not generic functions
#'   in base R, but have been reimplemented as generics in the `generics`
#'   package. `vctrs` extends these and calls `vec_cast()`. To inherit this
#'   behaviour in a package, import and re-export the generic of interest
#'   from `generics`.
#'
#' * `==`, `!=`, `unique()`, `anyDuplicated()`, and `is.na()` use
#'   [vec_proxy()].
#'
#' * `<`, `<=`, `>=`, `>`, `min()`, `max()`, `range()`, `median()`,
#'   `quantile()`, and `xtfrm()` methods use [vec_proxy_compare()].
#'
#' * `+`, `-`, `/`, `*`, `^`, `%%`, `%/%`, `!`, `&`, and `|` operators
#'   use [vec_arith()].
#'
#' * Mathematical operations including the Summary group generics (`prod()`,
#'   `sum()`, `any()`, `all()`), the Math group generics (`abs()`, `sign()`,
#'   etc), `mean()`, `is.nan()`, `is.finite()`, and `is.infinite()`
#'   use [vec_math()].
#'
#' * `dims()`, `dims<-`, `dimnames()`, `dimnames<-`, `levels()`, and
#'   `levels<-` methods throw errors.
#'
#' @param .data Foundation of class. Must be a vector
#' @param ... Name-value pairs defining attributes
#' @param class Name of subclass.
#' @param inherit_base_type `r lifecycle::badge("experimental")`
#'   A single logical, or `NULL`. Does this class extend the base type of
#'   `.data`? i.e. does the resulting object extend the behaviour of the
#'   underlying type? Defaults to `FALSE` for all types except lists, which
#'   are required to inherit from the base type.
#' @export
#' @keywords internal
#' @aliases vctr
new_vctr <- function(.data,
                     ...,
                     class = character(),
                     inherit_base_type = NULL) {
  if (!is_vector(.data)) {
    abort("`.data` must be a vector type.")
  }

  nms <- validate_names(.data)

  if (is_list(.data)) {
    if (is.data.frame(.data)) {
      abort("`.data` can't be a data frame.")
    }

    if (is.null(inherit_base_type)) {
      inherit_base_type <- TRUE
    } else if (is_false(inherit_base_type)) {
      abort("List `.data` must inherit from the base type.")
    }
  }

  # Default to `FALSE` in all cases except lists
  if (is.null(inherit_base_type)) {
    inherit_base_type <- FALSE
  }

  class <- c(class, "vctrs_vctr", if (inherit_base_type) typeof(.data))
  attrib <- list(names = nms, ..., class = class)

  vec_set_attributes(.data, attrib)
}

validate_names <- function(.data) {
  nms <- names(.data)

  if (any_na_names(nms)) {
    abort("The names of `.data` must not be `NA`.")
  }

  nms
}
any_na_names <- function(names) {
  if (is.null(names)) {
    FALSE
  } else {
    any(is.na(names))
  }
}

#' @export
vec_proxy.vctrs_vctr <- function(x, ...) {
  if (is_list(x)) {
    unclass(x)
  } else {
    x
  }
}

#' @export
vec_restore.vctrs_vctr <- function(x, to, ..., i = NULL) {
  if (typeof(x) != typeof(to)) {
    stop_incompatible_cast(x, to, x_arg = "", to_arg = "")
  }
  NextMethod()
}

#' @method vec_cast vctrs_vctr
#' @export
vec_cast.vctrs_vctr <- function(x, to, ...) {
  UseMethod("vec_cast.vctrs_vctr")
}

vctr_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  # These are not strictly necessary, but make bootstrapping a new class
  # a bit simpler
  if (is.object(x)) {
    if (is_same_type(x, to)) {
      x
    } else {
      stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
    }
  } else {
    # FIXME: `vec_restore()` should only be called on proxies
    vec_restore(x, to)
  }
}

#' @export
c.vctrs_vctr <- function(..., recursive = FALSE, use.names = TRUE) {
  if (!is_false(recursive)) {
    abort("`recursive` must be `FALSE` when concatenating vctrs classes.")
  }
  if (!is_true(use.names)) {
    abort("`use.names` must be `TRUE` when concatenating vctrs classes.")
  }
  vec_c(...)
}

# Printing ----------------------------------------------------------------

#' @export
print.vctrs_vctr <- function(x, ...) {
  obj_print(x, ...)
  invisible(x)
}

#' @export
str.vctrs_vctr <- function(object, ...) {
  obj_str(object, ...)
}

#' @export
format.vctrs_vctr <- function(x, ...) {
  format(vec_data(x), ...)
}

# Subsetting --------------------------------------------------------------

#' @export
`[.vctrs_vctr` <- function(x, i, ...) {
  vec_index(x, i, ...)
}

#' @export
`[[.vctrs_vctr` <- function(x, i, ...) {
  if (is.list(x)) {
    NextMethod()
  } else {
    vec_restore(NextMethod(), x)
  }
}

#' @export
`$.vctrs_vctr` <- function(x, i) {
  if (is.list(x)) {
    NextMethod()
  } else {
    vec_restore(NextMethod(), x)
  }
}

#' @export
rep.vctrs_vctr <- function(x, ...) {
  vec_restore(NextMethod(), x)
}

#' @export
`length<-.vctrs_vctr` <- function(x, value) {
  vec_restore(NextMethod(), x)
}

#' @export
diff.vctrs_vctr <- function(x, lag = 1L, differences = 1L, ...) {
  stopifnot(length(lag) == 1L, lag >= 1L)
  stopifnot(length(differences) == 1L, differences >= 1L)

  n <- vec_size(x)
  if (lag * differences >= n)
    return(vec_slice(x, 0L))

  out <- x
  for (i in seq_len(differences)) {
    n <- vec_size(out)
    lhs <- (1L + lag):n
    rhs <- 1L:(n - lag)

    out <- vec_slice(out, lhs) - vec_slice(out, rhs)
  }

  out
}


# Modification -------------------------------------------------------------

#' @export
`[[<-.vctrs_vctr` <- function(x, ..., value) {
  if (!is.list(x)) {
    value <- vec_cast(value, x)
  }
  NextMethod()
}

#' @export
`$<-.vctrs_vctr` <- function(x, i, value) {
  if (is.list(x)) {
    NextMethod()
  } else {
    # Default behaviour is to cast LHS to a list
    abort("$ operator is invalid for atomic vectors.")
  }
}

#' @export
`[<-.vctrs_vctr` <- function(x, i, value) {
  value <- vec_cast(value, x)
  NextMethod()
}

#' @export
`names<-.vctrs_vctr` <- function(x, value) {
  if (length(value) != 0 && length(value) != length(x)) {
    abort("`names()` must be the same length as x.")
  }
  if (any_na_names(value)) {
    abort("Names must not be `NA`.")
  }
  NextMethod()
}
# Coercion ----------------------------------------------------------------

#' @export
as.logical.vctrs_vctr <- function(x, ...) {
  vec_cast(x, logical())
}

#' @export
as.integer.vctrs_vctr <- function(x, ...) {
  vec_cast(x, integer())
}

#' @export
as.double.vctrs_vctr <- function(x, ...) {
  vec_cast(x, double())
}

#' @export
as.character.vctrs_vctr <- function(x, ...) {
  vec_cast(x, character())
}

#' @export
as.list.vctrs_vctr <- function(x, ...) {
  out <- vec_chop(x)

  if (vec_is_list(x)) {
    out <- lapply(out, `[[`, 1)
  }

  out
}

#' @export
as.Date.vctrs_vctr <- function(x, ...) {
  vec_cast(x, new_date())
}

#' @export
as.POSIXct.vctrs_vctr <- function(x, tz = "", ...) {
  vec_cast(x, new_datetime(tzone = tz))
}

#' @export
as.POSIXlt.vctrs_vctr <- function(x, tz = "", ...) {
  to <- as.POSIXlt(new_datetime(), tz = tz)
  vec_cast(x, to)
}

# Work around inconsistencies in as.data.frame()
as.data.frame2 <- function(x) {
  # Unclass to avoid dispatching on `as.data.frame()` methods that break size
  # invariants, like `as.data.frame.table()` (#913). This also prevents infinite
  # recursion with shaped vctrs in `as.data.frame.vctrs_vctr()`.
  x <- unclass(x)

  out <- as.data.frame(x)

  if (vec_dim_n(x) == 1) {
    # 1D arrays are not stripped from their dimensions
    out[[1]] <- as.vector(out[[1]])

    # 1D arrays are auto-labelled with substitute()
    names(out) <- "V1"
  }

  out
}

#' @export
as.data.frame.vctrs_vctr <- function(x,
                                     row.names = NULL,
                                     optional = FALSE,
                                     ...,
                                     nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")) {
  force(nm)

  if (has_dim(x)) {
    return(as.data.frame2(x))
  }

  cols <- list(x)
  if (!optional) {
    names(cols) <- nm
  }

  new_data_frame(cols, n = vec_size(x))
}

# Dynamically registered in .onLoad()
as.factor.vctrs_vctr <- function(x, levels = character(), ...) {
  vec_cast(x, new_factor(levels = levels))
}

# Dynamically registered in .onLoad()
as.ordered.vctrs_vctr <- function(x, levels = character(), ...) {
  vec_cast(x, new_ordered(levels = levels))
}

# Dynamically registered in .onLoad()
as.difftime.vctrs_vctr <- function(x, units = "secs", ...) {
  vec_cast(x, new_duration(units = units))
}

# Equality ----------------------------------------------------------------

#' @export
`==.vctrs_vctr` <- function(e1, e2) {
  vec_equal(e1, e2)
}

#' @export
`!=.vctrs_vctr` <- function(e1, e2) {
  !vec_equal(e1, e2)
}

#' @export
is.na.vctrs_vctr <- function(x) {
  vec_equal_na(x)
}

#' @export
anyNA.vctrs_vctr <- if (getRversion() >= "3.2") {
  function(x, recursive = FALSE) {
    any(is.na(x))
  }
} else {
  function(x) {
    any(is.na(x))
  }
}

#' @export
unique.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_unique(x)
}

#' @export
duplicated.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_duplicate_id(x) != seq_along(x)
}

#' @export
anyDuplicated.vctrs_vctr <- function(x, incomparables = FALSE, ...) {
  vec_duplicate_any(x)
}

# Comparison ----------------------------------------------------------------

#' @export
`<=.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) <= 0
}

#' @export
`<.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) < 0
}

#' @export
`>=.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) >= 0
}

#' @export
`>.vctrs_vctr` <- function(e1, e2) {
  vec_compare(e1, e2) > 0
}

#' @export
xtfrm.vctrs_vctr <- function(x) {
  proxy <- vec_proxy_order(x)
  type <- typeof(proxy)

  if (type == "logical") {
    proxy <- unstructure(proxy)
    proxy <- as.integer(proxy)
    return(proxy)
  }

  if (type %in% c("integer", "double")) {
    proxy <- unstructure(proxy)
    return(proxy)
  }

  vec_rank(proxy, ties = "dense", na_propagate = TRUE)
}

#' @importFrom stats median
#' @export
median.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  # nocov start
  stop_unimplemented(x, "median")
  # nocov end
}

#' @importFrom stats quantile
#' @export
quantile.vctrs_vctr <- function(x, ..., type = 1, na.rm = FALSE) {
  # nocov start
  stop_unimplemented(x, "quantile")
  # nocov end
}

vec_cast_or_na <- function(x, to, ...) {
  tryCatch(
    vctrs_error_incompatible_type = function(...) vec_init(to, length(x)),
    vec_cast(x, to)
  )
}

#' @export
min.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  if (vec_is_empty(x)) {
    return(vec_cast_or_na(Inf, x))
  }

  # TODO: implement to do vec_arg_min()
  rank <- xtfrm(x)

  if (isTRUE(na.rm)) {
    idx <- which.min(rank)
    if (vec_is_empty(idx)) {
      return(vec_cast_or_na(Inf, x))
    }
  } else {
    idx <- which(vec_equal(rank, min(rank), na_equal = TRUE))
  }

  x[[idx[[1]]]]
}

#' @export
max.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  if (vec_is_empty(x)) {
    return(vec_cast_or_na(-Inf, x))
  }

  # TODO: implement to do vec_arg_max()
  rank <- xtfrm(x)

  if (isTRUE(na.rm)) {
    idx <- which.max(rank)
    if (vec_is_empty(idx)) {
      return(vec_cast_or_na(-Inf, x))
    }
  } else {
    idx <- which(vec_equal(rank, max(rank), na_equal = TRUE))
  }

  x[[idx[[1]]]]
}

#' @export
range.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  if (vec_is_empty(x)) {
    return(vec_cast_or_na(c(Inf, -Inf), x))
  }

  # Inline `min()` / `max()` to only call `xtfrm()` once
  rank <- xtfrm(x)

  if (isTRUE(na.rm)) {
    idx_min <- which.min(rank)
    idx_max <- which.max(rank)
    if (vec_is_empty(idx_min) && vec_is_empty(idx_max)) {
      return(vec_cast_or_na(c(Inf, -Inf), x))
    }
  } else {
    idx_min <- which(vec_equal(rank, min(rank), na_equal = TRUE))
    idx_max <- which(vec_equal(rank, max(rank), na_equal = TRUE))
  }

  c(x[[idx_min[[1]]]], x[[idx_max[[1]]]])
}

# Numeric -----------------------------------------------------------------

#' @export
Math.vctrs_vctr <- function(x, ...) {
  vec_math(.Generic, x, ...)
}

#' @export
Summary.vctrs_vctr <- function(..., na.rm = FALSE) {
  vec_math(.Generic, vec_c(...), na.rm = na.rm)
}

#' @export
mean.vctrs_vctr <- function(x, ..., na.rm = FALSE) {
  vec_math("mean", x, na.rm = na.rm)
}

#' @export
is.finite.vctrs_vctr <- function(x) {
  vec_math("is.finite", x)
}

#' @export
is.infinite.vctrs_vctr <- function(x) {
  vec_math("is.infinite", x)
}

#' @export
is.nan.vctrs_vctr <- function(x) {
  vec_math("is.nan", x)
}

# Arithmetic --------------------------------------------------------------

#' @export
`+.vctrs_vctr` <- function(e1, e2) {
  if (missing(e2)) {
    vec_arith("+", e1, MISSING())
  } else {
    vec_arith("+", e1, e2)
  }
}

#' @export
`-.vctrs_vctr` <- function(e1, e2) {
  if (missing(e2)) {
    vec_arith("-", e1, MISSING())
  } else {
    vec_arith("-", e1, e2)
  }
}

#' @export
`*.vctrs_vctr` <- function(e1, e2) {
  vec_arith("*", e1, e2)
}

#' @export
`/.vctrs_vctr` <- function(e1, e2) {
  vec_arith("/", e1, e2)
}

#' @export
`^.vctrs_vctr` <- function(e1, e2) {
  vec_arith("^", e1, e2)
}

#' @export
`%%.vctrs_vctr` <- function(e1, e2) {
  vec_arith("%%", e1, e2)
}

#' @export
`%/%.vctrs_vctr` <- function(e1, e2) {
  vec_arith("%/%", e1, e2)
}

#' @export
`!.vctrs_vctr` <- function(x) {
  vec_arith("!", x, MISSING())
}

#' @export
`&.vctrs_vctr` <- function(e1, e2) {
  vec_arith("&", e1, e2)
}

#' @export
`|.vctrs_vctr` <- function(e1, e2) {
  vec_arith("|", e1, e2)
}

# Unimplemented ------------------------------------------------------------

#' @export
summary.vctrs_vctr <- function(object, ...) {
  # nocov start
  stop_unimplemented(object, "summary")
  # nocov end
}

# Unsupported --------------------------------------------------------------

#' @export
`dim<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "dim<-")
}

#' @export
`dimnames<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "dimnames<-")
}

#' @export
levels.vctrs_vctr <- function(x) {
  stop_unsupported(x, "levels")
}

#' @export
`levels<-.vctrs_vctr` <- function(x, value) {
  stop_unsupported(x, "levels<-")
}

#' @export
`t.vctrs_vctr` <- function(x) {
  stop_unsupported(x, "t")
}

#' @export
`is.na<-.vctrs_vctr` <- function(x, value) {
  vec_assign(x, value, vec_init(x))
}

# Helpers -----------------------------------------------------------------

# This simple class is used for testing as defining methods inside
# a test does not work (because the lexical scope is lost)
# nocov start
new_hidden <- function(x = double()) {
  stopifnot(is.numeric(x))
  new_vctr(vec_cast(x, double()), class = "hidden", inherit_base_type = FALSE)
}
format.hidden <- function(x, ...) rep("xxx", length(x))

local_hidden <- function(frame = caller_env()) {
  local_bindings(.env = global_env(), .frame = frame,
    vec_ptype2.hidden.hidden  = function(x, y, ...) new_hidden(),
    vec_ptype2.hidden.double  = function(x, y, ...) new_hidden(),
    vec_ptype2.double.hidden  = function(x, y, ...) new_hidden(),
    vec_ptype2.hidden.logical = function(x, y, ...) new_hidden(),
    vec_ptype2.logical.hidden = function(x, y, ...) new_hidden(),

    vec_cast.hidden.hidden   = function(x, to, ...) x,
    vec_cast.hidden.double   = function(x, to, ...) new_hidden(vec_data(x)),
    vec_cast.double.hidden   = function(x, to, ...) vec_data(x),
    vec_cast.hidden.logical  = function(x, to, ...) new_hidden(as.double(x)),
    vec_cast.logical.hidden  = function(x, to, ...) as.logical(vec_data(x))
  )
}

# nocov end
