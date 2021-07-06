#' Arithmetic operations
#'
#' This generic provides a common double dispatch mechanism for all infix
#' operators (`+`, `-`, `/`, `*`, `^`, `%%`, `%/%`, `!`, `&`, `|`). It is used
#' to power the default arithmetic and boolean operators for [vctr]s objects,
#' overcoming the limitations of the base [Ops] generic.
#'
#' `vec_arith_base()` is provided as a convenience for writing methods. It
#' recycles `x` and `y` to common length then calls the base operator with the
#' underlying [vec_data()].
#'
#' `vec_arith()` is also used in `diff.vctrs_vctr()` method via `-`.
#'
#' @param op An arithmetic operator as a string
#' @param x,y A pair of vectors. For `!`, unary `+` and unary `-`, `y` will be
#'   a sentinel object of class `MISSING`, as created by `MISSING()`.
#' @inheritParams ellipsis::dots_empty
#'
#' @seealso [stop_incompatible_op()] for signalling that an arithmetic
#'   operation is not permitted/supported.
#' @seealso See [vec_math()] for the equivalent for the unary mathematical
#'   functions.
#' @export
#' @keywords internal
#' @examples
#' d <- as.Date("2018-01-01")
#' dt <- as.POSIXct("2018-01-02 12:00")
#' t <- as.difftime(12, unit = "hours")
#'
#' vec_arith("-", dt, 1)
#' vec_arith("-", dt, t)
#' vec_arith("-", dt, d)
#'
#' vec_arith("+", dt, 86400)
#' vec_arith("+", dt, t)
#' vec_arith("+", t, t)
#'
#' vec_arith("/", t, t)
#' vec_arith("/", t, 2)
#'
#' vec_arith("*", t, 2)
vec_arith <- function(op, x, y, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  UseMethod("vec_arith", x)
}

#' @export
#' @rdname vec_arith
vec_arith.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

# Atomic vectors ----------------------------------------------------------

#' @rdname vec_arith
#' @export vec_arith.logical
#' @method vec_arith logical
#' @export
vec_arith.logical <- function(op, x, y, ...) UseMethod("vec_arith.logical", y)
#' @method vec_arith.logical default
#' @export
vec_arith.logical.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
#' @method vec_arith.logical logical
#' @export
vec_arith.logical.logical <- function(op, x, y, ...) vec_arith_base(op, x, y)
#' @method vec_arith.logical integer
#' @export
vec_arith.logical.integer <- function(op, x, y, ...) vec_arith_base(op, x, y)
#' @method vec_arith.logical double
#' @export
vec_arith.logical.double <- function(op, x, y, ...) vec_arith_base(op, x, y)

# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.integer.integer <- function(op, x, y, ...) vec_arith_base(op, x, y)
# #' @export
# vec_arith.integer.double <- function(op, x, y, ...) vec_arith_base(op, x, y)
# #' @export
# vec_arith.integer.logical <- function(op, x, y, ...) vec_arith_base(op, x, y)

# FIXME: Uncomment after custom double dispatch
# #' @export
# vec_arith.double.double <- function(op, x, y, ...) vec_arith_base(op, x, y)
# #' @export
# vec_arith.double.integer <- function(op, x, y, ...) vec_arith_base(op, x, y)
# #' @export
# vec_arith.double.logical <- function(op, x, y, ...) vec_arith_base(op, x, y)

#' @rdname vec_arith
#' @export vec_arith.numeric
#' @method vec_arith numeric
#' @export
vec_arith.numeric <- function(op, x, y, ...) UseMethod("vec_arith.numeric", y)
# FIXME: Remove numeric method after custom double dispatch
#' @method vec_arith.numeric default
#' @export
vec_arith.numeric.default <- function(op, x, y, ...) stop_incompatible_op(op, x, y)
# FIXME: Remove numeric method after custom double dispatch
#' @method vec_arith.numeric logical
#' @export
vec_arith.numeric.logical <- function(op, x, y, ...) vec_arith_base(op, x, y)
# FIXME: Remove numeric method after custom double dispatch
#' @method vec_arith.numeric numeric
#' @export
vec_arith.numeric.numeric <- function(op, x, y, ...) vec_arith_base(op, x, y)

# Helpers -----------------------------------------------------------------

#' @export
#' @rdname vec_arith
vec_arith_base <- function(op, x, y) {
  args <- vec_recycle_common(x, y)

  op_fn <- getExportedValue("base", op)
  op_fn(vec_data(args[[1L]]), vec_data(args[[2L]]))
}

#' @export
#' @rdname vec_arith
MISSING <- function() {
  structure(list(), class = "MISSING")
}
