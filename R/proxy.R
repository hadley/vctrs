#' Extract underlying data
#'
#' @description
#'
#' Extract the data underlying an S3 vector object, i.e. the underlying
#' (named) atomic vector or list.
#'
#' * `vec_data()` returns unstructured data. The only attributes
#'   preserved are names, dims, and dimnames.
#'
#'   Currently, due to the underlying memory architecture of R, this
#'   creates a full copy of the data.
#'
#' * `vec_proxy()` may return structured data. This generic is the
#'   main customisation point in vctrs, along with [vec_restore()].
#'   See the section below to learn when you should implement
#'   `vec_proxy()`.
#'
#'   Methods must return a vector type. Records and data frames will
#'   be processed rowwise.
#'
#' @param x A vector or object implementing `vec_proxy()`.
#' @return The data underlying `x`, free from any attributes except the names.
#'
#' @section When should you proxy your type:
#'
#' You should only implement `vec_proxy()` when your type is designed
#' around a non-vector class. I.e. anything that is not either:
#'
#' * An atomic vector
#' * A bare list
#' * A data frame
#'
#' In this case, implement `vec_proxy()` to return such a vector
#' class. The vctrs operations such as [vec_slice()] are applied on
#' the proxy and `vec_restore()` is called to restore the original
#' representation of your type.
#'
#' The most common case where you need to implement `vec_proxy()` is
#' for S3 lists. In vctrs, S3 lists are treated as scalars by
#' default. This way we don't treat objects like model fits as
#' vectors. To prevent vctrs from treating your S3 list as a scalar,
#' unclass it in the `vec_proxy()` method. For instance, here is the
#' definition for `list_of`:
#'
#' ```
#' vec_proxy.vctrs_list_of <- function(x) {
#'   unclass(x)
#' }
#' ```
#'
#' Another case where you need to implement a proxy is [record
#' types][new_rcrd]. Record types should return a data frame, as in
#' the `POSIXlt` method:
#'
#' ```
#' vec_proxy.POSIXlt <- function(x) {
#'   new_data_frame(unclass(x))
#' }
#' ```
#'
#' Note that you don't need to implement `vec_proxy()` when your class
#' inherits from `vctrs_vctr` or `vctrs_rcrd`.
#'
#' @seealso See [vec_restore()] for the inverse operation: it restores
#'   attributes given a bare vector and a prototype;
#'   `vec_restore(vec_data(x), x)` will always yield `x`.
#' @export
vec_data <- function(x) {
  vec_assert(x)
  x <- vec_proxy(x)

  if (has_dim(x)) {
    x <- vec_set_attributes(x, list(dim = dim(x), dimnames = dimnames(x)))
  } else {
    x <- vec_set_attributes(x, list(names = names(x)))
  }

  x
}
#' @rdname vec_data
#' @inheritParams ellipsis::dots_empty
#' @export
vec_proxy <- function(x, ...) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_proxy, x))
  UseMethod("vec_proxy")
}
vec_proxy_dispatch <- function(x, ...) {
  UseMethod("vec_proxy")
}
#' @export
vec_proxy.default <- function(x, ...) {
  x
}

vec_proxy_recursive <- function(x, kind = "default") {
  .Call(vctrs_proxy_recursive, x, sym(kind))
}

#' @export
#' @rdname vec_cast
vec_restore <- function(x, to, ..., n = NULL) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  return(.Call(vctrs_restore, x, to, n))
  UseMethod("vec_restore", to)
}
vec_restore_dispatch <- function(x, to, ..., n = NULL) {
  UseMethod("vec_restore", to)
}
#' @export
vec_restore.default <- function(x, to, ..., n = NULL) {
  .Call(vctrs_restore_default, x, to)
}


vec_proxy_push_vcols <- function(x, ...) {
  stopifnot(is.data.frame(x))
  size <- nrow(x)

  # Prevent S3 dispatch
  x <- as.list(x)

  vcols <- list2(...)
  stopifnot(every(vcols, function(x) vec_size(x) == size))

  n <- length(x)
  has_vcols <- length(x) && identical(names(x)[[n]], "vctrs::virtual_cols")
  if (has_vcols) {
    vcols <- c(x[[n]], vcols)
    x <- x[-n]
  }

  x <- c(x, list(`vctrs::virtual_cols` = new_data_frame(vcols)))
  new_data_frame(x)
}

vec_proxy_pop_vcols <- function(x, names) {
  stopifnot(is.data.frame(x))

  # Prevent S3 dispatch
  x <- as.list(x)

  n <- length(x)
  has_vcols <- identical(names(x)[[n]], "vctrs::virtual_cols")

  if (!has_vcols) {
    abort("Internal error: Expected virtual column in proxy.")
  }

  # Transform to list to prevent deduplication of column names
  vcols <- as.list(x[[n]])

  ind <- names(vcols) == names
  if (!any(ind)) {
    abort("Internal error: Can't find virtual column in proxy.")
  }

  out <- vcols[ind]

  # If no vcol left, pop the vcols column from the proxy. Otherwise,
  # pop the relevant vcols.
  if (all(ind)) {
    x <- x[-n]
  } else {
    x[[n]] <- new_data_frame(vcols[!ind])
  }

  data_frame(
    proxy = new_data_frame(x),
    vcols = new_data_frame(out)
  )
}
