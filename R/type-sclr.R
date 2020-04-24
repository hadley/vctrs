new_sclr <- function(..., class = character()) {
  fields <- list(...)
  stopifnot(has_unique_names(fields))

  structure(
    list(...),
    class = c(class, "vctrs_sclr")
  )
}

# Subsetting --------------------------------------------------------------

#' @export
`[[.vctrs_sclr` <- function(x, i, ...) {
  .Call(vctrs_list_get, x, i)
}

#' @export
`$.vctrs_sclr` <- function(x, i, ...) {
  .Call(vctrs_list_get, x, i)
}

#' @export
`[[<-.vctrs_sclr` <- function(x, i, value) {
  .Call(vctrs_list_set, x, i, value)
}

#' @export
`$<-.vctrs_sclr` <- function(x, i, value) {
  .Call(vctrs_list_set, x, i, value)
}

# Shared niceties ---------------------------------------------------------

#' @export
print.vctrs_sclr <- function(x, ...) {
  obj_print(x, ...)
  invisible(x)
}

#' @export
as.list.vctrs_sclr <- function(x, ...) {
  vec_set_attributes(x, list(names = names(x)))
}

#' @export
as.data.frame.vctrs_sclr <- function(x,
                                     row.names = NULL,
                                     optional = FALSE,
                                     ...,
                                     nm = paste(deparse(substitute(x), width.cutoff = 500L), collapse = " ")
                                     ) {

  force(nm)
  cols <- list(list(x))
  if (!optional) {
    names(cols) <- nm
  }

  new_data_frame(cols, .size = 1L)
}

# Vector behaviours -------------------------------------------------------

#' @export
`[.vctrs_sclr` <- function(x, ...) {
  stop_unsupported(x, "[")
}

#' @export
`[<-.vctrs_sclr` <- function(x, ..., value) {
  stop_unsupported(x, "[<-")
}

#' @export
c.vctrs_sclr <- function(...) {
  stop_unsupported(..1, "c")
}

#' @export
Math.vctrs_sclr <- function(x, ...) {
  stop_unsupported(x, .Generic)
}

#' @export
Ops.vctrs_sclr <- function(e1, e2) {
  stop_unsupported(e1, .Generic)
}

#' @export
Complex.vctrs_sclr <- function(z) {
  stop_unsupported(z, .Generic)
}

#' @export
Summary.vctrs_sclr <- function(..., na.rm = TRUE) {
  stop_unsupported(..1, .Generic)
}

#' @export
`names<-.vctrs_sclr` <- function(x, value) {
  stop_unsupported(x, "names<-")
}

#' @export
xtfrm.vctrs_sclr <- function(x) {
  stop_unsupported(x, "xtfrm")
}

#' @export
`dim<-.vctrs_sclr` <- function(x, value) {
  stop_unsupported(x, "dim<-")
}

#' @export
`dimnames<-.vctrs_sclr` <- function(x, value) {
  stop_unsupported(x, "dimnames<-")
}

#' @export
levels.vctrs_sclr <- function(x) {
  stop_unsupported(x, "levels")
}

#' @export
`levels<-.vctrs_sclr` <- function(x, value) {
  stop_unsupported(x, "levels<-")
}

#' @export
`t.vctrs_sclr` <- function(x) {
  stop_unsupported(x, "t")
}

#' @export
`is.na<-.vctrs_sclr` <- function(x, value) {
  stop_unsupported(x, "is.na<-")
}

#' @export
unique.vctrs_sclr <- function(x, incomparables = FALSE, ...) {
  stop_unsupported(x, "unique")
}

#' @export
duplicated.vctrs_sclr <- function(x, incomparables = FALSE, ...) {
  stop_unsupported(x, "unique")
}

#' @export
anyDuplicated.vctrs_sclr <- function(x, incomparables = FALSE, ...) {
  stop_unsupported(x, "unique")
}

#' @export
as.logical.vctrs_sclr <- function(x, ...) {
  stop_unsupported(x, "as.logical")
}

#' @export
as.integer.vctrs_sclr <- function(x, ...) {
  stop_unsupported(x, "as.integer")
}

#' @export
as.double.vctrs_sclr <- function(x, ...) {
  stop_unsupported(x, "as.double")
}

#' @export
as.character.vctrs_sclr <- function(x, ...) {
  stop_unsupported(x, "as.character")
}

#' @export
as.Date.vctrs_sclr <- function(x, ...) {
  stop_unsupported(x, "as.Date")
}

#' @export
as.POSIXct.vctrs_sclr <- function(x, tz = "", ...) {
  stop_unsupported(x, "as.POSIXct")
}


# Unimplemented -----------------------------------------------------------

#' @export
summary.vctrs_sclr <- function(object, ...) {
  # nocov start
  stop_unimplemented(object, "summary")
  # nocov end
}

