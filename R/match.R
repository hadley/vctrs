vec_matches <- function(needles,
                        haystack,
                        ...,
                        condition = "==",
                        na_equal = TRUE,
                        no_match = NA_integer_,
                        multiple = "all",
                        nan_distinct = FALSE,
                        chr_transform = NULL,
                        needles_arg = "",
                        haystack_arg = "") {
  if (!missing(...)) {
    check_dots_empty()
  }

  .Call(
    vctrs_matches,
    needles,
    haystack,
    condition,
    na_equal,
    no_match,
    multiple,
    nan_distinct,
    chr_transform,
    needles_arg,
    haystack_arg
  )
}

# ------------------------------------------------------------------------------

stop_matches <- function(class = NULL, ...) {
  stop_vctrs(
    class = c(class, "vctrs_error_matches"),
    ...
  )
}

# ------------------------------------------------------------------------------

stop_matches_nothing <- function(i, needles_arg, haystack_arg) {
  stop_matches(
    class = "vctrs_error_matches_nothing",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg
  )
}

#' @export
cnd_header.vctrs_error_matches_nothing <- function(cnd, ...) {
  if (nzchar(cnd$needles_arg)) {
    needles_name <- glue::glue(" of `{cnd$needles_arg}` ")
  } else {
    needles_name <- " "
  }

  if (nzchar(cnd$haystack_arg)) {
    haystack_name <- glue::glue(" in `{cnd$haystack_arg}`")
  } else {
    haystack_name <- ""
  }

  glue::glue("Each element{needles_name}must have a match{haystack_name}.")
}

#' @export
cnd_body.vctrs_error_matches_nothing <- function(cnd, ...) {
  bullet <- glue::glue("The element at location {cnd$i} does not have a match.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

# ------------------------------------------------------------------------------

stop_matches_multiple <- function(i, needles_arg, haystack_arg) {
  stop_matches(
    class = "vctrs_error_matches_multiple",
    i = i,
    needles_arg = needles_arg,
    haystack_arg = haystack_arg
  )
}

#' @export
cnd_header.vctrs_error_matches_multiple <- function(cnd, ...) {
  if (nzchar(cnd$needles_arg)) {
    needles_name <- glue::glue(" of `{cnd$needles_arg}` ")
  } else {
    needles_name <- " "
  }

  if (nzchar(cnd$haystack_arg)) {
    haystack_name <- glue::glue(" from `{cnd$haystack_arg}`")
  } else {
    haystack_name <- ""
  }

  glue::glue("Each element{needles_name}can match at most 1 observation{haystack_name}.")
}

#' @export
cnd_body.vctrs_error_matches_multiple <- function(cnd, ...) {
  bullet <- glue::glue("The element at location {cnd$i} has multiple matches.")
  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}
