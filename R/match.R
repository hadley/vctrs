vec_matches <- function(needles,
                        haystack,
                        ...,
                        condition = "==",
                        na_equal = TRUE,
                        no_match = NA_integer_,
                        multiple = "all",
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
    needles_arg,
    haystack_arg
  )
}
