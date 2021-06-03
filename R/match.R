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
