new_vctrs_compact_rep_lgl <- function(value, size) {
  value <- vec_assert(value, logical())
  size <- vec_cast(size, double())
  .Call(vctrs_new_vctrs_compact_rep_lgl, value, size)
}

new_vctrs_compact_rep_int <- function(value, size) {
  value <- vec_assert(value, integer())
  size <- vec_cast(size, double())
  .Call(vctrs_new_vctrs_compact_rep_int, value, size)
}

new_vctrs_compact_rep_dbl <- function(value, size) {
  value <- vec_assert(value, double())
  size <- vec_cast(size, double())
  .Call(vctrs_new_vctrs_compact_rep_dbl, value, size)
}

new_vctrs_compact_rep_chr <- function(value, size) {
  value <- vec_assert(value, character())
  size <- vec_cast(size, double())
  .Call(vctrs_new_vctrs_compact_rep_chr, value, size)
}

vec_is_vctrs_compact_rep <- function(x) {
  .Call(vctrs_is_vctrs_compact_rep, x)
}

vec_is_vctrs_compact_rep_compact <- function(x) {
  .Call(vctrs_is_vctrs_compact_rep_compact, x)
}

skip_if_no_altrep <- function() {
  skip_if(getRversion() < "3.5.0", "Testing these ALTREP features requires R 3.5+")
}

skip_if_no_altrep_3_6 <- function() {
  skip_if(getRversion() < "3.6.0", "Testing these ALTREP features requires R 3.6+")
}

skip_on_32_bit <- function() {
  if (.Machine$sizeof.pointer == 4L) {
    skip("On 32-bit arch")
  } else {
    invisible(TRUE)
  }
}

vctrs_compact_rep_test_info <- function() {
  list(
    list(x = 1L,  na = NA_integer_,   ctor = new_vctrs_compact_rep_int),
    list(x = 1,   na = NA_real_,      ctor = new_vctrs_compact_rep_dbl),
    list(x = "1", na = NA_character_, ctor = new_vctrs_compact_rep_chr)
  )
}

vctrs_compact_rep_test_info_3_6 <- function() {
  list(
    list(x = TRUE, na = NA, ctor = new_vctrs_compact_rep_lgl)
  )
}
