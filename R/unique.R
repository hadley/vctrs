vec_unique_loc2 <- function(x) {
  .Call(vctrs_unique_loc2, x)
}

vec_unique2 <- function(x) {
  .Call(vctrs_unique2, x)
}

vec_unique_count2 <- function(x) {
  .Call(vctrs_unique_count2, x)
}
