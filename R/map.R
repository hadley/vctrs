
vec_map <- function(.x, .fn, ..., .ptype = list()) {
  .elt <- NULL # Defined in the mapping loop
  .fn <- as_function(.fn)
  .External(vctrs_map, .x, environment(), .ptype)
}
