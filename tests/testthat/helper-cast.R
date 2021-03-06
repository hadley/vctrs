
expect_lossy_cast <- function(expr) {
  cnd <- NULL

  out <- with_handlers(
    warning = calling(function(x) {
      cnd <<- x
      cnd_muffle(x)
    }),
    expr
  )
  expect_s3_class(cnd, "vctrs_warning_cast_lossy")

  out
}
