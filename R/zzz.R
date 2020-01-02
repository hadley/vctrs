# nocov start
.onLoad <- function(libname, pkgname) {
  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  # Remove once tibble has implemented the methods
  if (is_installed("tibble") && !env_has(ns_env("tibble"), "vec_ptype2.tbl_df")) {
    s3_register("vctrs::vec_ptype2", "tbl_df")
    s3_register("vctrs::vec_ptype2.tbl_df", "default")
    s3_register("vctrs::vec_ptype2.tbl_df", "data.frame")
    s3_register("vctrs::vec_ptype2.data.frame", "tbl_df")

    s3_register("vctrs::tbl_ptype2", "tbl_df")
    s3_register("vctrs::tbl_ptype2.tbl_df", "default")
    s3_register("vctrs::tbl_ptype2.tbl_df", "data.frame")
    s3_register("vctrs::tbl_ptype2.data.frame", "tbl_df")
  }

  utils::globalVariables("vec_set_attributes")

  # Prevent two copies from being made by `attributes(x) <- attrib` on R < 3.6.0
  if (getRversion() >= '3.6.0') {
    vec_set_attributes <- function(x, attrib) {
      attributes(x) <- attrib
      x
    }
  } else {
    vec_set_attributes <- function(x, attrib) {
      .Call(vctrs_set_attributes, x, attrib)
    }
  }

  ns <- ns_env("vctrs")
  env_bind(ns, vec_set_attributes = vec_set_attributes)

  .Call(vctrs_init_library, ns_env())
}

# nocov end
