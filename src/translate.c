#include "vctrs.h"
#include "utils.h"

// -----------------------------------------------------------------------------

// UTF-8 translation will be successful in these cases:
// - (utf8 + latin1), (unknown + utf8), (unknown + latin1)
// UTF-8 translation will fail purposefully in these cases:
// - (bytes + utf8), (bytes + latin1), (bytes + unknown)
// UTF-8 translation is not attempted in these cases:
// - (utf8 + utf8), (latin1 + latin1), (unknown + unknown), (bytes + bytes)

static bool chr_translation_required_impl(const SEXP* x, R_len_t size, cetype_t reference) {
  for (R_len_t i = 0; i < size; ++i, ++x) {
    if (Rf_getCharCE(*x) != reference) {
      return true;
    }
  }

  return false;
}

// [[ include("vctrs.h") ]]
bool chr_translation_required(SEXP x, R_len_t size) {
  if (size == 0) {
    return false;
  }

  const SEXP* p_x = STRING_PTR_RO(x);
  cetype_t reference = Rf_getCharCE(*p_x);

  return chr_translation_required_impl(p_x, size, reference);
}

// Check if `x` or `y` need to be translated to UTF-8, relative to each other
static bool chr_translation_required2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  const SEXP* p_x;
  const SEXP* p_y;

  bool x_empty = x_size == 0;
  bool y_empty = y_size == 0;

  if (x_empty && y_empty) {
    return false;
  }

  if (x_empty) {
    p_y = STRING_PTR_RO(y);
    return chr_translation_required_impl(p_y, y_size, Rf_getCharCE(*p_y));
  }

  if (y_empty) {
    p_x = STRING_PTR_RO(x);
    return chr_translation_required_impl(p_x, x_size, Rf_getCharCE(*p_x));
  }

  p_x = STRING_PTR_RO(x);
  cetype_t reference = Rf_getCharCE(*p_x);

  if (chr_translation_required_impl(p_x, x_size, reference)) {
    return true;
  }

  p_y = STRING_PTR_RO(y);

  if (chr_translation_required_impl(p_y, y_size, reference)) {
    return true;
  }

  return false;
}

// -----------------------------------------------------------------------------
// Utilities required for checking if any character elements of a list have a
// "known" encoding. This implies that we have to convert all character
// elements of the list to UTF-8. This function is solely used by
// `translate_encoding_list2()`.

static bool chr_any_known_encoding(SEXP x, R_len_t size);
static bool list_any_known_encoding(SEXP x, R_len_t size);
static bool df_any_known_encoding(SEXP x, R_len_t size);

static bool obj_any_known_encoding(SEXP x, R_len_t size) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_character: return chr_any_known_encoding(x, size);
  case vctrs_type_list: return list_any_known_encoding(x, size);
  case vctrs_type_dataframe: return df_any_known_encoding(x, size);
  default: return false;
  }
}

static bool chr_any_known_encoding(SEXP x, R_len_t size) {
  if (size == 0) {
    return false;
  }

  const SEXP* p_x = STRING_PTR_RO(x);

  for (int i = 0; i < size; ++i, ++p_x) {
    if (Rf_getCharCE(*p_x) != CE_NATIVE) {
      return true;
    }
  }

  return false;
}

static bool list_any_known_encoding(SEXP x, R_len_t size) {
  SEXP elt;

  for (int i = 0; i < size; ++i) {
    elt = VECTOR_ELT(x, i);

    if (obj_any_known_encoding(elt, vec_size(elt))) {
      return true;
    }
  }

  return false;
}

static bool df_any_known_encoding(SEXP x, R_len_t size) {
  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    if (obj_any_known_encoding(VECTOR_ELT(x, i), size)) {
      return true;
    }
  }

  return false;
}

// -----------------------------------------------------------------------------
// Utilities required for translating the character vector elements of a list
// to UTF-8. This function is solely used by `translate_encoding_list2()`.

static SEXP chr_translate_encoding(SEXP x, R_len_t size);
static SEXP list_translate_encoding(SEXP x, R_len_t size);
static SEXP df_translate_encoding(SEXP x, R_len_t size);

static SEXP obj_translate_encoding(SEXP x, R_len_t size) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_character: return chr_translate_encoding(x, size);
  case vctrs_type_list: return list_translate_encoding(x, size);
  case vctrs_type_dataframe: return df_translate_encoding(x, size);
  default: return x;
  }
}

static SEXP chr_translate_encoding(SEXP x, R_len_t size) {
  if (size == 0) {
    return x;
  }

  const SEXP* p_x = STRING_PTR_RO(x);

  SEXP out = PROTECT(Rf_allocVector(STRSXP, size));
  SEXP* p_out = STRING_PTR(out);

  SEXP chr;
  const void *vmax = vmaxget();

  for (int i = 0; i < size; ++i, ++p_x, ++p_out) {
    chr = *p_x;

    if (Rf_getCharCE(chr) == CE_UTF8) {
      *p_out = chr;
      continue;
    }

    *p_out = Rf_mkCharCE(Rf_translateCharUTF8(chr), CE_UTF8);
  }

  vmaxset(vmax);
  UNPROTECT(1);
  return out;
}

static SEXP list_translate_encoding(SEXP x, R_len_t size) {
  SEXP elt;
  x = PROTECT(r_maybe_duplicate(x));

  for (int i = 0; i < size; ++i) {
    elt = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, obj_translate_encoding(elt, vec_size(elt)));
  }

  UNPROTECT(1);
  return x;
}

static SEXP df_translate_encoding(SEXP x, R_len_t size) {
  SEXP col;
  x = PROTECT(r_maybe_duplicate(x));

  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    col = VECTOR_ELT(x, i);
    SET_VECTOR_ELT(x, i, obj_translate_encoding(col, size));
  }

  UNPROTECT(1);
  return x;
}

// -----------------------------------------------------------------------------

static SEXP translate_none(SEXP x, SEXP y);
static SEXP chr_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);
static SEXP list_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);
static SEXP df_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size);

// Notes:
// - Assumes that `x` and `y` are the same type from calling `vec_cast()`.
// - Does not assume that `x` and `y` are the same size.
// - Returns a list holding `x` and `y` translated to their common encoding.

// [[ include("vctrs.h") ]]
SEXP obj_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  switch (vec_proxy_typeof(x)) {
  case vctrs_type_character: return chr_translate_encoding2(x, x_size, y, y_size);
  case vctrs_type_list: return list_translate_encoding2(x, x_size, y, y_size);
  case vctrs_type_dataframe: return df_translate_encoding2(x, x_size, y, y_size);
  default: return translate_none(x, y);
  }
}

static SEXP translate_none(SEXP x, SEXP y) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(1);
  return out;
}

static SEXP chr_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (chr_translation_required2(x, x_size, y, y_size)) {
    SET_VECTOR_ELT(out, 0, chr_translate_encoding(x, x_size));
    SET_VECTOR_ELT(out, 1, chr_translate_encoding(y, y_size));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP list_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  if (list_any_known_encoding(x, x_size) || list_any_known_encoding(y, y_size)) {
    SET_VECTOR_ELT(out, 0, list_translate_encoding(x, x_size));
    SET_VECTOR_ELT(out, 1, list_translate_encoding(y, y_size));
  } else {
    SET_VECTOR_ELT(out, 0, x);
    SET_VECTOR_ELT(out, 1, y);
  }

  UNPROTECT(1);
  return out;
}

static SEXP df_translate_encoding2(SEXP x, R_len_t x_size, SEXP y, R_len_t y_size) {
  SEXP x_elt;
  SEXP y_elt;
  SEXP translated;

  x = PROTECT(r_maybe_duplicate(x));
  y = PROTECT(r_maybe_duplicate(y));

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));

  int n_col = Rf_length(x);

  for (int i = 0; i < n_col; ++i) {
    x_elt = VECTOR_ELT(x, i);
    y_elt = VECTOR_ELT(y, i);

    translated = PROTECT(obj_translate_encoding2(x_elt, x_size, y_elt, y_size));

    SET_VECTOR_ELT(x, i, VECTOR_ELT(translated, 0));
    SET_VECTOR_ELT(y, i, VECTOR_ELT(translated, 1));

    UNPROTECT(1);
  }

  SET_VECTOR_ELT(out, 0, x);
  SET_VECTOR_ELT(out, 1, y);

  UNPROTECT(3);
  return out;
}
