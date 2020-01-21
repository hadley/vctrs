#' Retrieve and repair names
#'
#' @description
#'
#' `vec_as_names()` takes a character vector of names and repairs it
#' according to the `repair` argument. It is the r-lib and tidyverse
#' equivalent of [base::make.names()].
#'
#' vctrs deals with a few levels of name repair:
#'
#' * `minimal` names exist. The `names` attribute is not `NULL`. The
#'   name of an unnamed element is `""` and never `NA`. For instance,
#'   `vec_as_names()` always returns minimal names and data frames
#'   created by the tibble package have names that are, at least,
#'   `minimal`.
#'
#' * `unique` names are `minimal`, have no duplicates, and can be used
#'   where a variable name is expected. Empty names, `...`, and
#'   `..` followed by a sequence of digits are banned.
#'
#'   - All columns can be accessed by name via `df[["name"]]` and
#'     ``df$`name` `` and ``with(df, `name`)``.
#'
#' * `universal` names are `unique` and syntactic (see Details for
#'   more).
#'
#'   - Names work everywhere, without quoting: `df$name` and `with(df,
#'     name)` and `lm(name1 ~ name2, data = df)` and
#'     `dplyr::select(df, name)` all work.
#'
#' `universal` implies `unique`, `unique` implies `minimal`. These
#' levels are nested.
#'
#'
#' @param names A character vector.
#' @param repair Either a string or a function. If a string, it must
#'   be one of `"check_unique"`, `"minimal"`, `"unique"`, or `"universal"`.
#'   If a function, it is invoked with a vector of minimal names and must
#'   return minimal names, otherwise an error is thrown.
#'
#'   * Minimal names are never `NULL` or `NA`. When an element doesn't
#'     have a name, its minimal name is an empty string.
#'
#'   * Unique names are unique. A suffix is appended to duplicate
#'     names to make them unique.
#'
#'   * Universal names are unique and syntactic, meaning that you can
#'     safely use the names as variables without causing a syntax
#'     error.
#'
#'   The `"check_unique"` option doesn't perform any name repair.
#'   Instead, an error is raised if the names don't suit the
#'   `"unique"` criteria.
#' @param quiet By default, the user is informed of any renaming
#'   caused by repairing the names. This only concerns unique and
#'   universal repairing. Set `quiet` to `TRUE` to silence the
#'   messages.
#' @inheritParams ellipsis::dots_empty
#'
#' @section `minimal` names:
#'
#' `minimal` names exist. The `names` attribute is not `NULL`. The
#' name of an unnamed element is `""` and never `NA`.
#'
#' Examples:
#'
#' ```
#' Original names of a vector with length 3: NULL
#'                            minimal names: "" "" ""
#'
#'                           Original names: "x" NA
#'                            minimal names: "x" ""
#' ```
#'
#'
#' @section `unique` names:
#'
#' `unique` names are `minimal`, have no duplicates, and can be used
#'  (possibly with backticks) in contexts where a variable is
#'  expected. Empty names, `...`, and `..` followed by a sequence of
#'  digits are banned. If a data frame has `unique` names, you can
#'  index it by name, and also access the columns by name. In
#'  particular, `df[["name"]]` and `` df$`name` `` and also ``with(df,
#'  `name`)`` always work.
#'
#' There are many ways to make names `unique`. We append a suffix of the form
#' `...j` to any name that is `""` or a duplicate, where `j` is the position.
#' We also change `..#` and `...` to `...#`.
#'
#' Example:
#'
#' ```
#' Original names:     ""     "x"     "" "y"     "x"  "..2"  "..."
#'   unique names: "...1" "x...2" "...3" "y" "x...5" "...6" "...7"
#' ```
#'
#' Pre-existing suffixes of the form `...j` are always stripped, prior
#' to making names `unique`, i.e. reconstructing the suffixes. If this
#' interacts poorly with your names, you should take control of name
#' repair.
#'
#'
#' @section `universal` names:
#'
#' `universal` names are `unique` and syntactic, meaning they:
#'
#'   * Are never empty (inherited from `unique`).
#'   * Have no duplicates (inherited from `unique`).
#'   * Are not `...`. Do not have the form `..i`, where `i` is a
#'     number (inherited from `unique`).
#'   * Consist of letters, numbers, and the dot `.` or underscore `_`
#'     characters.
#'   * Start with a letter or start with the dot `.` not followed by a
#'     number.
#'   * Are not a [reserved] word, e.g., `if` or `function` or `TRUE`.
#'
#' If a vector has `universal` names, variable names can be used
#' "as is" in code. They work well with nonstandard evaluation, e.g.,
#' `df$name` works.
#'
#' vctrs has a different method of making names syntactic than
#' [base::make.names()]. In general, vctrs prepends one or more dots
#' `.` until the name is syntactic.
#'
#' Examples:
#'
#' ```
#'  Original names:     ""     "x"    NA      "x"
#' universal names: "...1" "x...2" "...3" "x...4"
#'
#'   Original names: "(y)"  "_z"  ".2fa"  "FALSE"
#'  universal names: ".y." "._z" "..2fa" ".FALSE"
#' ```
#'
#' @seealso [rlang::names2()] returns the names of an object, after
#'   making them `minimal`.
#'
#' The [Names attribute](https://principles.tidyverse.org/names-attribute.html)
#' section in the "tidyverse package development principles".
#'
#' @examples
#' # By default, `vec_as_names()` returns minimal names:
#' vec_as_names(c(NA, NA, "foo"))
#'
#' # You can make them unique:
#' vec_as_names(c(NA, NA, "foo"), repair = "unique")
#'
#' # Universal repairing fixes any non-syntactic name:
#' vec_as_names(c("_foo", "+"), repair = "universal")
#' @export
vec_as_names <- function(names,
                         ...,
                         repair = c("minimal", "unique", "universal", "check_unique"),
                         quiet = FALSE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  repair <- validate_repair(repair)

  if (is_function(repair)) {
    names <- as_minimal_names(names)
    new_names <- validate_minimal(repair(names), n = length(names))

    if (!quiet) {
      describe_repair(names, new_names)
    }

    return(new_names)
  }

  switch(repair,
    minimal = as_minimal_names(names),
    unique = as_unique_names(names, quiet = quiet),
    universal = as_universal_names(as_minimal_names(names), quiet = quiet),
    check_unique = validate_unique(names)
  )
}

validate_repair <- function(repair = c("minimal", "unique", "universal", "check_unique")) {
  if (is_formula(repair, scoped = TRUE, lhs = FALSE)) {
    repair <- as_function(repair)
  }

  if (is_function(repair)) {
    repair
  } else {
    arg_match(repair)
  }
}
validate_minimal <- function(names, n = NULL) {
  if (is.null(names)) {
    abort("Names repair functions can't return `NULL`.")
  }
  if (!is_character(names)) {
    abort("Names repair functions must return a character vector.")
  }
  if (!is_null(n) && length(names) != n) {
    abort(sprintf(
      "Repaired names have length %d instead of length %d.",
      length(names), n
    ))
  }
  if (anyNA(names)) {
    abort("Names repair functions can't return `NA` values.")
  }
  names
}
validate_unique <- function(names, n = NULL) {
  validate_minimal(names, n)

  empty_names <- which(names == "")
  if (has_length(empty_names)) {
    stop_names_cannot_be_empty(empty_names)
  }

  dot_dot_name <- grep("^[.][.](?:[.]|[1-9][0-9]*)$", names)
  if (has_length(dot_dot_name)) {
    stop_names_cannot_be_dot_dot(dot_dot_name)
  }

  if (anyDuplicated(names)) {
    stop_names_must_be_unique(which(duplicated(names)))
  }

  invisible(names)
}

#' Extract repaired names from a vector
#'
#' Returns the repaired names from a vector, even if the vector is unnamed.
#'
#' @param x A vector with names
#' @inheritParams vec_as_names
#'
#' @return The names of x, repaired
#' @noRd
vec_names2 <- function(x,
                       ...,
                       repair = c("minimal", "unique", "universal", "check_unique"),
                       quiet = FALSE) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  repair <- validate_repair(repair)

  if (is_function(repair)) {
    names <- minimal_names(x)
    new_names <- validate_minimal(repair(names), n = length(names))

    if (!quiet) {
      describe_repair(names, new_names)
    }

    return(new_names)
  }

  switch(repair,
    minimal = minimal_names(x),
    unique = unique_names(x, quiet = quiet),
    universal = as_universal_names(minimal_names(x), quiet = quiet),
    check_unique = validate_unique(minimal_names(x))
  )
}
vec_repair_names <- function(x,
                             repair = c("minimal", "unique", "universal", "check_unique"),
                             ...,
                             quiet = FALSE) {
  set_names2(x, vec_names2(x, ..., repair = repair, quiet = quiet))
}

minimal_names <- function(x) {
  .Call(vctrs_minimal_names, x)
}
unique_names <- function(x, quiet = FALSE) {
  .Call(vctrs_unique_names, x, quiet)
}

vec_names <- function(x) {
  .Call(vctrs_names, x)
}
`vec_names<-` <- function(x, value) {
  if (is.data.frame(x)) {
    # Do not update row names
  } else if (vec_dim_n(x) == 1) {
    names(x) <- value
  } else {
    rownames(x) <- value
  }
  x
}
set_names2 <- `vec_names<-`

as_minimal_names <- function(names) {
  .Call(vctrs_as_minimal_names, names)
}
as_unique_names <- function(names, quiet = FALSE) {
  .Call(vctrs_as_unique_names, names, quiet)
}
as_universal_names <- function(names, quiet = FALSE) {
  new_names <- names
  new_names[] <- ""

  naked_names <- strip_pos(two_to_three_dots(names))
  empty <- naked_names %in% c("", "...")

  new_names[!empty] <- make_syntactic(naked_names[!empty])

  needs_suffix <- empty | vec_duplicate_flg(new_names, first = TRUE)
  new_names <- append_pos(new_names, needs_suffix = needs_suffix)

  if (!quiet) {
    describe_repair(names, new_names)
  }

  new_names
}

two_to_three_dots <- function(names) {
  sub("(^[.][.][1-9][0-9]*$)", ".\\1", names)
}
append_pos <- function(names, needs_suffix) {
  need_append_pos <- which(needs_suffix)
  names[need_append_pos] <- paste0(names[need_append_pos], "...", need_append_pos)
  names
}
strip_pos <- function(names) {
  rx <- "([.][.][.][1-9][0-9]*)+$"
  gsub(rx, "", names) %|% ""
}

# Makes each individual name syntactic but does not enforce unique-ness
make_syntactic <- function(names) {
  names[is.na(names)]       <- ""
  names[names == ""]        <- "."
  names[names == "..."]     <- "...."
  names <- sub("^_", "._", names)

  new_names <- make.names(names)

  X_prefix <- grepl("^X", new_names) & !grepl("^X", names)
  new_names[X_prefix] <- sub("^X", "", new_names[X_prefix])

  dot_suffix <- which(new_names == paste0(names, "."))
  new_names[dot_suffix] <- sub("^(.*)[.]$", ".\\1", new_names[dot_suffix])
  # Illegal characters have been replaced with '.' via make.names()
  # however, we have:
  #   * Declined its addition of 'X' prefixes.
  #   * Turned its '.' suffixes to '.' prefixes.

  regex <- paste0(
    "^(?<leading_dots>[.]{0,2})",
    "(?<numbers>[0-9]*)",
    "(?<leftovers>[^0-9]?.*$)"
  )

  re <- re_match(new_names, pattern = regex)
  needs_dots <- which(re$numbers != "")
  needs_third_dot <- (re$leftovers[needs_dots] == "")
  re$leading_dots[needs_dots] <- ifelse(needs_third_dot, "...", "..")
  new_names <- paste0(re$leading_dots, re$numbers, re$leftovers)

  new_names
}

# From rematch2, except we don't add tbl_df or tbl classes to the return value
re_match <- function(text, pattern, perl = TRUE, ...) {
  stopifnot(
    is.character(pattern),
    length(pattern) == 1,
    !is.na(pattern)
  )
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  res
}


describe_repair <- function(orig_names, names) {
  if (is_null(orig_names)) {
    orig_names <- rep_along(names, "")
  }
  if (length(orig_names) != length(names)) {
    stop("Internal error: New names and old names don't have same length")
  }

  new_names <- names != as_minimal_names(orig_names)
  if (any(new_names)) {
    msg <- bullets(
      "New names:",
      paste0(
        tick_if_needed(orig_names[new_names]),
        " -> ",
        tick_if_needed(names[new_names]),
        .problem = ""
      )
    )
    message(msg)
  }
}

bullets <- function(header, ..., .problem) {
  problems <- c(...)
  MAX_BULLETS <- 6L
  if (length(problems) >= MAX_BULLETS) {
    n_more <- length(problems) - MAX_BULLETS + 1L
    problems[[MAX_BULLETS]] <- "..."
    length(problems) <- MAX_BULLETS
  }

  paste0(
    header, "\n",
    paste0("* ", problems, collapse = "\n")
  )
}

tick <- function(x) {
  ifelse(is.na(x), "NA", encodeString(x, quote = "`"))
}

is_syntactic <- function(x) {
  ret <- (make_syntactic(x) == x)
  ret[is.na(x)] <- FALSE
  ret
}

tick_if_needed <- function(x) {
  needs_ticks <- !is_syntactic(x)
  x[needs_ticks] <- tick(x[needs_ticks])
  x
}

# Used in names.c
set_rownames_fallback <- function(x, names) {
  rownames(x) <- names
  x
}

# Used in names.c
set_names_fallback <- function(x, names) {
  names(x) <- names
  x
}

vec_set_names <- function(x, names) {
  .Call(vctrs_set_names, x, names)
}

#' Repair names with legacy method
#'
#' This standardises names with the legacy approach that was used in
#' tidyverse packages (such as tibble, tidyr, and readxl) before
#' [vec_as_names()] was implemented. This tool is meant to help
#' transitioning to the new name repairing standard and will be
#' deprecated and removed from the package some time in the future.
#'
#' @inheritParams vec_as_names
#' @param prefix,sep Prefix and separator for repaired names.
#'
#' @examples
#' if (rlang::is_installed("tibble")) {
#'
#' library(tibble)
#'
#' # Names repair is turned off by default in tibble:
#' try(tibble(a = 1, a = 2))
#'
#' # You can turn it on by supplying a repair method:
#' tibble(a = 1, a = 2, .name_repair = "universal")
#'
#' # If you prefer the legacy method, use `vec_as_names_legacy()`:
#' tibble(a = 1, a = 2, .name_repair = vec_as_names_legacy)
#'
#' }
#' @keywords internal
#' @export
vec_as_names_legacy <- function(names, prefix = "V", sep = "") {
  if (length(names) == 0) {
    return(character())
  }

  blank <- names == ""
  names[!blank] <- make.unique(names[!blank], sep = sep)

  new_nms <- setdiff(paste(prefix, seq_along(names), sep = sep), names)
  names[blank] <- new_nms[seq_len(sum(blank))]

  names
}


#' Name specifications
#'
#' @description
#'
#' A name specification describes how to combine an inner and outer
#' names. This sort of name combination arises when concatenating
#' vectors or flattening lists. There are two possible cases:
#'
#' * Named vector:
#'
#'   ```
#'   vec_c(outer = c(inner1 = 1, inner2 = 2))
#'   ```
#'
#' * Unnamed vector:
#'
#'   ```
#'   vec_c(outer = 1:2)
#'   ```
#'
#' In r-lib and tidyverse packages, these cases are errors by default,
#' because there's no behaviour that works well for every case.
#' Instead, you can provide a name specification that describes how to
#' combine the inner and outer names of inputs. Name specifications
#' can refer to:
#'
#' * `outer`: The external name recycled to the size of the input
#'   vector.
#'
#' * `inner`: Either the names of the input vector, or a sequence of
#'   integer from 1 to the size of the vector if it is unnamed.
#'
#' @param name_spec,.name_spec A name specification for combining
#'   inner and outer names. This is relevant for inputs passed with a
#'   name, when these inputs are themselves named, like `outer =
#'   c(inner = 1)`, or when they have length greater than 1: `outer =
#'   1:2`. By default, these cases trigger an error. You can resolve
#'   the error by providing a specification that describes how to
#'   combine the names or the indices of the inner vector with the
#'   name of the input. This specification can be:
#'
#'   * A function of two arguments. The outer name is passed as a
#'     string to the first argument, and the inner names or positions
#'     are passed as second argument.
#'
#'   * An anonymous function as a purrr-style formula.
#'
#'   * A glue specification of the form `"{outer}_{inner}"`.
#'
#'   See the [name specification topic][name_spec].
#'
#' @examples
#' # By default, named inputs must be length 1:
#' vec_c(name = 1)         # ok
#' try(vec_c(name = 1:3))  # bad
#'
#' # They also can't have internal names, even if scalar:
#' try(vec_c(name = c(internal = 1)))  # bad
#'
#' # Pass a name specification to work around this. A specification
#' # can be a glue string referring to `outer` and `inner`:
#' vec_c(name = 1:3, other = 4:5, .name_spec = "{outer}")
#' vec_c(name = 1:3, other = 4:5, .name_spec = "{outer}_{inner}")
#'
#' # They can also be functions:
#' my_spec <- function(outer, inner) paste(outer, inner, sep = "_")
#' vec_c(name = 1:3, other = 4:5, .name_spec = my_spec)
#'
#' # Or purrr-style formulas for anonymous functions:
#' vec_c(name = 1:3, other = 4:5, .name_spec = ~ paste0(.x, .y))
#' @name name_spec
NULL

apply_name_spec <- function(name_spec, outer, inner, n = length(inner)) {
  .Call(vctrs_apply_name_spec, name_spec, outer, inner, n)
}

glue_as_name_spec <- function(`_spec`) {
  function(inner, outer) {
    glue::glue(`_spec`)
  }
}

# Evaluate glue specs in a child of base for now
environment(glue_as_name_spec) <- baseenv()
