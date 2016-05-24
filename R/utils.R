# Capture dots. Primarily used to pass calls to R6 methods ---------------------
capture_dots <- function(...) {
  eval(substitute(alist(...)))
}

# Get renamed columns from lazy_dots in dplyr::select and dplyr::rename.
# Return is equivalent to: setNames(old_name, new_name) ------------------------
renamed_vars <- function(vars, dots) {
  renamed <- dplyr::select_vars_(vars, dots)
  renamed <- renamed[names(renamed) != renamed]

  # Return NULL early if nothing has been renamed.
  if (!length(renamed)) return()

  # If renamed. Return a renamed vars vector.
  # (has to be done in a loop to support referencing newly created vars.)
  for (i in seq_along(renamed)) {
    vars[vars == renamed[i]] <- names(renamed)[i]
  }
  vars

}

# Check whether data is a tbl --------------------------------------------------
is_tbl <- function(x) {
  inherits(x, c("tbl", "tbl_df", "tbl_dt"))
}


# Check which OS we are on -----------------------------------------------------
on_windows <- function() {
  Sys.info()["sysname"] == "Windows"
}

on_osx <- function() {
  Sys.info()["sysname"] == "Darwin"
}

# match_all returns ALL matching indices for x in table, ordered by x ----------
match_all <- function(x, table) {
  unlist(lapply(x, function(x) which(table == x)))
}

# normalizes paths and removes trailing /'s ------------------------------------
clean_path <- function(path) {
  if (!is_string(path)) stop("Path must be a string.")

  # Normalize if path is not absolute.
  if (!stri_detect(path, regex = "^(/|[A-Za-z]:|\\\\|~)")) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  # Remove trailing slashes
  if (stri_detect(path, regex = "/$")) {
    path <- stri_replace(path, "", regex = "/$")
  }

  path

}

# Retrieve the filename sans extension -----------------------------------------
basename_sans_ext <- function(file)  {
  tools::file_path_sans_ext(basename(file))
}

# Check if x is a string (length 1 character vector.) --------------------------
is_string <- function(x) {
  is.character(x) && length(x) == 1
}

# See if a vector is named -----------------------------------------------------
is_named <- function(x) {
  nms <- names(x)
  !is.null(nms) && !any(is.na(nms)) && !any(nms == "")
}

# See if a numeric vactor contains fractions/decimals (i.e., not an integer) ---
any_fractions <- function(x) {
  num <- x%%1 != 0
  any(num[!is.na(num)])
}

# Alternative with precision and comparable benchmark:
# any_fractions <- function(x) {
#   num <- abs(x - round(x)) > .Machine$double.eps^0.5
#   any(num[!is.na(num)])
# }

# See if a list or data.frame contains any labelled vectors. -------------------
any_labelled <- function(x) {
  any(vapply(x, inherits, what = "labelled", logical(1)))
}

# Like is.list, except it does not return true for data.frame ------------------
is_list <- function(x) {
  inherits(x, "list")
}

# Hadley's %||% ----------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

