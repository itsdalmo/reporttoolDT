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

# Check whether data is a tbl --------------------------------------------------
is_tbl <- function(x) {
  inherits(x, c("tbl", "tbl_df", "tbl_dt"))
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

