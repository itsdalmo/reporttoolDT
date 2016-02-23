#' @export
replace <- function(x, lst, by = x, ignore_case = FALSE) {
  stopifnot(length(x) == length(by))
  if (is.character(lst)) lst <- as.list(lst)

  # Replacements must be named
  if (!is.list2(lst)) {
    stop("Expecting a named list or character vector.", call. = FALSE)
  } else if (is.null(names(lst)) || any(is.na(names(lst))) || any(names(lst) == "")) {
    stop("All lst arguments must be named.", call. = FALSE)
  }

  # Perform replacement
  for (i in names(lst)) {
    new <- i
    old <- lst[[i]]

    if (ignore_case) {
      id <- stri_trans_tolower(by) %in% stri_trans_tolower(old)
    } else {
      id <- by %in% old
    }

    x[id] <- new

  }

  x

}

#' @export
clean_score <- function(var) {
  if (is.factor(var)) var <- as.character(var)
  var <- stri_replace(var, replacement = "$1", regex = "([0-1]+).*$")
  suppressWarnings(as.numeric(var))
}

#' @export
rescale_score <- function(var) {
  if (is.factor(var)) stop("Cannot coerce factor to numeric.", call. = FALSE)
  suppressWarnings(ifelse(var %in% 1:10, (as.numeric(var)-1)*(100/9), NA))
}

#' @export
intranet_link <- function(https) {

  if (Sys.info()["sysname"] != "Windows") {
    stop("This function only works with a network drive on windows.", call. = FALSE)
  } else {
    # If you are on windows and a http(s) link ends with .se
    if (stri_detect(https, regex = "^https?://.*[^/]\\.se/.*")) {
      domain <- stri_replace(https, "$1", regex = "^https?://(.[^/]*)/.*")
      folder <- stri_replace(https, "$1", regex = stri_c(".*", domain, "(.*)"))
      https <- stri_c("\\\\", domain, "@SSL/DavWWWRoot", folder)
    }
  }

  https

}

#  Takes c("X", "Y", "Z") and turns it into "x, y and z"
#' @export
join_str <- function(x, conjunction = "and") {
  stopifnot(is.character(x))
  if (length(x) == 1L) return(x)
  stri_c(stri_c(x[1:(length(x)-1)], collapse = ", "), conjunction, x[length(x)], sep = " ")
}

#  Simple function to align text in plots/tables
#' @export
trim_str <- function(x, n = 50, trail = "...", pad = NULL, side = "right") {
  stopifnot(is.character(x))
  smax <- n - stri_length(trail)
  smin <- if (!is.null(pad)) n else 0

  x <- vapply(x, function(s) {
    if (stri_length(s) > n) {
      stri_c(stri_sub(s, to = smax), trail, sep = "")
    } else if (smin > 0) {
      stri_pad(s, width = smin, pad = pad, side = side)
    } else {
      s
    }
  }, character(1))

  unname(x)
}

# ------------------------------------------------------------------------------

# base::match(x, table): only returns the first match-indicies if there are multiple hits.
# table %in% x:          returns the indicies for all matches, but retains the order of table.
# match_all:             returns all indicies like %in%, but ordered by x.
match_all <- function(x, table) {
  unlist(lapply(x, function(x) which(table == x)))
}

"%ordin%" <- function(x, table) {
  match_all(table, x)
}

clean_path <- function(path) {
  if (!is.string(path)) stop("Path must be a string.", call. = FALSE)
  if (!stri_detect(path, regex = "^(/|[A-Za-z]:|\\\\|~)")) {
    path <- normalizePath(path, "/", mustWork = FALSE)
  }
  stri_replace(path, "", regex = "/$")
}

filename_no_ext <- function(file)  {
  stri_replace(basename(file), "$1", regex = stri_c("(.*)\\.", tools::file_ext(file), "$"))
}

is.string <- function(x) is.character(x) && length(x) == 1
is.labelled <- function(x) any(vapply(x, inherits, what = "labelled", logical(1)))
is.list2 <- function(x) inherits(x, "list")

capture_dots <- function(...) {
  eval(substitute(alist(...)))
}