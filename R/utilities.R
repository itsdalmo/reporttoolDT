#' @export
replace <- function(x, list, by = x, ignore_case = FALSE, invert = FALSE) {
  stopifnot(length(x) == length(by))
  if (is.character(list)) list <- as.list(list)

  # Replacements must be named
  if (!is.list2(list)) {
    stop("Expecting a named list or character vector.", call. = FALSE)
  } else if (is.null(names(list)) || any(is.na(names(list))) || any(names(list) == "")) {
    stop("All list arguments must be named.", call. = FALSE)
  }

  # Perform replacement
  for (i in seq_along(matches)) {
    # Invert if the list is of the form: list(value = matches)
    if (invert) {
      matches <- list[[i]]
      value <- names(list)[i]
    } else {
      matches <- names(list)[i]
      value <- list[[i]]
    }

    if (ignore_case) {
      id <- stri_trans_tolower(by) %in% stri_trans_tolower(matches[i])
    } else {
      id <- by %in% matches[i]
    }
    x[id] <- value[i]
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

#' @export
join_strings <- function(x, conjunction = "and") {
  stopifnot(is.character(x))
  if (length(x) == 1L) return(x)
  stri_c(stri_c(x[1:(length(x)-1)], collapse = ", "), conjunction, x[length(x)], sep = " ")
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
