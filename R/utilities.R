#' @export
replace <- function(x, replacement, ignore_case = TRUE) {

  if (is.character(replacement)) {
    replacement <- as.list(replacement)
  }

  if (!is.list2(replacement)) {
    stop("Expecting a named list or character vector.", call. = FALSE)
  } else if (is.null(names(replacement)) || any(names(replacement) == "")) {
    stop("All replacement arguments must be named.", call. = FALSE)
  }

  for (i in names(replacement)) {
    old <- replacement[[i]]
    new <- i

    if (ignore_case) {
      old_id <- stri_trans_tolower(x) %in% stri_trans_tolower(old)
    } else {
      old_id <- x %in% old
    }

    x[old_id] <- new

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

# MISC -------------------------------------------------------------------------
isFALSE <- function(x) identical(x, FALSE)
is.string <- function(x) is.character(x) && length(x) == 1
is.labelled <- function(x) any(vapply(x, inherits, what = "labelled", logical(1)))
is.list2 <- function(x) inherits(x, "list")

clean_path <- function(path) {

  if (!is.string(path)) {
    stop("Path must be a string.", call. = FALSE)
  }

  # Normalize
  if (!stri_detect(path, regex = "^(/|[A-Za-z]:|\\\\|~)")) {
    path <- normalizePath(path, "/", mustWork = FALSE)
  }

  # Remove trailing slashes and return
  stri_replace(path, "", regex = "/$")

}

filename_no_ext <- function(file)  {
  stri_replace(basename(file), "$1", regex = stri_c("(.*)\\.", tools::file_ext(file), "$"))
}
