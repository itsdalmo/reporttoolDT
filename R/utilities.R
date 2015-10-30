#' @export
clean_score <- function(var) {
  if (is.factor(var)) var <- as.character(var)
  var <- stri_replace(var, replacement = "$1", regex = "([0-1]+).*$")
  suppressWarnings(as.numeric(var))
}

#' @export
rescale_score <- function(var) {
  stopifnot(!is.factor(var)); if (is.character(var)) var <- as.numeric(var)
  suppressWarnings(ifelse(var %in% 1:10, (as.numeric(var)-1)*(100/9), NA))
}

#' @export
ordered_replace <- function(x, match_by, replacement = NULL) {

  # Make sure a named vector is used if replacement is not specified
  if (is.null(replacement)) {

    if (is.null(attr(match_by, "names"))) {
      stop("'match_by' must be a named vector or replacement must be specified.", call. = FALSE)
    } else {
      y <- match_by
    }

  } else {

    if (length(match_by) == length(replacement)) {
      y <- setNames(match_by, replacement)
    } else {
      stop("'match' and 'replace' must have same length.", call. = FALSE)
    }
  }

  # Replace x with values from replace (based on 'match')
  if (any(x %chin% y)) {
    x[x %chin% y] <- names(y)[chmatch(x, y, nomatch = 0)]
  }

  x

}

#' @export
intranet_link <- function(https) {


  if (Sys.info()["sysname"] != "Windows") {
    warning("This function only works with a network drive on windows.", call. = FALSE)
  } else {

    # If you are on windows and a http(s) link ends with .se
    if (stri_detect(https, regex = "^https?://.*[^/]\\.se/.*")) {
      domain <- stri_replace(https, "$1", regex = "^https?://(.[^/]*)/.*")
      folder <- stri_replace(https, "$1", regex = paste0(".*", domain, "(.*)"))

      https <- stri_c("\\\\", domain, "@SSL/DavWWWRoot", folder)
    }
  }

  https

}

# MISC -------------------------------------------------------------------------

clean_path <- function(path) {

  if (!is.string(path)) {
    stop("Path is not a string (character(1)):\n", path, call. = FALSE)
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

isFALSE <- function(x) identical(x, FALSE)
is.string <- function(x) is.character(x) && length(x) == 1
is.spss <- function(x) any(vapply(x, inherits, what = "labelled", logical(1)))
is.list2 <- function(x) inherits(x, "list")