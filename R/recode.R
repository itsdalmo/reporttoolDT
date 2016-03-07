# TODO: Separate out to a new function and export. recode or replace?
replace_all <- function(x, lst, by = x, ignore_case = FALSE) {
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