# GET: Functions for getting private fields in Survey --------------------------
#' @export
get_label <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_label(which)
}

#' @export
get_association <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_association(which)
}

#' @export
get_marketshare <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_marketshare(which)
}

#' @export
get_config <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_config(which)
}

#' @export
get_translation <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_translation(which)
}

# SET --------------------------------------------------------------------------
#' @export
set_label <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_label: argument 'x' must be of class 'Survey'.", call. = FALSE)
  x$set_label(..., lst = lst)
}

#' @export
set_association <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_association: argument 'x' must be of class 'Survey'.", call. = FALSE)
  x$set_association(..., lst = lst)
}

#' @export
set_marketshare <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_marketshare: argument 'x' must be of class 'Survey'.", call. = FALSE)
  x$set_marketshare(..., lst = lst)
}

#' @export
set_config <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_config: argument 'x' must be of class 'Survey'.", call. = FALSE)
  x$set_config(..., lst = lst)
}

#' @export
set_translation <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_translation: argument 'x' must be of class 'Survey'.", call. = FALSE)
  x$set_translation(..., lst = lst)
}

# Function that merges named vectors for private fields for Survey's.
# Duplicates are dropped from the end of the named vector (after unlist).

merge_attributes <- function(default, lst = NULL) {
  if (is.null(names(default)))
    default <- setNames(rep(NA, length(default)), default)

  if (!is.null(lst) && length(lst) >= 1L) {
    lst <- unlist(lst)
    inv <- is.null(names(lst)) || any(names(lst) == "")
    if (inv) stop("merge_attributes: 'lst' contains unnamed arguments.")
  }

  x <- c(lst, default)
  x <- x[!duplicated(names(x), fromLast = FALSE)]
  x[names(default)]

}
