#' Get a private field from a Survey object.
#'
#' The listed functions provide a familiar interface to the hidden fields of
#' a \code{Survey} object. Each returns a named vector with the corresponding values.
#'
#' @param x A \code{Survey} object.
#' @param which If this is specified, the function only returns the elements
#' which match the criteria. This should be a character vector.
#' @param invert For \code{get_association} the default output is returned with
#' the association as a name and variable names as the values. Set to \code{FALSE}
#' to override.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # Create a new survey (regular)
#' x <- survey_df(data.frame("A" = 1, "B" = 2))
#' x <- set_label(x, A = "This is a label")
#' get_label(x, "A")
#'
#' # Same example using R6
#' y <- survey_df(data.frame("A" = 1, "B" = 2))
#' y$set_label(A = "This is a label")
#' y$get_label("A")
#'
#' # Should be equivalent
#' all.equal(x, y)

get_label <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_label(which)
}

#' @rdname get_label
#' @export
get_association <- function(x, which = NULL, invert = TRUE) {
  if (!is.survey(x))
    return()
  x$get_association(which, invert = invert)
}

#' @rdname get_label
#' @export
get_marketshare <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_marketshare(which)
}

#' @rdname get_label
#' @export
get_config <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_config(which)
}

#' @rdname get_label
#' @export
get_translation <- function(x, which = NULL) {
  if (!is.survey(x))
    return()
  x$get_translation(which)
}

#' Set a private field for a Survey object.
#'
#' Like the \code{get_} functions with the same name, these functions provide a
#' familiar interface to set the hidden fields of a \code{Survey} object.
#'
#' @param x A \code{Survey} object.
#' @param ... Named arguments of the format \code{name = value}, with the exception
#' of \code{set_association} which uses \code{value = name}.
#' @param lst Optional: A \code{list} (or named character vector) the same format as
#' \code{...}.
#' @note These functions return a copy (\code{deep_clone}) of the \code{Survey}.
#' Use the \code{R6} method directly to avoid copying.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # Create a new survey (regular)
#' x <- survey_df(data.frame("A" = 1, "B" = 2))
#' x <- set_label(x, A = "This is a label")
#' get_label(x, "A")
#'
#' # Same example using R6
#' y <- survey_df(data.frame("A" = 1, "B" = 2))
#' y$set_label(A = "This is a label")
#' y$get_label("A")
#'
#' # Should be equivalent
#' all.equal(x, y)

set_label <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_label: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_label(..., lst = lst)
  res
}

#' @rdname set_label
#' @export
set_association <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_association: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_association(..., lst = lst)
  res
}

#' @rdname set_label
#' @export
set_marketshare <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_marketshare: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_marketshare(..., lst = lst)
  res
}

#' @rdname set_label
#' @export
set_config <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_config: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_config(..., lst = lst)
  res
}

#' @rdname set_label
#' @export
set_translation <- function(x, ..., lst = NULL) {
  if (!is.survey(x))
    stop("set_translation: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_translation(..., lst = lst)
  res
}

# Utility function that merges named vectors for private fields in Survey's.
# Duplicates are dropped from the end of the named vector (after unlisting).

merge_vectors <- function(..., default = NULL) {
  dots <- list(...)
  if (!length(dots)) stop("No vectors supplied.")

  # Use unnamed default's as names for a vector of NA's.
  # (So we can pass colummnames as a default.)
  if (!is.null(default) && is.null(names(default))) {
    default <- setNames(rep(NA, length(default)), default)
  }

  res <- c(unlist(dots), default)
  if (!is_named(res)) stop("All elements must be named.")

  # Return should be ordered by default if it exists
  res <- res[!duplicated(names(res), fromLast = FALSE)]
  if (!is.null(default)) {
    res[names(default)]
  } else {
    res
  }

}
