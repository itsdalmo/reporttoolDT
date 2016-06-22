#' Get a private field from a Survey object.
#'
#' The listed functions provide a familiar interface to the hidden fields of
#' a \code{Survey} object. Each returns a named vector with the corresponding values.
#'
#' @param .data A \code{Survey} object.
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

get_label <- function(.data, which = NULL) {
  stopifnot(is.survey(.data))
  .data$get_label(which)
}

#' @rdname get_label
#' @export
get_association <- function(.data, which = NULL, invert = TRUE) {
  stopifnot(is.survey(.data))
  .data$get_association(which, invert = invert)
}

#' @rdname get_label
#' @export
get_marketshare <- function(.data, which = NULL) {
  stopifnot(is.survey(.data))
  .data$get_marketshare(which)
}

#' @rdname get_label
#' @export
get_config <- function(.data, which = NULL) {
  stopifnot(is.survey(.data))
  .data$get_config(which)
}

#' @rdname get_label
#' @export
get_translation <- function(.data, which = NULL) {
  stopifnot(is.survey(.data))
  .data$get_translation(which)
}

#' @rdname get_label
#' @export
get_data <- function(.data) {
  stopifnot(is.survey(.data))
  .data$get_data()
}

#' Set a private field for a Survey object.
#'
#' Like the \code{get_} functions with the same name, these functions provide a
#' familiar interface to set the hidden fields of a \code{Survey} object.
#'
#' @param .data A \code{Survey} object.
#' @param ... Named arguments of the format \code{name = value}, with the exception
#' of \code{set_association} which uses \code{value = name}.
#' @param .list Optional: A \code{list} (or named character vector) the same format as
#' \code{...}.
#' @param .language Optional: Language defaults to use for translations.
#' @param .auto Optional: Set to \code{TRUE} if you want to automatically set labels
#' for EM variables and latents (if translations are set).
#' @param .common Optional: Set to \code{TRUE} if you want associations to be set
#' for common variable names. E.g., q1 is set as mainentity, and q4a-z is associated with image.
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

set_label <- function(.data, ..., .list = NULL, .auto = FALSE) {
  if (!is.survey(.data))
    stop("set_label: argument '.data' must be of class 'Survey'.", call. = FALSE)
  res <- .data$clone(deep = TRUE)$set_label(..., .list = .list, .auto = .auto)
  res
}

#' @rdname set_label
#' @export
set_association <- function(.data, ..., .list = NULL, .common = FALSE) {
  if (!is.survey(.data))
    stop("set_association: argument '.data' must be of class 'Survey'.", call. = FALSE)
  res <- .data$clone(deep = TRUE)$set_association(..., .list = .list, .common = .common)
  res
}

#' @rdname set_label
#' @export
set_marketshare <- function(.data, ..., .list = NULL) {
  if (!is.survey(.data))
    stop("set_marketshare: argument '.data' must be of class 'Survey'.", call. = FALSE)
  res <- .data$clone(deep = TRUE)$set_marketshare(..., .list = .list)
  res
}

#' @rdname set_label
#' @export
set_config <- function(.data, ..., .list = NULL) {
  if (!is.survey(.data))
    stop("set_config: argument '.data' must be of class 'Survey'.", call. = FALSE)
  res <- .data$clone(deep = TRUE)$set_config(..., .list = .list)
  res
}

#' @rdname set_label
#' @export
set_translation <- function(.data, ..., .list = NULL, .language = NULL) {
  if (!is.survey(.data))
    stop("set_translation: argument '.data' must be of class 'Survey'.", call. = FALSE)
  res <- .data$clone(deep = TRUE)$set_translation(..., .list = .list, .language = .language)
  res
}
