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
  stopifnot(is.survey(x))
  x$get_label(which)
}

#' @rdname get_label
#' @export
get_association <- function(x, which = NULL, invert = TRUE) {
  stopifnot(is.survey(x))
  x$get_association(which, invert = invert)
}

#' @rdname get_label
#' @export
get_marketshare <- function(x, which = NULL) {
  stopifnot(is.survey(x))
  x$get_marketshare(which)
}

#' @rdname get_label
#' @export
get_config <- function(x, which = NULL) {
  stopifnot(is.survey(x))
  x$get_config(which)
}

#' @rdname get_label
#' @export
get_translation <- function(x, which = NULL) {
  stopifnot(is.survey(x))
  x$get_translation(which)
}

#' @rdname get_label
#' @export
get_data <- function(x) {
  stopifnot(is.survey(x))
  x$get_data()
}

#' Set a private field for a Survey object.
#'
#' Like the \code{get_} functions with the same name, these functions provide a
#' familiar interface to set the hidden fields of a \code{Survey} object.
#'
#' @param x A \code{Survey} object.
#' @param ... Named arguments of the format \code{name = value}, with the exception
#' of \code{set_association} which uses \code{value = name}.
#' @param list Optional: A \code{list} (or named character vector) the same format as
#' \code{...}.
#' @param common Optional: Set to \code{TRUE} if you want associations to be set
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

set_label <- function(x, ..., list = NULL) {
  if (!is.survey(x))
    stop("set_label: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_label(..., list = list)
  res
}

#' @rdname set_label
#' @export
set_association <- function(x, ..., list = NULL, common = FALSE) {
  if (!is.survey(x))
    stop("set_association: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_association(..., list = list, common = common)
  res
}

#' @rdname set_label
#' @export
set_marketshare <- function(x, ..., list = NULL) {
  if (!is.survey(x))
    stop("set_marketshare: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_marketshare(..., list = list)
  res
}

#' @rdname set_label
#' @export
set_config <- function(x, ..., list = NULL) {
  if (!is.survey(x))
    stop("set_config: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_config(..., list = list)
  res
}

#' @rdname set_label
#' @export
set_translation <- function(x, ..., list = NULL) {
  if (!is.survey(x))
    stop("set_translation: argument 'x' must be of class 'Survey'.", call. = FALSE)
  res <- x$clone(deep = TRUE)$set_translation(..., list = list)
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

# Function to detect common associations in variable names, based
# on list in default values.
common_latents <- function(var) {
  stopifnot(is.character(var))

  pattern <- get_default("associations")
  out <- vector("character", length(var))
  for (latent in names(pattern)) {
    match <- pattern[[latent]]

    if (length(match) == 1L) {
      match <- suppressWarnings(stri_c("^", match, "[[:alpha:]]*$"))
    } else {
      match <- suppressWarnings(stri_c("^", match, "$", collapse = "|"))
    }

    # Always exclude variable names that end with "em".
    include <- stri_detect(var, regex = match, case_insensitive = TRUE)
    exclude <- stri_detect(var, regex = "em$", case_insensitive = TRUE)

    out[include & !exclude] <- latent

  }

  # Suggest q1 as mainentity if it exists
  is_me <- stri_detect(var, regex = "^q1$", case_insensitive = TRUE)
  if (any(is_me)) {
    out[is_me] <- "mainentity"
  }

  # Set remaining values to NA and return
  out[out == ""] <- NA
  out

}

# We specify c(value = name) to avoid repetition when specifying model-related
# associations. This function reverses the process and returns c(name = value).
names_as_values <- function(x) {
  name <- names(x); value <- unname(x)

  if (is_list(value)) {
    is_null <- vapply(value, is.null, logical(1L))
    is_atomic <- vapply(value, is.atomic, logical(1L))

    # Make sure we have not recieved a list of e.g. data.frames.
    if (any(is_null) || !all(is_atomic))
      stop("Cannot reverse lists that contain non-atomic vectors.")

    # Rep names to match length of value
    name <- lapply(seq_along(value), function(i) rep(name[[i]], length(value[[i]])))

    # Unlist
    name <- unlist(name)
    value <- unlist(value)
  }

  # Return
  setNames(name, value)

}
