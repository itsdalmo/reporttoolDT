#' @export
set_association <- function(srv, ...) {

  dots <- list(...)
  asso <- attr(srv, "associations")

  missing <- setdiff(unlist(dots), names(asso))
  if (length(missing)) {
    stop("The following variables were not found in the data:\n", join_strings(missing))
  }

  for (i in names(dots)) {
    asso[names(asso) %in% dots[[i]]] <- i
  }

  setattr(srv, "associations", asso)

}

#' @export
get_association <- function(srv, associations, order = TRUE) {

  stopifnot(is.survey(srv))
  res <- attr(srv, "associations")

  missing <- setdiff(associations, unique(res))
  if (length(missing)) {
    associations <- setdiff(associations, missing)
    if (!length(associations)) {
      stop("None of the associations were found.")
    } else {
      warning("The following associations were not found:\n", join_strings(missing))
    }
  }

  # Return
  if (order) {
    names(res)[match_all(associations, res)]
  } else {
    names(res)[res %in% associations]
  }

}