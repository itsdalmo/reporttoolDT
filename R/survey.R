#' @import data.table stringi

survey <- function(x) {

  x <- data.table::copy(x)

  data.table::setDT(x)
  data.table::setattr(x, "class", c("survey", "data.table", "data.frame"))

  # Additional attributes
  fill_char <- setNames(rep(NA_character_, length = ncol(x)), names(x))

  data.table::setattr(x, "labels", fill_char)
  data.table::setattr(x, "associations", fill_char)
  data.table::setattr(x, "marketshares", NULL)

  x

}

#' @export
"[.survey" <- function(x, ...) {

  x <- data.table:::"[.data.table"(x, ...)

  # Update labels
  lbls <- attr(x, "labels"); nms <- names(x)
  setattr(x, "labels", ifelse(lbls == "", nms, lbls))
  x

}

#' @export
"[[.survey" <- function(...) NextMethod()

set_association <- function(srv, ...) {

  dots <- list(...)
  asso <- attr(srv, "associations")

  missing <- setdiff(unlist(dots), names(asso))
  if (length(missing)) {
    missing <- stri_c(missing, collapse = ", ")
    stop("The following variables were not found in the data:\n", missing, call. = FALSE)
  }

  for (i in names(dots)) {
    asso[names(asso) %in% dots[[i]]] <- i
  }

  setattr(srv, "associations", asso)

}

get_association <- function(srv, associations) {

  res <- attr(srv, "associations")
  if (is.null(res) || all(is.na(res))) return(NULL)

  missing <- setdiff(associations, unique(res))
  if (length(missing)) {
    associations <- setdiff(associations, missing)
    if (!length(associations)) {
      stop("None of the associations were found.", call. = FALSE)
    } else {
      missing <- stri_c(missing, collapse = ", ")
      warning("The following associations were not found:\n", missing, call. = FALSE)
    }
  }

  names(res)[res %in% associations]

}

#' @export
.datatable.aware <- TRUE
