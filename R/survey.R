#' @import data.table stringi

survey <- function(x) {

  x <- data.table::copy(x)

  data.table::setDT(x)
  data.table::setattr(x, "class", c("survey", "data.table", "data.frame"))
  data.table::setattr(x, "labels", vector("character", length = ncol(x)))

  # Additional attributes


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

  missing <- setdiff(unlist(dots), names(srv))
  if (length(missing)) {
    missing <- stri_c(missing, collapse = ", ")
    stop("The following variables were not found in the data:\n", missing, call. = FALSE)
  }

  res <- vector("character", ncol(srv))
  res <- NA

  for (i in names(dots)) {
    res[names(srv) %in% dots[[i]]] <- i
  }

  setattr(srv, "association", unname(res))


}

get_association <- function(srv, associations) {

  res <- attr(srv, "association")
  if (is.null(res) || all(is.na(res))) return(NULL)

  missing <- setdiff(associations, unique(res))
  if (length(missing)) {
    associations <- setdiff(associations, missing)
    missing <- stri_c(missing, collapse = ", ")
    warning("The following associations were not found:\n", missing, call. = FALSE)
  }

  res <- names(srv)[res %in% associations]
  if (!length(res)) return(NULL)

  res

}

#' @export
.datatable.aware <- TRUE
