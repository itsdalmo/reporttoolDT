#' @import data.table
#' @import stringi

survey <- function(srv) {

  srv <- as.data.table(srv)
  setattr(srv, "class", c("survey", "data.table", "data.frame"))
  srv

}

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

#' @export
"[.survey" <- function(...) NextMethod()

#' @export
"[[.survey" <- function(...) NextMethod()