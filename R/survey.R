survey <- function(x) {
  if (is.labelled(x)) x <- from_labelled(x)
  x <- as.data.table(x)

  # Attributes and return
  set_survey_attr(x)
  x

}


# is/as ------------------------------------------------------------------------

#' @export
is.survey <- function(x) inherits(x, "survey")

#' @export
as.survey <- function(x) UseMethod("as.survey")

#' @export
as.survey.survey <- function(x) x

#' @export
as.survey.default <- function(x) survey(x)

# Basic operations -------------------------------------------------------------

#' @export
"[.survey" <- function(x, ...) {
  x <- data.table:::"[.data.table"(x, ...)
  set_survey_attr(x)
  x
}

#' @export
"[<-.survey" <- function(x, i, j, ...) {
  x <- NextMethod()
  set_survey_attr(x)
  x
}

#' @export
"$<-.survey" <- function(x, i, j, ...) {
  x <- NextMethod()
  set_survey_attr(x)
  x
}

#' @export
"names<-.survey" <- function(x, value) {
  x <- NextMethod()
  setattr(x, "labels", setNames(attr(x, "labels"), names(x)))
  setattr(x, "associations", setNames(attr(x, "labels"), names(x)))
  x
}

# Bind rows/cols ---------------------------------------------------------------
#' @export
rbind <- function(...) UseMethod("rbind")

#' @export
cbind <- function(...) UseMethod("cbind")

#' @export
rbind.default <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  base::rbind(..., use.names = use.names, fill = fill, idcol = idcol)
}

#' @export
cbind.default <- function(...) {
  base::cbind(...)
}

#' @export
rbind.survey <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  dots <- list(...); oa <- lapply(dots, get_survey_attr)
  x <- data.table::rbindlist(dots, use.names = use.names, fill = fill, idcol = idcol)
  set_survey_attr(x, old = oa)
  x
}

#' @export
cbind.survey <- function(...) {
  dots <- list(...); oa <- lapply(dots, get_survey_attr)
  x <- data.table::data.table(dots)
  set_survey_attr(x, old = oa)
  x
}

# Merge/join -------------------------------------------------------------------
#' @export
merge.survey <- function(x, y, ...) {
  dots <- list(x, y); oa <- lapply(dots, get_survey_attr)
  x <- NextMethod()
  set_survey_attr(x, old = oa)
  x
}