survey <- function(x) {

  x <- data.table::copy(x)
  tmp <- rep(NA_character_, ncol(x))

  data.table::setDT(x)
  data.table::setattr(x, "class", c("survey", "data.table", "data.frame"))

  # Additional attributes
  setattr(x, "associations", setNames(tmp, names(x)))
  setattr(x, "labels", setNames(tmp, names(x)))
  setattr(x, "marketshares", NA)
  setattr(x, "config", NA)
  setattr(x, "translations", NA)

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

  # Update labels
  lbls <- attr(x, "labels"); nms <- names(x)
  setattr(x, "labels", ifelse(lbls == "", nms, lbls))
  x

}

#' @export
"[[.survey" <- function(...) NextMethod()

#' @export
"$<-.survey" <- function(...) NextMethod()

#' @export
"names<-.survey" <- function(x, value) {
  data.table::setnames(x, value)
}

# Split/join -------------------------------------------------------------------

rbind.survey <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {

  x <- list(...)
  print("survey")
#   # Keep attributes
#   is_survey <- vapply(x, is.survey, logical(1))
#   if (any(is_survey)) {
#     att <- lapply(x[is_survey], attributes)
#   }

  data.table::rbindlist(x, use.names, fill, idcol)

}