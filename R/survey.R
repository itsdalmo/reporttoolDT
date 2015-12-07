survey <- function(x) {
  if (is.labelled(x)) x <- from_labelled(x)
  x <- as.data.table(x)

  # Attributes and return
  update_attributes(x)
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
  update_attributes(x)
  x
}

#' @export
"[<-.survey" <- function(x, i, j, ...) {
  x <- NextMethod()
  update_attributes(x)
  x
}

#' @export
"$<-.survey" <- function(x, i, j, ...) {
  x <- NextMethod()
  update_attributes(x)
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
  d <- list(...)
  a <- merge_attributes(lapply(d, function(x) if (is.survey(x)) attributes(x)))
  x <- data.table::rbindlist(d, use.names = use.names, fill = fill, idcol = idcol)
  x <- structure(x, class = c("survey", "data.table", "data.frame"))
  update_attributes(x, old = a)
  x
}

#' @export
cbind.survey <- function(...) {
  a <- merge_attributes(lapply(list(...), function(x) if (is.survey(x)) attributes(x)))
  x <- data.table::data.table(...)
  x <- structure(x, class = c("survey", "data.table", "data.frame"))
  update_attributes(x, old = a)
  x
}

# Merge/join -------------------------------------------------------------------
#' @export
merge.survey <- function(x, y, ...) {
  a <- merge_attributes(lapply(list(x, y), function(x) if (is.survey(x)) attributes(x)))
  x <- NextMethod()
  update_attributes(x, old = a)
  structure(x, class = c("survey", "data.table", "data.frame"))
}