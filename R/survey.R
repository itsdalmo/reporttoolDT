# Create survey ----------------------------------------------------------------

survey <- function(x) {
  x <- data.table::copy(x)
  if (is.labelled(x)) x <- from_labelled(x)
  o <- get_attributes(x, which = default$attributes)
  update_survey(as.data.table(x), old_attributes = list(o))
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

#' @export
as_list <- function(x) {
  df <- as.data.frame(data.table::copy(x))
  list("df" = df, "ents" = entities(y), "mm" = model(x))
}
# #' @export
# as.list.survey <- function(x) {
#   x <- as.data.frame(data.table::copy(x))
#   strip_attributes(x, which = default$attributes)
#   list("df" = x)
# }

# Basic operations -------------------------------------------------------------

#' @export
"[.survey" <- function(x, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- data.table:::"[.data.table"(x, ...)
  update_survey(x, old = list(o))
  x
}

#' @export
"[<-.survey" <- function(x, i, j, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey(x, old = list(o))
  x
}

#' @export
"[[<-.survey" <- function(x, i, j, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey(x, old = list(o))
  x
}

#' @export
"$<-.survey" <- function(x, i, j, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey(x, old = list(o))
  x
}

#' @export
"names<-.survey" <- function(x, value) {
  x <- NextMethod()
  setattr(x, "labels", setNames(attr(x, "labels"), names(x)))
  setattr(x, "associations", setNames(attr(x, "associations"), names(x)))
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
  dots <- list(...); o <- lapply(dots, get_attributes, which = default$attributes)
  x <- data.table::rbindlist(dots, use.names = use.names, fill = fill, idcol = idcol)
  update_survey(x, old = o)
  x
}

#' @export
cbind.survey <- function(...) {
  dots <- list(...); o <- lapply(dots, get_attributes, which = default$attributes)
  x <- base::cbind.data.frame(dots)
  update_survey(x, old = o)
  x
}

# Merge/join -------------------------------------------------------------------
#' @export
merge.survey <- function(x, y, ...) {
  dots <- list(x, y); o <- lapply(dots, get_attributes, which = default$attributes)
  x <- NextMethod()
  update_survey(x, old = o)
  x
}
