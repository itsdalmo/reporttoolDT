survey <- function(x) {

  x <- data.table::copy(x)
  x <- if (is.labelled(x)) as.data.table(from_labelled(x)) else as.data.table(x)

  # Additional attributes
  setattr(x, "associations", update_associations(x))
  setattr(x, "labels", update_labels(x))
  setattr(x, "marketshares", NA)
  setattr(x, "translations", with(default$translation, setNames(norwegian, required)))
  setattr(x, "config", with(default$config, setNames(value, setting)))

  # Return
  setattr(x, "class", c("survey", "data.table", "data.frame"))
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
  setattr(x, "labels", update_labels(x))
  setattr(x, "associations", update_associations(x))
  x
}

#' @export
"[<-.survey" <- function(x, i, j, ...) {
  x <- NextMethod()
  setattr(x, "labels", update_labels(x))
  setattr(x, "associations", update_associations(x))
  x
}

#' @export
"$<-.survey" <- function(x, i, j, ...) {
  x <- NextMethod()
  setattr(x, "labels", update_labels(x))
  setattr(x, "associations", update_associations(x))
  x
}

#' @export
"names<-.survey" <- function(x, value) {
  x <- NextMethod()
  setattr(x, "labels", setNames(attr(x, "labels"), names(x)))
  setattr(x, "associations", setNames(attr(x, "labels"), names(x)))
  x
}

# Split/join -------------------------------------------------------------------
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
  dots <- list(...); oa <- merge_attributes(lapply(dots, get_attributes))
  res <- data.table::rbindlist(dots, use.names = use.names, fill = fill, idcol = idcol)
  attributes(res) <- append(attributes(res), oa[setdiff(names(oa), names(attributes(res)))])
  structure(res, class = c("survey", "data.table", "data.frame"))
}

#' @export
cbind.survey <- function(...) {
  s <- survey(base::cbind.data.frame(...))
  s
}

#' @export
merge.survey <- function(x, y, ...) {
  dots <- list(x, y); oa <- merge_attributes(lapply(dots, get_attributes))
  res <- NextMethod()
  attributes(res) <- append(attributes(res), oa[setdiff(names(oa), names(attributes(res)))])
  structure(res, class = c("survey", "data.table", "data.frame"))
}

merge_attributes <- function(old) {
  old <- old[!vapply(old, is.null, logical(1))]
  if (length(old) == 1L) return(old[[1]])

  new <- old[[1]]; old <- old[-1]
  for (x in old) {
    new <- suppressWarnings(Map(function(n, o) { c(n, o[setdiff(names(o), names(n))]) }, new, x))
  }
  new

}

get_attributes <- function(x, which = default$attributes) {
  if (is.survey(x)) {
    a <- attributes(x)
    a[names(a) %in% default$attributes]
  } else {
    NULL
  }
}