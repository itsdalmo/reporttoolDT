#' @export
survey <- function(x) UseMethod("survey")

#' @export
is.survey <- function(x) inherits(x, "survey")

#' @export
as.survey <- function(x) UseMethod("as.survey")

#' @export
as.survey.survey <- function(x) x

#' @export
as.survey.default <- function(x) survey(x)

new_survey <- function(x) {
  list(
    .data = x,
    .associations = NULL,
    .labels = attr(x, "labels"),
    .config = NULL,
    .dictionary = NULL
  )
}

# Basic operations -------------------------------------------------------------

#' @export
`[.survey` <- function(x, ...) {
  `[`(x$.data, ...)
}

#' @export
`[<-.survey` <- function(x, i, j, value) {
  `[<-`(x$.data, i, j, value)
}

#' @export
`$.survey` <- function(x, name) {
  `[[`(x, name)
}

#' @export
`$<-.survey` <- function(x, name, value) {
  `$<-`(x$.data, name, value)
}

#' @export
`[[.survey` <- function(x, ...) {
  args <- list(...)
  if (substr(args[[1]], 0, 1) == ".") {
    NextMethod()
  } else {
    `[[`(x$.data, ...)
  }
}

#' @export
`[[<-.survey` <- function(x, i, j, value) {
  `[[<-`(x$.data, i, j, value)
}

#' @export
names.survey <- function(x) names(x$.data)

#' @export
`names<-.survey` <- function(x, value) {
  `names<-`(x$.data, value)
}

#' @export
length.survey <- function(x) length(x$.data)

#' @export
dim.survey <- function(x) dim(x$.data)

#' @export
dimnames.survey <- function(x) {
  dimnames(x$.data)
}

#' @export
`dimnames<-.survey` <- function(x, value) {
  `dimnames<-`(x$.data, value)
}

# data.table -------------------------------------------------------------------

#' @export
setnames <- function(x, old, new) UseMethod("setnames")

#' @export
setnames.default <- function(x, old, new) data.table::setnames(x, old, new)

#' @export
setnames.survey <- function(x, old, new) {
  data.table::setnames(x.data, old, new)
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
rbind.survey <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  o <- lapply(list(...), get_attributes, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = o)
  x
}

#' @export
cbind.default <- function(...) {
  base::cbind(...)
}

#' @export
cbind.survey <- function(...) {
  o <- lapply(list(...), get_attributes, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = o)
  x
}

# Merge/join/cast --------------------------------------------------------------
#' @export
merge.survey <- function(x, y, ...) {
  o <- lapply(list(x, y), get_attributes, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = o)
  x
}

melt.survey <- function(x, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

dcast.survey <- function(x, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

# Print methods ----------------------------------------------------------------
print.survey <- function(x, ...) {
  print(x$.data, ...)
}

print.survey_list <- function(x, width = getOption("width")) {

  cat("Survey\n")

  # Class and dimensions of the objects
  info <- lapply(x, function(x) {
    classes <- stri_c("(", class(x)[1], ")", sep = "")
    dimensions <- stri_c("[", stri_c(dim(x), collapse = "x"), "]", sep = "")
    stri_c(classes, dimensions, sep = "")
  })

  # Fix width
  nms <- stri_c("$", names(x))
  nms <- stri_pad_right(nms, width = max(stri_length(nms), n.rm = TRUE) + 4)
  cat(stri_c(nms, info, collapse = "\n"))

}
