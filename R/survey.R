# Create survey ----------------------------------------------------------------
survey <- function(x) {
  x <- data.table::copy(x)
  if (is.labelled(x)) x <- from_labelled(x)
  o <- get_attributes(x, which = default$attributes)
  update_survey_attributes(x, old_attributes = list(o))
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
as.list.survey <- function(x, attributes = FALSE) {
  if (!attributes) return(NextMethod())

  if (is.null(get_association(x, "mainentity"))) {
    ents <- NULL
  } else {
    ents <- entities(x)
  }

  df <- data.table::copy(x)
  strip_attributes(df, which = default$attributes)
  structure(list("df" = df, "ents" = ents, "mm" = model(x)), class = c("survey_list", "list"))
}

# Basic operations -------------------------------------------------------------

#' @export
"[.survey" <- function(x, ...) {
  o <- get_attributes(x, which = default$attributes)
  # x <- data.table:::"[.data.table"(x, i, j, ...)
  x <- NextMethod()
  if (!is.atomic(x))
    update_survey_attributes(x, old = list(o))
  x
}

#' @export
"[<-.survey" <- function(x, i, j, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

#' @export
"[[<-.survey" <- function(x, i, j, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

#' @export
`$<-.survey` <- function(x, i, j, ...) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

#' @export
`names<-.survey` <- function(x, value) {
  update_survey_names(x, names(x), value)
  NextMethod()
}

#' @export
setnames <- function(x, old, new) UseMethod("setnames")

#' @export
setnames.default <- function(x, old, new) data.table::setnames(x, old, new)

#' @export
setnames.survey <- function(x, old, new) {
  update_survey_names(x, old, new)
  data.table::setnames(x, old, new)
}

# setnames.survey <- function(x, old, new) {
#   o <- get_attributes(x, which = default$attributes)
#
#   # If new is missing, old is a vector with the new columnnames
#   if (missing(new)) {
#     new <- old
#     old <- names(x)
#     if (length(old) != length(new)) {
#       stop("When new is not specified, old must be the same length as the number of columns.", call. = FALSE)
#     }
#   }
#
#   # Replace names for attributes
#   n <- lapply(o, function(x) {
#     if (any(old %in% names(x)))
#       names(x) <- replace(names(x), lst = setNames(old, new))
#     x
#   })
#
#   x <- data.table::setnames(x, old, new)
#   update_survey_attributes(x, old = list(n))
#   x
# }

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
  o <- lapply(list(...), get_attributes, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = o)
  x
}

#' @export
cbind.survey <- function(...) {
  o <- lapply(list(...), get_attributes, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = o)
  x
}

# Merge/join -------------------------------------------------------------------
#' @export
merge.survey <- function(x, y, ...) {
  o <- lapply(list(x, y), get_attributes, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = o)
  x
}

# melt/dcast -------------------------------------------------------------------
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
