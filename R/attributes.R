get_attribute <- function(x, ...) print("getting attribute")

merge_attributes <- function(default, ...) {
  if (is.null(names(default)))
    default <- setNames(rep(NA, length(default)), default)

  inv <- vapply(list(...), function(x) {all(!is.null(x) && length(x) > 1L && is.null(names(x)))}, logical(1))
  if (any(inv)) stop("merge_attributes: vectors must be either NULL or named vectors")

  x <- c(..., default)
  x <- x[!duplicated(names(x), fromLast = FALSE)]
  x[names(default)]

}
