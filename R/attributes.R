update_labels <- function(x) {
  new <- setNames(rep(NA_character_, ncol(x)), names(x))
  old <- attr(x, "labels")
  if (!is.null(old)) new <- replace(new, old[!is.na(old)])
  new
}

update_associations <- function(x) {
  new <- setNames(rep(NA_character_, ncol(x)), names(x))
  old <- attr(x, "associations")
  if (!is.null(old)) new <- replace(new, old[!is.na(old)])
  new
}