update_labels <- function(x) {
  new <- setNames(rep(NA_character_, ncol(x)), names(x))
  old <- attr(x, "labels")
  if (!is.null(old)) {
    old <- old[!is.na(old)]
    new[match_all(names(old), names(new))] <- old[intersect(names(old), names(new))]
  }
  new
}

update_associations <- function(x) {
  new <- setNames(rep(NA_character_, ncol(x)), names(x))
  old <- attr(x, "associations")
  if (!is.null(old)) {
    old <- old[!is.na(old)]
    new[match_all(names(old), names(new))] <- old[intersect(names(old), names(new))]
  }
  new
}