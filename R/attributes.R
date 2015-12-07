
update_attributes <- function(x, old = NULL) {
  setattr(x, "class", c("survey", "data.table", "data.frame"))
  old <- merge_attributes(list(old, attributes(x)))

  setattr(x, "labels", update_named_attr(names(x), old[["labels"]]))
  setattr(x, "associations", update_named_attr(names(x), old[["associations"]]))
  setattr(x, "marketshares", update_marketshares(x))
  setattr(x, "translations", update_named_attr(default$translation$required, old[["translations"]]))
  setattr(x, "config", update_named_attr(default$config$setting, old[["config"]]))

}

merge_attributes <- function(x) {
  stopifnot(is.list2(x))

  # Gather all attributes
  old <- lapply(x, function(a) { if (!is.null(a)) a[names(a) %in% default$attributes] else a })
  old <- old[!vapply(old, is.null, logical(1))]

  # Return early if there is nothing to merge
  if (length(old) == 0L) return()
  if (length(old) == 1L) return(old[[1]])

  # Merge in turn
  new <- old[[1]]
  old <- old[-1]

  for (a in old) {
    nms <- intersect(names(new), names(a))
    new[nms] <- suppressWarnings(Map(function(na, oa) {
      c(na[!is.na(na)], oa[setdiff(names(oa), names(na))])
      }, new[nms], a[nms]))
  }

  new

}

update_marketshares <- function(x) {
  entities <- get_association(x, "mainentity")
  if (is.null(entities)) return()
  if (length(entities) > 1L) stop("Multiple mainentities specified.", call. = FALSE)

  entities <- unique(x[[entities]])
  update_named_attr(entities, attr(x, "marketshares"))

}

update_named_attr <- function(nms, old) {
  new <- setNames(rep(NA_character_, length(nms)), nms)

  if (!is.null(old)) {
    old <- old[!is.na(old)]
    if (length(old)) {
      new[match_all(names(old), names(new))] <- old[intersect(names(old), names(new))]
    }
  }

  new

}

