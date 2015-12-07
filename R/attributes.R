set_survey_attr <- function(x, old = NULL) {
  setattr(x, "class", c("survey", "data.table", "data.frame"))
  old <- merge_attributes(list(old, get_survey_attr(x)))

  setattr(x, "labels", update_named_attr(names(x), old[["labels"]]))
  setattr(x, "associations", update_named_attr(names(x), old[["associations"]]))
  setattr(x, "marketshares", update_marketshares(x, old[["marketshares"]]))
  setattr(x, "translations", update_named_attr(default$translation$required, old[["translations"]]))
  setattr(x, "config", update_named_attr(default$config$setting, old[["config"]]))

}

get_survey_attr <- function(x) {
  if (!is.survey(x)) return()
  a <- attributes(x)
  a <- a[names(a) %in% default$attributes]
  a[!vapply(a, is.null, logical(1))]
}

merge_attributes <- function(x) {
  stopifnot(is.list2(x))

  # Return early if there is nothing to merge
  if (length(x) == 0L) return()
  if (length(x) == 1L) return(x[[1]])

  # Merge in turn
  new <- x[[1]]
  old <- x[-1]

  for (a in old) {
    nms <- intersect(names(new), names(a))
    new[nms] <- suppressWarnings(Map(update_named_attr, new[nms], a[nms]))
    new <- c(new, a[setdiff(names(a), names(new))])
  }

  new

}

update_named_attr <- function(x, y) {
  if (is.null(attr(x, "names"))) x <- setNames(rep(NA_character_, length(x)), x)

  if (!is.null(y)) {
    m <- intersect(names(y), names(x)[is.na(x)])
    if (length(m)) x[match_all(m, names(x))] <- y[m] #x[m] <- y[m]
    x <- c(x, y[setdiff(names(y), names(x))])
  }

  x

}

update_marketshares <- function(x, old = NULL) {
  entities <- get_association(x, "mainentity")
  if (is.null(entities)) return()
  if (length(entities) > 1L) stop("Multiple mainentities specified.", call. = FALSE)

  entities <- unique(x[[entities]])
  update_named_attr(entities, old)

}
