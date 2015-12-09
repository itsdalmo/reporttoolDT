set_survey_attributes <- function(x, old = NULL) {
  setattr(x, "class", c("survey", "data.table", "data.frame"))

  if (is.null(old)) {
    old <- get_survey_attributes(x)
  } else {
    stopifnot(is.list2(old))
    old <- merge_survey_attributes(c(old, get_survey_attributes(x)))
  }

  setattr(x, "labels",       update_attr(names(x), old$labels))
  setattr(x, "associations", update_attr(names(x), old$associations))
  setattr(x, "translations", update_attr(default$translation$required, old$translations))
  setattr(x, "config",       update_attr(default$config$setting, old$config))
  setattr(x, "marketshares", update_marketshares(x, old$marketshares))

}

strip_survey_attributes <- function(x, old = NULL) {
  lapply(default$attributes, setattr, x = x, value = NULL)
}

get_survey_attributes <- function(x) {
  if (!is.survey(x)) return()
  a <- attributes(x)
  a <- a[names(a) %in% default$attributes]
  a[!vapply(a, is.null, logical(1))]
}

merge_survey_attributes <- function(x) {
  stopifnot(is.list2(x))
  x <- x[!vapply(x, is.null, logical(1))]

  # Return early if there is nothing to merge
  if (length(x) == 0L) return()
  if (length(x) == 1L) return(x[[1]])

  # Merge in turn
  new <- x[[1]]
  old <- x[-1]

  for (a in old) {
    nms <- intersect(names(new), names(a))
    new[nms] <- suppressWarnings(Map(merge_attr, new[nms], a[nms]))
    new <- c(new, a[setdiff(names(a), names(new))])
  }

  new

}

update_attr <- function(x, y) {
  if (is.null(attr(x, "names"))) x <- setNames(rep(NA_character_, length(x)), x)

  if (!is.null(y)) {
    m <- intersect(names(y), names(x)[is.na(x)])
    if (length(m)) x[match_all(m, names(x))] <- y[m] #x[m] <- y[m]
  }

  x

}

merge_attr <- function(x, y) {
  x <- update_attr(x, y)
  c(x, y[setdiff(names(y), names(x))])
}

update_marketshares <- function(x, old = NULL) {
  entities <- get_association(x, "mainentity")
  if (is.null(entities)) return()
  if (length(entities) > 1L) stop("Multiple mainentities specified.", call. = FALSE)

  entities <- unique(x[[entities]])
  update_attr(entities, old)

}

get_attr <- function(srv, which, matches = NULL,  arrange = TRUE, match_names = TRUE) {
  stopifnot(is.survey(srv))
  res <- attr(srv, which)

  # Return early if matches is NULL
  if (is.null(res)) {
    stop("Attribute '", which, "' is not set.")
  } else if (is.null(matches)) {
    return(res)
  }

  # Return NULL if no matches and give warning if not everything 'matches'
  missing <- setdiff(matches, unique(res))
  if (length(missing)) {
    matches <- setdiff(matches, missing)
    if (!length(matches)) return()
    warning("The following ", which, " were not found:\n", join_strings(missing))
  }

  # Match values or names
  m <- if (match_names) names(res) else unname(res)

  # Return
  if (arrange) {
    res[match_all(matches, m)]
  } else {
    res[m %in% matches]
  }
}

set_attr <- function(srv, which, dots, match_names = TRUE) {
  stopifnot(is.survey(srv))
  res <- attr(srv, which)

  # Set matches and replacements
  if (match_names) {
    m <- as.list(names(dots))
    v <- unname(dots)
  } else {
    m <- unname(dots)
    v <- as.list(names(dots))
  }

  missing <- setdiff(unlist(m), names(res))
  if (length(missing)) {
    stop("The following arguments were not found in the data:\n", join_strings(missing))
  }

  for (i in seq_along(m)) {
    res[names(res) %in% m[[i]]] <- v[[i]]
  }

  setattr(srv, which, res)

}