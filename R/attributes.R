update_survey <- function(x, old_attributes) {

  if (!is.null(old_attributes)) {
    old <- merge_survey_attributes(old_attributes)
  } else {
    old <- NULL
  }

  setattr(x, "labels", update_attribute(names(x), old$labels))
  setattr(x, "associations", update_attribute(names(x), old$associations))
  setattr(x, "translations", update_attribute(default$translation$required, old$translations))
  setattr(x, "config", update_attribute(default$config$setting, old$config))
  setattr(x, "marketshares", update_marketshares(x, old$marketshares))
  setattr(x, "class", c("survey", "data.table", "data.frame"))

}

new_survey <- function(x) update_survey(x, old_attributes = NULL)

merge_survey_attributes <- function(x) {
  if (!is.list2(x)) stop("Argument 'x' should be a list of attributes.")

  # Return early if there is nothing to merge
  x <- x[!vapply(x, is.null, logical(1))]
  if (!length(x) > 1L) return(x[[1]])

  # Use the first element as base and merge in turn
  new <- x[[1]]
  old <- x[-1]

  for (a in old) {
    nms <- intersect(names(new), names(a))
    new[nms] <- suppressWarnings(Map(update_attribute, new[nms], a[nms], merge = TRUE))
    new <- c(new, a[setdiff(names(a), names(new))])
  }

  new

}

# Get/set/update attributes ----------------------------------------------------
get_attributes <- function(obj, which) {
  setNames(lapply(which, attr, x = obj), which)
}

set_attributes <- function(obj, list) {
  stopifnot(!is.null(names(list)))
  for (i in names(list)) setattr(obj, i, list[[i]])
}

strip_attributes <- function(obj, which) {
  for (i in which) setattr(obj, i, NULL)
}

update_attribute <- function(new, old, merge = FALSE) {
  if (is.null(attr(new, "names"))) {
    new <- setNames(rep(NA_character_, length(new)), new)
  }

  if (!is.null(old)) {
    nms <- intersect(names(old), names(new)[is.na(new)])
    if (length(nms)) new[names(new) %in% nms] <- old[nms]
    if (merge) new <- c(new, old[!names(old) %in% names(new)])
  }

  new

}


update_marketshares <- function(x, old = NULL) {
  entities <- get_association(x, "mainentity")
  if (is.null(entities)) return()
  if (length(entities) > 1L) stop("Multiple mainentities specified.", call. = FALSE)

  entities <- unique(x[[entities]])
  update_attribute(entities, old)

}

# Get and set single attributes ------------------------------------------------

get_attr <- function(srv, which, matches = NULL,  arrange = TRUE, match_names = TRUE) {
  # stopifnot(is.survey(srv))
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
  # stopifnot(is.survey(srv))
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