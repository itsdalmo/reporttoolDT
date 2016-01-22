#' @export
tbl_vars.survey <- function(srv) names(srv$df)

#' @export
groups.survey <- function(srv) groups(srv$df)

#' @export
ungroup.survey <- function(srv) { srv$df <- ungroup(srv$df); srv }

#' @export
group_by_.survey <- function(srv, ..., .dots, add = FALSE) {
  srv$df <- dplyr::group_by_(srv$df, ..., .dots = .dots, add = add)
  srv
}

#' @export
summarise_.survey <- function(srv, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  grps <- unlist(groups(srv$df))
  srv$df <- dplyr::summarise_(srv$df, .dots = dots)

  # Subset measurement model
  srv$mm <- filter(srv$mm, manifest %in% c(names(dots), grps))
  srv$mm <- as.survey_mm(srv$mm)

  srv
}

#' @export
arrange_.survey <- function(srv, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  srv$df <- dplyr::arrange_(srv$df, .dots = dots)

  srv
}

#' @export
mutate_.survey <- function(srv, ..., .dots) {

  # Gather dots and mutate the data
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  srv$df <- dplyr::mutate_(srv$df, .dots = dots)

  # Also update contrast if it exists
  if (nrow(srv$cd)) {
    vars <- intersect(names(dots), names(srv$cd))
    if (length(vars)) {
      srv$cd <- dplyr::mutate_(srv$cd, .dots = dots[names(dots) %in% vars])
    }
  }

  # Update mm (overhead)
  cols <- unique(names(dots))
  srv <- update_mm(srv, cols)

  # Return
  srv

}

#' @export
select_.survey <- function(srv, ..., .dots) {

  # Gather dots and apply select to data
  dots <- lazyeval::all_dots(.dots, ...)
  srv$df <- dplyr::select_(srv$df, .dots = dots)

  # Get list of renamed variables
  expr <- dots[!is.na(names(dots)) & names(dots) != ""]
  if (length(expr)) {
    nms <- lapply(names(expr), function(nm) { x <- expr[[nm]]$expr; if (x != nm) x })
    nms <- setNames(unlist(nms), names(expr))
  } else {
    nms <- NULL
  }

  # Update renames in manifest
  if (!is.null(nms) && length(nms)) {
    srv$mm$manifest <- ordered_replace(srv$mm$manifest, nms, names(nms))
  }

  # Subset measurement model while retaining order
  row_order <- match(names(srv$df), srv$mm$manifest)
  srv$mm <- slice(srv$mm, row_order)
  srv$mm <- as.survey_mm(srv$mm)

  # Return
  srv

}

#' @export
rename_.survey <- function(srv, ..., .dots) {

  # Gather dots and apply select to data
  dots <- lazyeval::all_dots(.dots, ...)
  srv$df <- dplyr::rename_(srv$df, .dots = dots)

  # Get list of renamed variables
  if (length(dots)) {
    nms <- lapply(dots, function(x) x$expr)
    nms <- setNames(unlist(nms), names(dots))
  } else {
    nms <- NULL
  }

  # Update renames in manifest
  if (!is.null(nms) && length(nms)) {
    srv$mm$manifest <- ordered_replace(srv$mm$manifest, nms, names(nms))
  }

  # Return
  srv

}

#' @export
filter_.survey <- function(srv, ..., .dots) {

  n <- nrow(srv$df)
  # Gather dots and filter data
  dots <- lazyeval::all_dots(.dots, ...)
  srv$df <- dplyr::filter_(srv$df, .dots = dots)

  # Update entities if the association is set and rows have been dropped
  has_me <- any(stri_detect(srv$mm$latent, regex = "mainentity"), na.rm = TRUE)
  if (has_me && n == nrow(srv$df)) {
    warning("No change in entities after filter().", call. = FALSE)
  } else if (has_me && nrow(srv$df) < n && nrow(srv$df) > 0L) {
    srv <- add_entities(srv)
  } else {
    warning("Entities could not be updated.", call. = FALSE)
  }

  # Return
  srv

}
