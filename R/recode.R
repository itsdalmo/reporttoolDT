#' Recode
#'
#' A simple function for recoding, which aims to replace nested \code{ifelse} statements.
#' Typical use cases include; recoding numeric vectors into a factor, merging factor levels in
#' an existing factor, and more.
#'
#' @param x The vector to be recoded.
#' @param ... Named arguments with the criteria for the recodes. In essence \code{recode = criteria}.
#' \code{.} can also be used as a shorthand for the vector given in \code{by}.
#' @param by The vector to recode values in \code{x} 'by'. Defaults to \code{x}.
#' @param dots A named list with either values that can be used with \code{\%in\%} or
#' a boolean values.
#' @param drop Drop factor levels after recode. Will only remove levels that have
#' no remaining observations after being explicitly recoded away from.
#' @param add Set to \code{TRUE} to add levels to an existing factor variable.
#' @param factor Convert \code{numeric} or \code{character} vectors to
#' factor after recoding. Named recodes will be used as levels.
#' @param arrange Set to \code{TRUE} to alphabetically sort the factor levels.
#' @param small Set to \code{TRUE} if you want small (5 year) age groups.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' recode(1:10L, small = 1:6L, medium = 7:8L, large = 9:10L)
#' recode_(1:10L, list(small = 1:6L, medium = 7:8L, large = 9:10L))
#' spread_10(1:10L)
#' spread_100(c(50, 65, 90))

recode <- function(x, ..., by = x, drop = TRUE, add = FALSE, factor = FALSE, arrange = FALSE) {
  # Lazily evaluate dots after replacing . with 'by'
  dots <- lapply(lazyeval::lazy_dots(...), lazyeval::lazy_eval, data = list(. = by))
  recode_(x, dots = dots, by = by, drop = drop, add = add, factor = factor, arrange = arrange)
}

#' @rdname recode
#' @export
recode_ <- function(x, dots, by = x, ...) {
  if (is.character(dots))
    dots <- as.list(dots)
  if (length(x) != length(by)) {
    stop("Arguments 'x' and 'by' must be the same length.", call. = FALSE)
  } else if (!is_list(dots) || !is_named(dots)) {
    stop("'dots' must be a named list.")
  }

  UseMethod("recode_")
}

#' @export
recode_.character <- function(x, dots, by = x, factor = FALSE, ...) {
  sets <- vapply(dots, function(x) {is.character(x) || is.numeric(x)}, logical(1))
  if (any(sets)) dots[sets] <- lapply(dots[sets], function(x) by %in% x)

  res <- recode_impl(x, subsets = dots)
  if (factor) res <- factor(res, levels = names(dots))
  res
}

#' @export
recode_.numeric <- recode_.character

#' @export
recode_.factor <- function(x, dots, by = x, drop = TRUE, add = FALSE, arrange = FALSE, ...) {
  if (!all(names(dots) %in% levels(x)) && !add)
    stop("Some arguments do not match existing factor levels. Set add = TRUE to override.", call. = FALSE)
  sets <- vapply(dots, function(x) {is.character(x) || is.numeric(x)}, logical(1))
  if (any(sets)) dots[sets] <- lapply(dots[sets], function(x) by %in% x)
  res <- recode_impl(as.character(x), subsets = dots)
  lvls <- levels(x)

  # Factor levels
  if (drop) lvls <- setdiff(lvls, setdiff(unique(as.character(x)), unique(res)))
  if (add) lvls <- union(lvls, names(dots))
  if (arrange) lvls <- lvls[stringi::stri_order(lvls)]

  factor(res, levels = lvls)
}

#' @rdname recode
#' @export
spread_10 <- function(x) {
  if (!is.numeric(x)) {
    stop("Expecting a numeric vector.")
  } else if (!is.integer(x)) {
    if (any_fractions(x))
      stop("This function should not be used with fractions, only wholenumbers.")
    x <- as.integer(x)
  }

  recode_(x, list("1-6" = 1:6L, "7-8" = 7:8L, "9-10" = 9:10L), factor = TRUE)

}

#' @rdname recode
#' @export
spread_100 <- function(x) {
  if (!is.numeric(x)) {
    stop("Expecting a numeric vector.")
  }

  rec <- list("-60" = x < 60L, "60-75" = x >= 60L & x <= 75L, "75-100" = x > 75L & x <= 100L)
  recode_(x, rec, factor = TRUE)

}

#' @rdname recode
#' @export
spread_age <- function(x, small = FALSE) {
  if (!is.numeric(x)) {
    stop("Expecting a numeric vector.")
  } else if (!is.integer(x)) {
    if (any_fractions(x))
      stop("This function should not be used with fractions, only wholenumbers.")
    x <- as.integer(x)
  }

  if (small) {
    rec <- list(
      "15-24" = 15:24L, "25-29" = 25:29L, "30-34" = 30:34L, "35-39" = 35:39L,
      "40-44" = 40:44L, "45-49" = 45:49L, "50-54" = 50:54L, "55-59" = 55:59L,
      "60-64" = 60:64L, "65-69" = 65:69L, "70-74" = 70:74L, "75+" = x >= 75L)
  } else {
    rec <- list("16-29" = 16:29L, "30-44" = 30:44L, "45-59" = 45:59L, "60+" = x >= 60L)
  }

  recode_(x, rec, factor = TRUE)
}


recode_impl <- function(x, subsets) {

  is_null <- vapply(subsets, is.null, logical(1))
  if (any(is_null)) {
    null <- names(subsets)[is_null]
    stop("Arguments evaluated to NULL:\n", str_list(null))
  }

  is_logical <- vapply(subsets, is.logical, logical(1))
  if (any(!is_logical)) {
    logical <- names(subsets)[!is_logical]
    stop("Arguments did not evaluate to a logical:\n", str_list(logical))
  }

  # Return early if there is nothing to recode
  recodes <- vapply(subsets, any, logical(1))
  if (!any(recodes)) {
    warning("No matches for recode. Vector is unchanged.", call. = FALSE)
    return(x)
  }

  # Warn if the recodes overlap
  overlap <- unlist(lapply(subsets, which))
  overlap <- any(duplicated(overlap))
  if (overlap) {
    warning("Values are being recoded multiple times. Check results.", call. = FALSE)
  }

  # Convert x to character and recode
  for (nm in names(subsets)) {
    id <- subsets[[nm]]
    x[id] <- nm
  }

  # Return
  x

}
