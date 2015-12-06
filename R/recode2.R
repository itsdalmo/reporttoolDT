<<<<<<< HEAD
#' Recode
#'
#' A simple function for recoding, which aims to replace nested \code{ifelse} statements.
#' Typical use cases include; recoding numeric vectors into a factor, merging factor levels in
#' an existing factor, and more.
#' 
#' @param x A vector to be recoded.
#' @param ... Named arguments with the criteria for the recodes. In essence \code{recode = criteria}.
#' \code{.} can also be used as a shorthand for the vector given in \code{by}.
#' @param by The vector to recode values in \code{x} 'by'.
#' @param dots A named list with either values that can be used with \code{\%in\%} or 
#' a boolean values.
#' @param drop Drop factor levels that have been recoded away from, and it has no remaining observations
#' after the recode.
#' @param add Set to \code{TRUE} to add levels to an existing factor variable.
#' @param as_factor Convert \code{numeric} or \code{character} vectors to
#' factor after recoding. Named recodes will be used as levels.
#' @param arrange Set to \code{TRUE} to alphabetically sort the factor levels.
#' @author Kristian D. Olsen
#' @export
#' @examples 
#' df %>% mutate(loy = recode(q15b, "Neg" = 1:6L, "Neu" = 7:8L, "Pos" = 9:10L, as_factor = TRUE))

recode <- function(x, ..., by = x, drop = TRUE, add = FALSE, as_factor = FALSE, arrange = FALSE) {
  
  # Lazily evaluate dots after replacing . with 'by'
  dots <- lazyeval::lazy_dots(...)
  dots <- lapply(dots, lazyeval::interp, .values = list(. = by))
  dots <- lapply(dots, lazyeval::lazy_eval)
 
  # Use recode_ with the resulting list of recodes
  recode_(x, dots = dots, by = by, drop = drop, add = add, as_factor = as_factor, arrange = arrange)
  
}

#' @rdname recode
#' @export
recode_ <- function(x, dots, by = x, ...) {
  if (length(x) != length(by)) stop("Arguments 'x' and 'by' must be the same length.", call. = FALSE)
  UseMethod("recode_")
}

#' @rdname recode
#' @export
recode_.default <- function(x, dots, by, as_factor = FALSE, ...) {
  sub <- lapply(dots, function(x) if (is.character(x) || is.numeric(x)) by %in% x else x)
  res <- recode_var(x, subsets = sub)
  if (as_factor) res <- factor(res, levels = names(sub))
  res
}

#' @rdname recode
#' @export
recode_.factor <- function(x, dots, by, drop = TRUE, add = FALSE, arrange = FALSE, ...) {
  if (!all(names(dots) %in% levels(x)) && !add)
    stop("Some arguments do not match existing factor levels. Set 'add' to TRUE to override.", call. = FALSE)
  sub <- lapply(dots, function(x) if (is.character(x) || is.numeric(x)) by %in% x else x)
  res <- recode_var(as.character(x), subsets = sub)
  lvls <- levels(x)
  
  # Factor levels
  if (drop) lvls <- setdiff(lvls, setdiff(unique(as.character(x)), unique(res)))
  if (add) lvls <- union(lvls, names(dots))
  if (arrange) lvls <- lvls[stri_order(lvls)]
  
  factor(res, levels = lvls)
}

recode_var <- function(x, subsets) {
  
  # Check the arguments
  is_null <- vapply(subsets, is.null, logical(1))
  if (any(is_null)) {
    null <- names(subsets)[is_null]
    stop("Some of the arguments evaluate to NULL:\n", 
         conjunct_string(null), call. = FALSE)
  }
  
  # Must be logical
  is_logical <- vapply(subsets, is.logical, logical(1))
  if (any(!is_logical)) {
    stop("Some of the arguments are not boolean (TRUE/FALSE):\n", 
         conjunct_string(names(is_logical[!is_logical])), call. = FALSE)
  } 
  
  # Check that something is recoded
  is_recoding <- vapply(subsets, any, logical(1))
  if (any(!is_recoding)) {
    warning("The expression for the following recodes resulted in no matches:\n", 
            conjunct_string(stri_c("'", names(is_recoding[!is_recoding]), "'")), call. = FALSE)
  }
  
  # Warn if the recodes overlap
  overlap <- unlist(lapply(subsets, which))
  if (length(overlap) != length(unique(overlap))) {
    warning("Values are being recoded multiple times. Check results.", call. = FALSE)
  }
  
  # Convert x to character and recode
  for (nm in names(subsets)) {
    by_subset <- subsets[[nm]]
    x[by_subset] <- nm
  }
  
  # Return
  x
  
}
=======
#' @export
recode <- function(x, ..., by = x) UseMethod("recode")

recode.default <- function(x, ..., by = x) {
  dots <- lazyeval::lazy_dots(...)
  dots
}

recode.factor <- function(x, ..., by = x) {
  dots <- lazyeval::lazy_eval(...)
  dots
}


# recode <- function(x, ..., by = x, drop = TRUE, add = FALSE, as_factor = FALSE) {
#
#   dots <- lazyeval::lazy_dots(...)
#
#   # x and by must be same length
#   if (length(x) != length(by)) {
#     stop("Arguments 'x' and 'by' must be the same length.", call. = FALSE)
#   } else if (!identical(x, by)) {
#     # Don't drop levels when recoding by another variable
#     drop <- FALSE
#   }
#
#   # Replace . with by and evaluate subsets
#   subsets <- lapply(dots, lazyeval::interp, .values = list(. = by))
#   subsets <- lapply(subsets, lazyeval::lazy_eval)
#
#   # Check the arguments
#   is_null <- vapply(subsets, is.null, logical(1))
#   if (any(is_null)) {
#     null <- names(subsets)[is_null]
#     stop("Some of the arguments evaluate to NULL:\n",
#          conjunct_string(null), call. = FALSE)
#   }
#
#   # Check which vectors do not evaluate to logical, and %in% them.
#   subsets <- lapply(subsets, function(x) if (is.character(x) || is.numeric(x)) by %in% x else x)
#
#   # Must be logical
#   is_logical <- vapply(subsets, is.logical, logical(1))
#   if (any(!is_logical)) {
#     stop("Some of the arguments are not boolean (TRUE/FALSE):\n",
#          conjunct_string(names(not_logical[!is_logical])), call. = FALSE)
#   }
#
#   # Check that something is recoded
#   is_recoding <- vapply(subsets, any, logical(1))
#   if (any(!is_recoding)) {
#     stop("The expression for the following recodes resulted in no matches:\n",
#          conjunct_string(stri_c("'", names(is_recoding[!is_recoding]), "'")), call. = FALSE)
#   }
#
#   # For factors, names must match the levels
#   if (is.factor(x) && !add) {
#     missing <- setdiff(names(subsets), levels(x))
#     if (length(missing)) {
#       stop("Some named arguments do not match existing factor levels:\n",
#            conjunct_string(missing), call. = FALSE)
#     }
#   }
#
#   # Warn if the recodes overlap
#   overlap <- unlist(lapply(subsets, which))
#   if (length(overlap) != length(unique(overlap))) {
#     warning("Values are being recoded multiple times. Check results.", call. = FALSE)
#   }
#
#   # Factors require special attention
#   old_levels <- levels(x)
#   new_levels <- old_levels
#
#   # Convert x to character and recode
#   x <- as.character(x)
#   for (nm in names(subsets)) {
#
#     by_subset <- subsets[[nm]]
#
#     # Store values that should be added/dropped for factors
#     if (!is.null(old_levels)) {
#       if (drop && add) {
#         new_levels[new_levels %in% x[by_subset]] <- nm
#         new_levels <- unique(new_levels)
#       } else if (add) {
#         new_levels <- union(new_levels, nm)
#       } else if (drop) {
#         new_levels <- setdiff(new_levels, x[by_subset])
#       }
#     }
#
#     # Do the recode
#     x[by_subset] <- nm
#
#   }
#
#   # Convert to desired output format
#   if (!is.null(old_levels)) {
#     # I.e., if it WAS a factor
#     x <- factor(x, levels = new_levels)
#   } else if (as_factor) {
#     # Coerce to factor based on recodes
#     x <- factor(x, levels = names(subsets))
#   }
#
#   # Return
#   x
#
# }
>>>>>>> origin/master
