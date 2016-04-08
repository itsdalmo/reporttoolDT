#' Clean scale scores
#'
#' Takes vectors representing likert scales and cleans text descriptions.
#' E.g. "10 Very happy" becomes "10", and the value is converted to \code{numeric}
#' (without warning).
#'
#' @param var A \code{character} vector (\code{factor} will be coerced).
#' @author Kristian D. Olsen
#' @export
#' @examples
#' x <- clean_scale(c("1 Not happy", "10 Very happy"))
#' identical(x, c(1, 10))

clean_scale <- function(var) {
  if (is.factor(var))
    var <- as.character(var)
  if (!is.character(var))
    stop("'var' should be a factor or character vector.")

  # Set values greater than 10L to NA
  var <- stringi::stri_extract_first(var, regex = "[0-9]{1,2}")
  var <- suppressWarnings(as.integer(var))

  # 10 point scale and return.
  var[var > 10L | var < 1L] <- NA
  var

}

#' Rescale 1-10 integer to 0-100 numeric.
#'
#' Takes vectors representing 10-point likert scales and transforms them to
#' 100-point scales (\code{numeric}). Formula: \code{(x-1)*(100/9)}
#'
#' @param var A numeric vector, \code{integer} and \code{character} will be coerced.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' x <- rescale_100(c(1L, 10L))
#' identical(x, c(0, 100))

rescale_100 <- function(var) {
  if (!is.integer(var)) stop("'var' should be an integer.", call. = FALSE)
  suppressWarnings(ifelse(var %in% 1:10, (var-1)*(100/9), NA))
}

#' Conjunct strings
#'
#' See examples.
#'
#' @param x A character vector.
#' @param conjunction The conjunction to use.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' x <- str_list(c("A", "B", "C"), conjunction = "or")
#' identical(x, c("A, B or C"))

# TODO: Needs a better name.
str_list <- function(x, conjunction = "and") {
  stopifnot(is.character(x))
  if (length(x) == 1L) return(x)
  stri_c(stri_c(x[1:(length(x)-1)], collapse = ", "), conjunction, x[length(x)], sep = " ")
}

#' Trim strings
#'
#' This function pads/trims strings to the desired length. When trimming, it
#' also adds "..." to the end of strings to indicated that they have been trimmed.
#'
#' @param x A character vector.
#' @param n Desired length.
#' @param trail Appended to strings that are longer than \code{n}.
#' @param pad A string which will be used to pad short strings to the desired
#' length.
#' @param side Which side should be padded. Either "left", "right", or "both".
#' Passed to \code{stri_pad}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' x <- paste(letters[1:10], collapse = "")
#' a <- str_just(x, n = 8)
#' b <- str_just(x, n = 11, pad = " ")
#' identical(a, "abcde...")
#' identical(b, "abcdefghij ")

# TODO: Needs a better name.
str_just <- function(x, n = 50, trail = "...", pad = NULL, side = "right") {
  stopifnot(is.character(x))
  smax <- n - stri_length(trail)
  smin <- if (!is.null(pad)) n else 0

  x <- vapply(x, function(s) {
    if (is.na(s)) {
      s
    } else if (stri_length(s) > n) {
      stri_c(stri_sub(s, to = smax), trail, sep = "")
    } else if (smin > 0) {
      stri_pad(s, width = smin, pad = pad, side = side)
    } else {
      s
    }
  }, character(1))

  unname(x)
}