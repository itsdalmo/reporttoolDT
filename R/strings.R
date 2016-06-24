#' Extract numbers from strings
#'
#' \code{str_to_numeric} lets you extract numbers from strings and convert them
#' to a numeric vector. If a single element in the character vector contains multiple
#' numbers, you can specify an aggregate function. See examples.
#'
#' @param x A character vector.
#' @param FUN If a string contains several numbers, you can specify an aggregate
#' function. By default this function returns the mean of all numbers, but you can use
#' \code{min}, \code{max}, \code{median}, \code{head} or \code{tail} also.
#' @param ... Arguments passed to \code{FUN}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' x <- c("10", "between 0 and 10", "about 8,5")
#' y <- str_to_numeric(x, FUN = mean)
#' identical(y, c(10, 5, 8.5))
#' y <- str_to_numeric(x, FUN = head, n = 1L)
#' identical(y, c(10, 0, 8.5))

str_to_numeric <- function(x, FUN = mean, ...) {
  if (is.factor(x)) {
    x <- as.character(x)
  } else if (!is.character(x)) (
    stop("'x' should be a string or character vector.")
  )

  out <- stringi::stri_extract_all(x, regex = "[0-9,.]+")
  out <- vapply(out, function(v) {
    v <- as.numeric(stringi::stri_replace_all(v, ".", fixed = ","))
    if (length(v) > 1L) v <- do.call(FUN, list(v, ...))
    v
  }, numeric(1))

  # Return
  unlist(out)

}

#' Conjunct strings
#'
#' See examples.
#'
#' @param x A character vector.
#' @param conjunction The conjunction to use.
#' @param quote Put quotes ('') around list items. Default is \code{TRUE}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' x <- str_list(c("A", "B", "C"), conjunction = "or")
#' identical(x, c("'A', 'B' or 'C'"))

str_list <- function(x, conjunction = "and", quote = TRUE) {
  stopifnot(is.character(x))
  if (quote) x <- stri_c("'", x, "'")
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
