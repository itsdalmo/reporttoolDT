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

  # Extract first occurrence of a 1-2 digit number and convert to integer.
  var <- stringi::stri_extract_first(var, regex = "[0-9]{1,2}")
  var <- suppressWarnings(as.numeric(var))

  # Set all values other than 1 - 10 as NA. Return
  var[var > 10L | var < 1L] <- NA
  var

}

#' Rescale 1-10 integer to 0-100 numeric.
#'
#' Takes vectors representing 10-point likert scales and transforms them to
#' 100-point scales (\code{numeric}). Formula: \code{(x-1)*(100/9)}
#'
#' @param var An \code{integer} vector.
#' @author Kristian D. Olsen
#' @seealso \code{\link{rescale_10}} for the complement.
#' @export
#' @examples
#' x <- rescale_100(c(1L, 10L))
#' identical(x, c(0, 100))

rescale_100 <- function(var) {
  if (!is.numeric(var))
    stop("'var' should be numeric.")

  # Rescale from 10 point to 100 point scale
  var <- (var-1L)*(100L/9L)

  # Set values other than 0 - 100 as NA. Return
  var[var > 100L | var < 0L] <- NA
  var

}

#' Rescale 0-100 numeric to 1-10 integer.
#'
#' Takes vectors representing 100-point scales and transforms them to
#' 10-point likert-scales (\code{numeric}).
#'
#' @param var A \code{numeric} vector.
#' @author Kristian D. Olsen
#' @seealso \code{\link{rescale_100}} for the complement.
#' @export
#' @examples
#' x <- rescale_100(c(1L, 10L))
#' x <- rescale_10(x)
#' identical(x, c(1L, 10L))

rescale_10 <- function(var) {
  if (!is.numeric(var))
    stop("'var' should be numeric.")

  # Rescale from 10 point to 100 point scale
  var <- (var-0L)/100L * 9L + 1L

  # Set values other than 0 - 100 as NA. Return
  var[var > 10L | var < 1L] <- NA
  var
}

#' Convert variable to scale
#'
#' Takes a character or factor variable and converts it to a factor with labels
#' ordered by numerals in the label. Labels with duplicated numerals will be
#' merged in the output.
#'
#' Note: \code{as_scale} is convenient for converting likert scales from \code{character}
#' to \code{factor}. This tends to happend when merging data with e.g.
#' \code{\link[dplyr]{bind_rows}}.
#'
#' @param var A \code{numeric} vector.
#' @author Kristian D. Olsen
#' @seealso \code{\link{rescale_10}} to convert 100-point scales to 10-point.
#' @export
#' @examples
#' NULL

as_scale <- function(var) {
  UseMethod("as_scale")
}

#' @export
as_scale.numeric <- function(var) {
  labs <- unique(var)[order(unique(var))]
  factor(as.character(var), levels = as.character(labs))
}

#' @export
as_scale.character <- function(var) {
  # Get labels and numerals in labels
  lab <- unique(var)
  num <- str_to_numeric(lab, FUN = stop, " <- ", "Labels contain multiple numerals.")
  out <- factor(var, levels = lab[order(num)])

  # Also recode labels if there are any duplicates
  is_dup <- duplicated(num) & !is.na(num)
  if (!any(is_dup)) return(out)

  for (i in num[is_dup]) {
    ID <- num == i & !is.na(num)
    out <- recode_(out, setNames(list(lab[ID]), lab[ID][1L]))
  }

  out
}

#' @export
as_scale.factor <- function(var) {
  as_scale(as.character(var))
}

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