#' tidyr: Methods for Survey.
#'
#' Surveys include support for \code{tidyr}. See \code{tidyr} documentation
#' for further information.
#'
#' NOTE: In order for these verbs to function properly, tidyr must be attached
#' with \code{library} or \code{require} before \code{reporttoolDT}. (I think).
#'
#' @param x A \code{Survey} object.
#' @param ... Further arguments passed to \code{tidyr}.
#' @author Kristian D. Olsen
#' @name tidyr_verbs

#' @rdname tidyr_verbs
#' @export
gather_.Survey <- function(x, ...) {
  f <- get("gather_", asNamespace("tidyr"))
  x$do(f, list(...))
}

#' @rdname tidyr_verbs
#' @export
spread_.Survey <- function(x, ...) {
  f <- get("spread_", asNamespace("tidyr"))
  x$do(f, list(...))
}

#' @rdname tidyr_verbs
#' @export
complete_.Survey <- function(x, ...) {
  f <- get("complete_", asNamespace("tidyr"))
  x$do(f, list(...))
}

#' @rdname tidyr_verbs
#' @export
expand_.Survey <- function(x, ...) {
  f <- get("expand_", asNamespace("tidyr"))
  x$do(f, list(...))
}

#' @rdname tidyr_verbs
#' @export
fill_.Survey <- function(x, ...) {
  f <- get("fill_", asNamespace("tidyr"))
  x$do(f, list(...))
}

