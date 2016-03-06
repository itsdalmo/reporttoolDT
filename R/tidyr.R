#' @export
gather_.Survey <- function(x, ...) {
  f <- get("gather_", asNamespace("tidyr"))
  x$do(f, list(...), assign = FALSE)
}

#' @export
spread_.Survey <- function(x, ...) {
  f <- get("spread_", asNamespace("tidyr"))
  x$do(f, list(...), assign = FALSE)
}

#' @export
complete_.Survey <- function(x, ...) {
  f <- get("complete_", asNamespace("tidyr"))
  x$do(f, list(...), assign = FALSE)
}

#' @export
expand_.Survey <- function(x, ...) {
  f <- get("expand_", asNamespace("tidyr"))
  x$do(f, list(...), assign = FALSE)
}

#' @export
fill_.Survey <- function(x, ...) {
  f <- get("fill_", asNamespace("tidyr"))
  x$do(f, list(...), assign = FALSE)
}

