#' @export
gather_.Survey <- function(x, ...) {
  f <- get("gather", asNamespace("tidyr"))
  x$do(f, lazyeval::lazy_eval(lazyeval::lazy_dots(...)), assign = FALSE)
}

#' @export
spread_.Survey <- function(x, key_col, value_col, ...) {
  f <- get("spread", asNamespace("tidyr"))
  # dots <- c(eval(substitute(key_col)), eval(substitute(value_col)), capture_dots(...))
  x$do(f, capture_dots(key_col, value_col, ...), assign = FALSE)
}

#' @export
complete_.Survey <- function(x, ...) {
  f <- get("complete_", asNamespace("tidyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
expand_.Survey <- function(x, ...) {
  f <- get("expand_", asNamespace("tidyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
fill_.Survey <- function(x, ...) {
  f <- get("fill_", asNamespace("tidyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

