#' @export
tbl_vars.Survey <- function(x) x$names()

#' @export
groups.Survey <- function(x) {
  f <- get("groups", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
ungroup.Survey <- function(x) {
  f <- get("ungroup", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
group_by_.Survey <- function(x, ..., add = FALSE) {
  f <- get("group_by_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
summarise_.Survey <- function(x, ...) {
  f <- get("summarise_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
arrange_.Survey <- function(x, ...) {
  f <- get("arrange_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
mutate_.Survey <- function(x, ...) {
  f <- get("mutate_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
select_.Survey <- function(x, ...) {
  f <- get("select_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
rename_.Survey <- function(x, ...) {
  # TODO
  f <- get("rename_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
filter_.Survey <- function(x, ...) {
  f <- get("filter_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}