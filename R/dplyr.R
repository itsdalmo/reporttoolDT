#' @export
tbl_vars.Survey <- function(x) x$names()

#' @export
select_.Survey <- function(x, ...) {
  dots <- lazyeval::all_dots(..., all_named = FALSE)

  # dplyr::select allows renaming variables when called. (e.g., select(x, new_name = old_var))
  vars <- renamed_vars(dots)
  if (length(vars))
    vars <- replace_all(names(x), vars)

  f <- get("select_", asNamespace("dplyr"))
  x$do(f, dots, renamed = vars, assign = FALSE)
}

#' @export
rename_.Survey <- function(x, ...) {
  dots <- lazyeval::all_dots(...)
  vars <- renamed_vars(dots)
  if (length(vars))
    vars <- replace_all(x$names(), vars)

  f <- get("rename_", asNamespace("dplyr"))
  x$do(f, dots, renamed = vars, assign = FALSE)
}

#' @export
filter_.Survey <- function(x, ...) {
  f <- get("filter_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
arrange_.Survey <- function(x, ...) {
  f <- get("arrange_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
group_by_.Survey <- function(x, ..., add = FALSE) {
  # TODO: "add" is not passed to next call. Error message.
  f <- get("group_by_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
groups.Survey <- function(x, ...) {
  f <- get("groups", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
ungroup.Survey <- function(x, ...) {
  f <- get("ungroup", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
mutate_.Survey <- function(x, ...) {
  f <- get("mutate_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
mutate_each_.Survey <- function(x, ...) {
  f <- get("mutate_each_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
summarise_.Survey <- function(x, ...) {
  f <- get("summarise_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}

#' @export
summarise_each_.Survey <- function(x, ...) {
  f <- get("summarise_each_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(..., all_named = TRUE), assign = FALSE)
}
