# Basic dplyr verbs ------------------------------------------------------------
#' @export
tbl_vars.Survey <- function(x) x$names()

#' @export
select_.Survey <- function(x, ...) {
  # dplyr::select also allows renaming variables when called.
  # e.g., select(x, new_name = old_var)
  dots <- lazyeval::all_dots(...)
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
  x$do(f, lazyeval::all_dots(...), assign = FALSE)
}

#' @export
arrange_.Survey <- function(x, ...) {
  f <- get("arrange_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...), assign = FALSE)
}

#' @export
group_by_.Survey <- function(x, ...) {
  # TODO: "add" is not passed to next call. Error message.
  f <- get("group_by_", asNamespace("dplyr"))
  x$do(f, list(...), assign = FALSE)
}

#' @export
groups.Survey <- function(x, ...) {
  f <- get("groups", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...), assign = FALSE)
}

#' @export
ungroup.Survey <- function(x, ...) {
  f <- get("ungroup", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...), assign = FALSE)
}

#' @export
mutate_.Survey <- function(x, ...) {
  f <- get("mutate_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...), assign = FALSE)
}

#' @export
summarise_.Survey <- function(x, ...) {
  f <- get("summarise_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...), assign = FALSE)
}

# Binds ------------------------------------------------------------------------

# Generic bind_rows
bind_rows <- function(x, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package required to use bind_rows.")
  }
  UseMethod("bind_rows")
}

bind_rows.default <- function(...) {
  dplyr::bind_rows(...)
}

bind_rows.Survey <- function(x, ...) {
  f <- get("bind_rows", asNamespace("dplyr"))
  x$do_merge(f, list(...), assign = FALSE)
}


# Generic bind_cols
bind_cols <- function(x, ...) {
  if (!requireNamespace("dplyr")) {
    stop("dplyr package required to use bind_rows.", call. = FALSE)
  }
  UseMethod("bind_cols")
}

bind_cols.default <- function(...) {
  dplyr::bind_cols(...)
}

bind_cols.Survey <- function(x, ...) {
  f <- get("bind_cols", asNamespace("dplyr"))
  x$do_merge(f, list(...), assign = FALSE)
}

# Joins ------------------------------------------------------------------------

left_join.Survey <- function(x, y, ...) {
  f <- get("left_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...), assign = FALSE)
}

right_join.Survey <- function(x, y, ...) {
  f <- get("right_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...), assign = FALSE)
}


full_join.Survey <- function(x, y, ...) {
  f <- get("full_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...), assign = FALSE)
}

semi_join.Survey <- function(x, y, ...) {
  f <- get("semi_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...), assign = FALSE)
}

anti_join.Survey <- function(x, y, ...) {
  f <- get("anti_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...), assign = FALSE)
}
