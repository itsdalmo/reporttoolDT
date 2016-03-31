#' dplyr: Methods for Survey.
#'
#' Surveys include support for \code{dplyr}. See \code{dplyr} documentation
#' for further information.
#'
#' NOTE: In order for these verbs to function properly, dplyr must be attached
#' with \code{library} or \code{require} before \code{reporttoolDT}. (I think).
#'
#' @param x A survey object.
#' @param ... Further arguments passed to \code{dplyr}.
#' @author Kristian D. Olsen
#' @name dplyr_verbs

#' @rdname dplyr_verbs
#' @export
tbl_vars.Survey <- function(x) x$names()

#' @rdname dplyr_verbs
#' @export
select_.Survey <- function(x, ...) {
  # dplyr::select also allows renaming variables when called.
  # e.g., select(x, new_name = old_var)
  dots <- lazyeval::all_dots(...)
  vars <- renamed_vars(dots)
  if (length(vars))
    vars <- recode_(names(x), vars)

  f <- get("select_", asNamespace("dplyr"))
  x$do(f, dots, renamed = vars)
}

#' @rdname dplyr_verbs
#' @export
rename_.Survey <- function(x, ...) {
  dots <- lazyeval::all_dots(...)
  vars <- renamed_vars(dots)
  if (length(vars))
    vars <- recode_(x$names(), vars)

  f <- get("rename_", asNamespace("dplyr"))
  x$do(f, dots, renamed = vars)
}

#' @rdname dplyr_verbs
#' @export
filter_.Survey <- function(x, ...) {
  f <- get("filter_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...))
}

#' @rdname dplyr_verbs
#' @export
arrange_.Survey <- function(x, ...) {
  f <- get("arrange_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...))
}

#' @rdname dplyr_verbs
#' @export
group_by_.Survey <- function(x, ...) {
  # TODO: "add" is not passed to next call. Error message.
  f <- get("group_by_", asNamespace("dplyr"))
  x$do(f, list(...))
}

#' @rdname dplyr_verbs
#' @export
groups.Survey <- function(x, ...) {
  f <- get("groups", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...))
}

#' @rdname dplyr_verbs
#' @export
ungroup.Survey <- function(x, ...) {
  f <- get("ungroup", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...))
}

#' @rdname dplyr_verbs
#' @export
mutate_.Survey <- function(x, ...) {
  f <- get("mutate_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...))
}

#' @rdname dplyr_verbs
#' @export
summarise_.Survey <- function(x, ...) {
  f <- get("summarise_", asNamespace("dplyr"))
  x$do(f, lazyeval::all_dots(...))
}

#' dplyr: Methods for Survey.
#'
#' \code{dplyr} bind methods for Survey.
#'
#' NOTE: In order for these verbs to function properly, dplyr must be attached
#' with \code{library} or \code{require} before \code{reporttoolDT}. (I think).
#'
#' @param x A survey object.
#' @param ... Further arguments passed to \code{dplyr}.
#' @author Kristian D. Olsen
#' @name dplyr_binds

#' @rdname dplyr_binds
#' @export
bind_rows <- function(x, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package required to use bind_rows.")
  }
  UseMethod("bind_rows")
}

#' @export
bind_rows.default <- function(x, ...) {
  dplyr::bind_rows(x, ...)
}

#' @rdname dplyr_binds
#' @export
bind_rows.Survey <- function(x, ...) {
  f <- get("bind_rows", asNamespace("dplyr"))
  x$do_merge(f, list(...))
}


#' @rdname dplyr_binds
#' @export
bind_cols <- function(x, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package required to use bind_rows.", call. = FALSE)
  }
  UseMethod("bind_cols")
}

#' @export
bind_cols.default <- function(x, ...) {
  dplyr::bind_cols(x, ...)
}

#' @rdname dplyr_binds
#' @export
bind_cols.Survey <- function(x, ...) {
  f <- get("bind_cols", asNamespace("dplyr"))
  x$do_merge(f, list(...))
}

# Joins ------------------------------------------------------------------------

#' dplyr: Methods for Survey.
#'
#' \code{dplyr} join methods for Survey.
#'
#' NOTE: In order for these verbs to function properly, dplyr must be attached
#' with \code{library} or \code{require} before \code{reporttoolDT}. (I think).
#'
#' @param x A \code{Survey} object.
#' @param y A \code{Survey}, regular \code{data.frame}, \code{data.table} or \code{tbl}.
#' @param ... Further arguments passed to \code{dplyr}.
#' @author Kristian D. Olsen
#' @name dplyr_joins

#' @rdname dplyr_joins
#' @export
left_join.Survey <- function(x, y, ...) {
  f <- get("left_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...))
}

#' @rdname dplyr_joins
#' @export
right_join.Survey <- function(x, y, ...) {
  f <- get("right_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...))
}

#' @rdname dplyr_joins
#' @export
full_join.Survey <- function(x, y, ...) {
  f <- get("full_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...))
}

#' @rdname dplyr_joins
#' @export
semi_join.Survey <- function(x, y, ...) {
  f <- get("semi_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...))
}

#' @rdname dplyr_joins
#' @export
anti_join.Survey <- function(x, y, ...) {
  f <- get("anti_join", asNamespace("dplyr"))
  x$do_merge(f, list(y, ...))
}
