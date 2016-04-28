#' qtable
#'
#' A function for generating "quick" tables for one or more variables (does not support mixed types).
#' By default it generates proportions for \code{factor} and \code{character} vectors,
#' means for \code{numeric} and \code{integer}, and min/max for \code{Date} (including POSIX)
#' vectors. It always includes the number of observations for each variable.
#' When producing wide tables, uneven counts are separate with \code{/}.
#'
#' @param df A \code{data.frame}, \code{data.table} or \code{Survey}.
#' @param vars The variables to aggregate.
#' @param groups Variables to group by.
#' @param margin Set to TRUE to generate a margin for the first variable in groups.
#' @param wide Should a long or a wide table be returned? Wide tables spread levels for
#' \code{factor} and unique values for \code{character}. For a single \code{numeric},
#' the last group is used, while multiple \code{numeric} will be spread by variable names.
#' @param weight A variable to weight results by.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # TODO

qtable <- function(df, vars, groups = NULL, margin = TRUE, wide = TRUE, weight = NULL) UseMethod("qtable")

#' @export
qtable.Survey <- function(df, vars, groups = NULL, margin = TRUE, wide = TRUE, weight = NULL) {
  qtable(df$data, vars, groups, margin, wide, weight)
}

#' @export
qtable.data.frame <- function(df, vars, groups = NULL, margin = TRUE, wide = TRUE, weight = NULL) {
  df <- data.table::as.data.table(df)
  as.data.frame(qtable_impl(df, vars, groups, margin, wide, weight))
}

#' @export
qtable.data.table <- function(df, vars, groups = NULL, margin = TRUE, wide = TRUE, weight = NULL) {
  df <- data.table::copy(df)
  qtable_impl(df, vars, groups, margin, wide, weight)
}

qtable_impl <- function(df, vars, groups = NULL, margin = TRUE, wide = TRUE, weight = NULL) {
  if (!length(vars)) {
    stop("No variables specified.")
  } else if (any(vars %in% groups)) {
    stop("Cannot group by and aggregate the same variable.")
  }

  # Subset data and make sure variables are specified correctly ----------------
  df <- df[, c(groups, vars), with = FALSE]
  type <- unique(simple_classes(df[, vars, with = FALSE]))

  if (length(vars) > 1L) {
    if (length(type) != 1L) stop("qtable does not support mixed classes.")
    if (type == "factor") {
      # Factors must have identical levels when spreading.
      levels <- lapply(df[, vars, with = FALSE], levels)
      levels <- unlist(lapply(levels[-1L], identical, levels[[1L]]))
      if (!all(levels) && wide) stop("All factors must have identical levels if wide is TRUE.")
    } else if (type != "numeric") {
      stop("qtable does not support multiple variables if all classes are not factor, or numeric.")
    }
  }

  # rbind to add an average for the first group
  # melt to long format and set groups as key.
  if (!is.null(groups) && margin)
    df <- rbind(data.table::copy(df), df[, groups[1] := "Total", with = FALSE])
  df <- data.table::melt(df, groups, vars, value.factor = isTRUE(type == "factor"))
  data.table::setkeyv(df, groups)

  # Aggregate the data and spread if desired
  # character/factor -----------------------------------------------------------
  if (type == "character" || type == "factor") {
    df <- df[, .(n = .N), by = c(groups, "variable", "value")]
    df[, proportion := prop.table(n), by = c(groups, "variable")]
    if (wide) {
      df[, n := sum(n), by = c(groups, "variable")]
      fm <- stri_c(c(groups, "n"), collapse = "+")
      fm <- stri_c(fm, "~ value", collapse = " ")
      df <- data.table::dcast(df, formula = fm, value.var = "proportion")
    }

  # numeric --------------------------------------------------------------------
  } else if (type == "numeric") {
    df <- df[, .(n = .N, value = mean(value, na.rm = TRUE)), by = c(groups, "variable")]
    if (wide) {
      if (length(groups) > 1L && length(vars) == 1L) {
        spread_by <- tail(groups, 1L)
        group_by <- setdiff(groups, spread_by)
        df[, n := as.character(n)][, n := stri_c(n, collapse = "/"), by = group_by]
        fm <- stri_c(c(group_by, "n"), collapse = "+")
        fm <- stri_c(fm, "~", spread_by)
      } else {
        df[, n := as.character(n)][, n := stri_c(n, collapse = "/"), by = groups]
        fm <- stri_c(c(groups, "n"), collapse = "+")
        fm <- stri_c(fm, "~ variable", collapse = " ")
      }

      df <- data.table::dcast(df, formula = fm, value.var = "value")
    }

  # date -----------------------------------------------------------------------
  } else if (type == "date") {
    df <- df[, .(n = .N, min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE)), by = c(groups, "variable")]
    if (!wide) {
      df <- data.table::melt(df, groups, c("min", "max"), variable.name = "type")
    }
  # error ----------------------------------------------------------------------
  } else {
    stop("qtable does not support variables of class ", stri_c("'", type, "'"))
  }

  df[]

}

simple_classes <- function(x) {
  stopifnot(is.data.frame(x))
  out <- vapply(x, function(x) class(x)[1], character(1))
  out <- ifelse(out == "integer", "numeric", out)
  out <- ifelse(out %in% c("POSIXct", "POSIXt", "Date"), "date", out)
  out
}

# complete_df <- function(df, vars) {
#   cj <- df[, vars, with = FALSE]
#   cj <- lapply(cj, function(x) { if (is.factor(x)) levels(x) else unique(x) })
#   cj <- expand.grid(cj)
#
#   data.table::setkeyv(df, vars)
#   df[cj]
# }
