# qtable <- function(srv, ..., groups = NULL, wide = TRUE) {
#   dots <- lazyeval::lazy_dots(...)
#   qtable_(srv, dots, groups, wide)
# }

qtable <- function(df, ...) UseMethod("qtable")

qtable.survey <- function(df, vars, groups = NULL, wide = TRUE) {

  # Remove observations above cutoff (if it is not NA)
  cutoff <- as.numeric(get_config(df, "cutoff"))
  if (!is.na(cutoff)) {
    if ("percent_missing" %in% names(df)) {
      df <- df[cutoff <= percent_missing, with = FALSE]
    } else {
      warning("Cutoff has been set, but there is no 'percent_missing' variable in data.", call. = FALSE)
    }
  }

  NextMethod()

}

qtable.data.frame <- function(df, vars, groups = NULL, wide = TRUE) {
  df <- data.table::as.data.table(df)
  as.data.frame(qtable(df, vars, groups, wide))
}

qtable.data.table <- function(df, vars, groups = NULL, wide = TRUE) {
  if (!length(vars)) stop("No variables specified.", call. = FALSE)

  # Subset variables and check type
  df <- df[, c(groups, vars), with = FALSE]

  types <- vapply(df, function(x) class(x)[1], character(1))
  types[types == "integer"] <- "numeric"
  types[types %in% c("POSIXct", "POSIXt", "Date")] <- "Date"

  # Give informative error if a class is not supported
  invalid <- names(df)[!types %in% c("numeric", "factor", "Date")]
  if (length(invalid)) {
    types <- stri_c("(", types[!types %in% c("numeric", "factor", "Date")], ")")
    invalid <- stri_c(invalid, types, sep = " ")
    stop("The following variables are not supported:\n", join_strings(invalid), call. = FALSE)
  }

  # Make sure classes are not mixed
  type <- unique(types[!names(types) %in% groups])
  if (length(type) != 1L) {
    stop("qtable does not support mixed classes.", call. = FALSE)
  }

  # When doing grouped operations, also produce an average for the first group specified.
  if (!is.null(groups)) {
    df <- rbind(data.table::copy(df), df[, groups[1] := "Total", with = FALSE])
  }

  if (type == "numeric") {
    qtable_numeric(df, vars, groups, wide)
  } else if (type == "factor") {
    if (length(vars) > 1L) stop("qtable cannot handle multiple factor variables.", call. = FALSE)
    qtable_factor(df, vars, groups, wide)
  } else if (type == "Date") {
    if (length(vars) > 1L) stop("qtable cannot handle multiple date variables.", call. = FALSE)
    qtable_date(df, vars, groups, wide)
  }

}

# Create tables based on type of variable --------------------------------------
qtable_numeric <- function(df, vars, groups, wide) {
  if (!is.null(groups)) df <- complete_df(df, groups)
  df <- df[, lapply(.SD, mean, na.rm = TRUE), .SDcols = vars, keyby = groups]

  if (!is.null(groups) && wide) {
    if (length(groups) > 1L && length(vars) == 1L) {
      n <- length(groups) # Spread on last group if there is only 1 var.
      df <- data.table::dcast(df, stri_c(stri_c(groups[-n], collapse = "+"), "~", groups[n]), value.var = vars)
    }
  }

  df[]

}

qtable_factor <- function(df, vars, groups, wide) {
  df <- df[, list("n" = .N), keyby = c(groups, vars)]
  df <- complete_df(df, c(vars, groups))
  df <- df[is.na(n), n := 0][, proportion := prop.table(n), keyby = groups]

  if (!is.null(groups) && wide) {
    df[, n := sum(n), by = groups]
    df <- data.table::dcast(df, stri_c(groups, "+", "n", "~", vars), value.var = "proportion")
  }

  df[]

}

qtable_date <- function(df, vars, groups, wide) {
  if (!is.null(groups)) df <- complete_df(df, groups)
  data.table::setnames(df, vars, "date")
  df[, .("start" = min(date, na.rm = TRUE), "end" = max(date, na.rm = TRUE)), keyby = groups][]
}


# Complete datasets ------------------------------------------------------------
complete_df <- function(df, vars) {
  cj <- df[, vars, with = FALSE]
  cj <- lapply(cj, function(x) { if (is.factor(x)) levels(x) else unique(x) })
  cj <- expand.grid(cj)

  data.table::setkeyv(df, vars)
  df[cj]
}