qtable <- function(srv, ..., groups = NULL, wide = TRUE) {
  dots <- lazyeval::lazy_dots(...)
  qtable_(srv, dots, groups, wide)
}

qtable_ <- function(df, ...) UseMethod("qtable_")

qtable_.survey <- function(df, vars, groups = NULL, wide = TRUE) {

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

qtable_.data.frame <- function(df, vars, groups = NULL, wide = TRUE) {
  df <- data.table::as.data.table(df)
  as.data.frame(qtable_(df, vars, groups, wide))
}

qtable_.data.table <- function(df, vars, groups = NULL, wide = TRUE) {
  if (!length(vars)) stop("No variables specified.", call. = FALSE)

  # Subset variables and check type
  cols <- c(groups, vars)
  df <- df[, cols, with = FALSE]

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
    qtable_factor(df, vars, groups, wide)
  } else if (type == "Date") {
    qtable_date(df, vars, groups, wide)
  }

}

# Create tables based on type of variable --------------------------------------
qtable_numeric <- function(df, vars, groups, wide) {
  # df[, n := .N, by = groups]
  if (!is.null(groups)) df <- complete_df(df, groups)
  df[, lapply(.SD, mean, na.rm = TRUE), .SDcols = vars, keyby = groups][]
}

qtable_factor <- function(df, var, groups, wide) {
  df <- df[, list("n" = .N), keyby = c(groups, var)]
  df <- complete_df(df, c(var, groups))
  df <- df[is.na(n), n := 0][, proportion := prop.table(n), keyby = groups]

  if (wide && !is.null(groups)) {
    df[, n := sum(n), by = groups]
    df <- data.table::dcast(df, stri_c(groups, "+", "n", "~", var), value.var = "proportion")
  }

  df[]

}

qtable_date <- function(df, var, groups, wideL) {
  if (!is.null(groups)) df <- complete_df(df, groups)
  df[, list("start" = min(var, na.rm = TRUE), "end" = min(var, na.rm = TRUE))][]
}


# Complete datasets ------------------------------------------------------------
complete_df <- function(df, vars) {
  cj <- df[, vars, with = FALSE]
  cj <- lapply(cj, function(x) { if (is.factor(x)) levels(x) else unique(x) })
  cj <- expand.grid(cj)

  data.table::setkeyv(df, vars)
  df[cj]
}