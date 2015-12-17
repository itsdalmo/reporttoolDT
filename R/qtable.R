qtable <- function(srv, ..., wide = TRUE, weight = TRUE, question = TRUE) {
  dots <- lazyeval::lazy_dots(...)
  qtable_(srv, dots)
}

qtable_ <- function(df, vars, groups = NULL) UseMethod("qtable_")

qtable_.survey <- function(df, vars, groups = NULL) {

  # Remove observations above cutoff (if it is not NA)
  cutoff <- as.numeric(get_config(df, "cutoff"))
  if (!is.na(cutoff) && "percent_missing" %in% names(df)) {
    df <- df[cutoff <= percent_missing, with = FALSE]
  } else {
    warning("Either cutoff is not set or 'percent_missing' is not in the data.", call. = FALSE)
  }

  NextMethod()

}

qtable_.data.frame <- function(df, vars, groups = NULL) {
  qtable_(as.data.table(df), vars, groups)
}

qtable_.data.table <- function(df, vars, groups = NULL) {
  if (!length(vars)) stop("No variables specified.", call. = FALSE)

  cols <- c(vars, groups)
  df <- df[, cols, with = FALSE]

  types <- vapply(df, function(x) class(x)[1], character(1))
  types[types == "integer"] <- "numeric"
  types[types %in% c("POSIXct", "POSIXt", "Date")] <- "time"

  invalid <- names(df)[!types %in% c("numeric", "factor", "Date")]
  if (length(invalid)) {
    types <- stri_c("(", types[!types %in% c("numeric", "factor", "Date")], ")")
    invalid <- stri_c(invalid, types, sep = " ")
    stop("The following variables are not supported:\n", join_strings(invalid), call. = FALSE)
  }

  type <- unique(types)
  if (length(type) != 1L) {
    stop("qtable does not support mixed classes.", call. = FALSE)
  }

  if (type == "numeric") {
    qtable_numeric(df, vars, groups)
  } else if (type == "factor") {
    if (length(vars) > 1L) stop("qtable can only handle 1 factor variable at a time.", call. = FALSE)
    qtable_factor(df, vars, groups)
  } else if (type == "time") {

  }

  # df <- df[, list("n" = .N, "value" = mean(value, na.rm = TRUE)), by = c(groups, "variable")]
  # df[, n := sum(n), by = groups]

#   df <- dcast(df, stri_c("...", "~", "variable"), value.var = "value", fun = mean, drop = FALSE)
#   df

}

qtable_numeric <- function(df, vars, groups = NULL) {
  if (is.null(groups)) {
    df <- df[, lapply(.SD, mean, na.rm = TRUE), .SDcols = vars]
  } else {
    df <- melt(df, id = groups, measure = vars, na.rm = TRUE)
  }
  df
}

qtable_factor <- function(df, vars, groups = NULL) {
  if (is.null(groups)) {
    cj <- CJ(levels(df[[vars]]), sorted = FALSE, unique = FALSE)
    df <- df[, list("n" = .N), keyby = vars][cj][is.na(n), n := 0][, proportion := prop.table(n)]
    df <- rbind(df, df[, setNames(list("Total", sum(n), sum(proportion)), names(df))])
  } else {
    df <- melt(df, id = groups, measure = vars, na.rm = TRUE)
  }
  df
}

qtable_date <- function(df, vars, groups = NULL) {
  df <- melt(df, id = groups, measure = vars, na.rm = TRUE)
  df
}