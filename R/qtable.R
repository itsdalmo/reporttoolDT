qtable <- function(srv, ..., wide = TRUE, weight = TRUE, question = TRUE) {
  dots <- lazyeval::lazy_dots(...)
  qtable_(srv, dots)
}

qtable_ <- function(df, vars, groups = NULL) UseMethod("qtable_")

qtable_.survey <- function(df, ...) {

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

  invalid <- names(df)[!types %in% c("numeric", "factor", "Date")]
  if (length(invalid)) {
    types <- stri_c("(", types[!types %in% c("numeric", "factor", "Date")], ")")
    invalid <- stri_c(invalid, types, sep = " ")
    stop("The following variables are not supported:\n", join_strings(invalid), call. = FALSE)
  } else if (length(unique(types)) != 1L) {
    stop("qtable does not support mixed classes.", call. = FALSE)
  }

  if (!is.null(groups)) {
    df <- melt(df, id = groups, measure = vars, na.rm = TRUE)
  } else {
    # df <- df[, list("n" = . N, )]
    df <- melt(df, measure = vars, na.rm = TRUE)
  }

  # df <- df[, list("n" = .N, "value" = mean(value, na.rm = TRUE)), by = c(groups, "variable")]
  # df[, n := sum(n), by = groups]

  df <- dcast(df, stri_c("...", "~", "variable"), value.var = "value", fun = mean, drop = FALSE)
  if ("." %in% names(df)) df[, . := NULL][]
  df

}
