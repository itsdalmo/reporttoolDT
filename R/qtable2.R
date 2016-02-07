qtable2 <- function(df, ...) UseMethod("qtable2")

qtable2.survey <- function(df, vars, groups = NULL, wide = TRUE) {

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

qtable2.data.frame <- function(df, vars, groups = NULL, wide = TRUE) {
  df <- data.table::as.data.table(df)
  as.data.frame(qtable_impl(df, vars, groups, wide))
}

qtable2.data.table <- function(df, vars, groups = NULL, wide = TRUE) {
  df <- data.table::copy(df)
  qtable_impl(df, vars, groups, wide)
}

qtable_impl <- function(df, vars, groups = NULL, wide = TRUE) {
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

  # Gather each variable and expose implicit missing values.
  df <- data.table::melt(df, groups, vars, value.factor = isTRUE(type == "factor"))
  data.table::setkeyv(df, groups)
  # df[CJ(df[, groups, with = FALSE])]

  if (type == "numeric") {
    df[, n := .N, by = c(groups, "variable")]
    frm <- stri_c(stri_c(groups, collapse = "+"), "+", "n", "~", "variable")
    fun <- quote(mean)
  } else if (type == "factor") {
    setnames(df, "value", "level")
    df <- df[, .(n = .N), keyby = c(groups, "level")][, value := prop.table(n)]
    frm <- stri_c(stri_c(groups, collapse = "+"), "+", "n", "~", "level")
    fun <- quote(NULL)
  } else if (type == "Date") {
    frm <- stri_c(stri_c(groups, collapse = "+"), "+", "n", "~", "variable")
    fun <- list(quote(min), quote(max))
  }

  df <- data.table::dcast(df, formula = frm, fun.aggregate = eval(fun))
  df[]

}

complete_df <- function(df, vars) {
  cj <- df[, vars, with = FALSE]
  cj <- lapply(cj, function(x) { if (is.factor(x)) levels(x) else unique(x) })
  cj <- expand.grid(cj)

  data.table::setkeyv(df, vars)
  df[cj]
}
