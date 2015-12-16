qtable <- function(srv, ..., wide = TRUE, weight = TRUE, question = TRUE) {
  dots <- lazyeval::lazy_dots(...)
  qtable_(srv, dots)
}

qtable_ <- function(srv, dots, groups = NULL) {

  if(!length(dots)) stop("No variables specified.", call. = FALSE)
  srv <- data.table::copy(srv)

  # Check the input
  if (!is.survey(srv)) {
    stop("Argument 'survey' is not an object with the class 'survey'. See help(survey).", call. = FALSE)
  }

  # Remove observations above cutoff (if it is not NA)
  cutoff <- as.numeric(get_config(srv, "cutoff"))
  if (!is.na(cutoff) && "percent_missing" %in% names(srv)) {
    srv <- srv[cutoff <= percent_missing, with = FALSE]
  } else {
    warning("Either cutoff is not set or 'percent_missing' is not in the data.", call. = FALSE)
  }

  # Aggregate
  if (length(groups)) {
    srv <- melt(srv, id = groups, measure = dots)
    srv <- srv[!is.na(variable) & !is.na(value), ]
  } else {

  }

  srv[, list(n = .N, value = mean(value, na.rm = TRUE)), keyby = c(groups, "variable")]
  # srv[, lapply(.SD, mean), .SDcols = dots, keyby = mainentity]

}