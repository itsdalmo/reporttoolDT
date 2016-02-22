#' @export
survey_dt <- function(x, copy = FALSE) {
  if (!requireNamespace("data.table")) {
    stop("data.table package required to use data tables", call. = FALSE)
  }
  if (data.table::is.data.table(x)) {
    if (copy)
      x <- data.table::copy(x)
  } else {
    x <- data.table::as.data.table(x)
  }

  structure(new_survey(x), class = c("survey_dt", "survey"))
}

#' @export
survey.data.table <- function(x) {
  if (is.labelled(x)) x <- from_labelled(x)
  survey_dt(x, copy = TRUE)
}
