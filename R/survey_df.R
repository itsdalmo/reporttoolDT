#' @export
survey_df <- function(x) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }

  structure(new_survey(x), class = c("survey_df", "survey"))
}

#' @export
survey.data.frame <- function(x) {
  if (is.labelled(x)) x <- from_labelled(x)
  survey_df(x)
}
