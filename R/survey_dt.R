#' @importFrom R6 R6Class
#' @export
Survey_dt <- R6::R6Class("Survey_dt",
  inherit = Survey,
  public = list(
    initialize = function(x) {
      if (!requireNamespace("data.table")) {
        stop("data.table package required to use data tables", call. = FALSE)
      }
      if (data.table::is.data.table(x)) {
        x <- data.table::copy(x)
      } else {
        x <- data.table::as.data.table(x)
      }
      super$initialize(x)
    }
  )
)

survey_dt <- function(x) {
  if (inherits(x, "Survey_dt")) {
    x
  } else {
    Survey_dt$new(x)
  }
}

#' @export
survey.data.table <- function(x) {
  if (is.labelled(x)) x <- from_labelled(x)
  survey_dt(x)
}
