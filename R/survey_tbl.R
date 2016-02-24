#' @importFrom R6 R6Class
#' @export
Survey_tbl <- R6::R6Class("Survey_tbl",
  inherit = Survey,
  public = list(
    initialize = function(x) {
      if (!requireNamespace("dplyr")) {
        stop("dplyr package required to use tbl's.", call. = FALSE)
      }
      super$initialize(dplyr::as.tbl(x))
    }

  )
)

#' @export
survey.tbl <- function(x) {
  survey_tbl(x)
}

survey_tbl <- function(x) {
  if (inherits(x, "Survey_tbl")) {
    x
  } else {
    Survey_tbl$new(x)
  }
}
