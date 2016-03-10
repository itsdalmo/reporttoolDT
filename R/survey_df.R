#' @rdname survey
#' @export
survey_df <- function(x) {
  if (inherits(x, "Survey_df")) {
    x
  } else {
    Survey_df$new(x)
  }
}

#' @export
survey.data.frame <- function(x) {
  survey_df(x)
}

# Survey (R6 Class) ------------------------------------------------------------
Survey_df <- R6::R6Class("Survey_df",
  inherit = Survey,
  public = list(
    initialize = function(x) {
      super$initialize(as.data.frame(x))
    }
  )
)
