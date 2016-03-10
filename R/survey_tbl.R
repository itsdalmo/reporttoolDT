#' @rdname survey
#' @export
survey_tbl <- function(x) {
  if (inherits(x, "Survey_tbl")) {
    x
  } else {
    Survey_tbl$new(x)
  }
}

#' @export
survey.tbl <- function(x) {
  survey_tbl(x)
}

# Survey (R6 Class) ------------------------------------------------------------
Survey_tbl <- R6::R6Class("Survey_tbl",
  inherit = Survey,
  private = list(
    deep_clone = function(name, value) {
      if (name == "data" && data.table::is.data.table(value)) {
        data.table::copy(value)
      } else {
        value
      }
    }
  ),

  public = list(
    initialize = function(x) {
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package required to use tbl's.")
      }
      super$initialize(dplyr::as.tbl(x))
    }

  )
)
