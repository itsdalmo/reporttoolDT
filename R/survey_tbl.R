#' @rdname survey
#' @export
survey_tbl <- function(x) {
  if (is.survey(x)) {
    x$as_tbl()
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
    initialize = function(x, fields = NULL) {
      if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("dplyr package required to use tbl's.")
      }
      if (!is_tbl(x))
        x <- dplyr::as.tbl(x)
      super$initialize(x, fields)
    },
    as_df = function(...) {
      Survey_df$new(self$get_data(), fields = private$all_fields())
    },
    as_dt = function(...) {
      Survey_dt$new(self$get_data(), fields = private$all_fields())
    },
    as_tbl = function(clone = FALSE) {
      if (clone) {
        self$clone(deep = FALSE)
      } else {
        self
      }
    },
    names = function() {
      data.table::copy(names(self$data))
    }

  )
)
