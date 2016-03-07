#' @rdname survey
#' @export
survey_dt <- function(x) {
  if (inherits(x, "Survey_dt")) {
    x
  } else {
    Survey_dt$new(x)
  }
}

#' @export
survey.data.table <- function(x) {
  survey_dt(x)
}

# Survey (R6 Class) ------------------------------------------------------------
Survey_dt <- R6::R6Class("Survey_dt",
  inherit = Survey,
  private = list(
    deep_clone = function(name, value) {
      if (name == "data") {
        data.table::copy(value)
      } else {
        value
      }
    }
  ),

  public = list(
    initialize = function(x) {
      if (data.table::is.data.table(x)) {
        x <- data.table::copy(x)
      } else {
        x <- data.table::as.data.table(x)
      }
      super$initialize(x)
    },

    do = function(f, dots, renamed = NULL, assign = FALSE) {
      "Perform operations directly on the data.table."
      res <- do.call(f, c(list(self$data), dots))

      if (identical(data.table::address(res), data.table::address(self$data))) {
        self$update(renamed)
        self
      } else if (assign) {
        self$data <- res
        self$update(renamed)
        self
      } else {
        if (is.data.frame(res)) {
          self$initialize_subset(res)$update(renamed)
        } else {
          res
        }
      }
    },

    names = function() {
      data.table::copy(names(self$data))
    }

  )
)
