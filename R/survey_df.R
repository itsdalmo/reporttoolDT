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
    },

    do = function(f, dots, renamed = NULL, assign = FALSE) {
      "Perform operations directly on the data.frame."
      res <- do.call(f, c(list(self$data), dots))

      if (assign) {
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
    }

  )
)
