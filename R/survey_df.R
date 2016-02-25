#' @importFrom R6 R6Class
#' @export
Survey_df <- R6::R6Class("Survey_df",
  inherit = Survey,
  public = list(
    initialize = function(x) {
      super$initialize(as.data.frame(x))
    },

    do = function(f, dots, assign = FALSE) {
      "Perform operations directly on the data.frame."
      res <- do.call(f, c(list(self$data), dots))

      if (assign) {
        self$data <- res
        super$update()
        self
      } else {
        if (is.data.frame(res)) {
          super$initialize_subset(res)
        } else {
          res
        }
      }
    }

  )
)

#' @export
survey.data.frame <- function(x) {
  survey_df(x)
}

survey_df <- function(x) {
  if (inherits(x, "Survey_df")) {
    x
  } else {
    Survey_df$new(x)
  }
}