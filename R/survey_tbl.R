#' @importFrom R6 R6Class
#' @export
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
      if (!requireNamespace("dplyr")) {
        stop("dplyr package required to use tbl's.", call. = FALSE)
      }
      super$initialize(dplyr::as.tbl(x))
    },

    do = function(f, dots, assign = FALSE) {
      "Perform operations directly on the tbl."
      res <- do.call(f, c(list(self$data), dots))

      if (identical(data.table::address(res), data.table::address(self$data))) {
        super$update()
        self
      } else if (assign) {
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
