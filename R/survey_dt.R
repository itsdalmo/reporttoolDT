#' @importFrom R6 R6Class
#' @export
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

    do = function(f, dots, assign = FALSE) {
      "Perform operations directly on the data.table."
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
    },

    names = function() {
      data.table::copy(names(self$data))
    },

    set_names = function(nms) {
      "Set colnames in the data."
      if (!length(nms) == length(self$data))
        stop("set_names: New names must be of same length as the data.", call. = FALSE)
      data.table::setnames(self$data, nms)
      data.table::setattr(private$.associations, "names", nms)
      data.table::setattr(private$.labels, "names", nms)
      invisible(self)
    }

  )
)

#' @export
survey.data.table <- function(x) {
  survey_dt(x)
}

survey_dt <- function(x) {
  if (inherits(x, "Survey_dt")) {
    x
  } else {
    Survey_dt$new(x)
  }
}

