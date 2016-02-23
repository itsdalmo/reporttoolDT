#' @importFrom R6 R6Class
#' @export
Survey <- R6::R6Class("Survey",
  private = list(
    .associations = NULL,
    .labels = NULL,
    .config = NULL,
    .dictionary = NULL,
    .marketshares = NULL
  ),

  public = list(

    data = NULL,

    initialize = function(x) {
      if (missing(x) || !is.data.frame(x))
        stop("Expecting a data.frame or data.table.", call = FALSE)
      if (is.labelled(x)) {
        x <- from_labelled(x, copy = FALSE)
        private$.labels <- attr(x, "labels")
      }
      self$data <- x
    },

    do = function(f, dots, assign = FALSE) {
      "Do operations directly on the data."
      if (assign) {
        self$data <- do.call(f, c(list(self$data), dots))
        self
      } else {
        # self$subset(do.call(f, c(list(self$data), dots)))
        do.call(f, c(list(self$data), dots))
      }
    },

    subset = function(x) {
      new <- self$clone(deep = TRUE)
      new$data <- x
      new
    },

    update = function() {
      "Update attributes after manipulating the object."
      print("Updating survey.")
    },

    get_labels = function() {
      private$.labels
    },

    print = function(...) {
      print(self$data)
    }
  )
)

# as/is ------------------------------------------------------------------------
#' @export
survey <- function(x) UseMethod("survey")

#' @export
is.survey <- function(x) inherits(x, "Survey")

#' @export
as.survey <- function(x) UseMethod("as.survey")

#' @export
as.survey.Survey <- function(x) x

#' @export
as.survey.default <- function(x) survey(x)

# Names ------------------------------------------------------------------------
#' @export
names.Survey <- function(x) names(x$data)

#' @export
dimnames.Survey <- function(x) {
  dimnames(x$data)
}

# Subset/alter -----------------------------------------------------------------
#' @export
`[.Survey` <- function(x, ...) {
  x$do("[", capture_dots(...))
}

#' @export
`[[.Survey` <- function(x, ...) {
  x$do("[[", capture_dots(...))
}

#' @export
`[<-.Survey` <- function(x, ...) {
  x$do("[<-", capture_dots(...), assign = TRUE)
}

#' @export
`[[<-.Survey` <- function(x, ...) {
  x$do("[[<-", capture_dots(...), assign = TRUE)
}
