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
      self$data <- x
    },

    update = function() {
      "Update attributes after manipulating the object."
      print("Updating survey.")
    },

    update_data = function(value) {
      "Update data."
      self$data <- value
    },

    get_attribute = function(name) {
      "Retrieve hidden/private attributes in a survey object."
      `$`(private, name)
    },

    set_attribute = function(name, value) {
      "Set hidden/private attributes in a survey object."
      `$<-`("private", name, value)
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
  x$data[...]
  x$update()
}

#' @export
`[[.Survey` <- function(x, ...) {
  x$data[[...]]
}

#' @export
`[<-.Survey` <- function(x, ...) {
  x$update_data(`[<-`(x$data, ...))
}

#' @export
`[[<-.Survey` <- function(x, ...) {
  x$update_data(`[[<-`(x$data, ...))
}


