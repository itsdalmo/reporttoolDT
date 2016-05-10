#' @rdname survey
#' @export
survey_dt <- function(x) {
  if (is.survey(x)) {
    x$as_dt()
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
    initialize = function(x, fields = NULL) {
      if (data.table::is.data.table(x)) {
        x <- data.table::copy(x)
      } else {
        x <- data.table::as.data.table(x)
      }
      super$initialize(x, fields)
    },
    as_df = function(...) {
      Survey_df$new(self$get_data(), fields = self$get_field())
    },
    as_dt = function(clone = FALSE) {
      if (clone) {
        self$clone(deep = FALSE)
      } else {
        self
      }
    },
    as_tbl = function(...) {
      Survey_tbl$new(self$get_data(), fields = self$get_field())
    },
    names = function() {
      data.table::copy(names(self$data))
    }

  )
)

#' data.table: melt
#'
#' Same as the \code{data.table} function \code{melt}. This is a new generic,
#' because the \code{data.table} version calls \code{reshape2} unless the input
#' is a \code{data.table}.
#'
#' @param data A \code{Survey} or \code{data.frame}.
#' @param ... Additional parameters passed to \code{\link[data.table]{melt}}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # Create a new survey
#' x <- survey_dt(data.frame("A" = "A", "B" = 2, "C" = 3))
#' x <- melt(x, "A", c("B", "C"))
#' x

melt <- function(data, ...) UseMethod("melt")

#' @export
melt.data.frame <- function(data, ...) {
  data.table::melt(data, ...)
}

#' @export
melt.Survey <- function(data, ...) {
  f <- get("melt", asNamespace("data.table"))
  data$do(f, capture_dots(...))
}

#' data.table: dcast
#'
#' Same as the \code{data.table} function \code{dcast}. This is a new generic,
#' because the \code{data.table} version calls \code{reshape2} unless the input
#' is a \code{data.table}.
#'
#' @param data A \code{Survey} or \code{data.frame}.
#' @param ... Additional parameters passed to \code{\link[data.table]{dcast}}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # Create a new survey
#' x <- survey_dt(data.frame("A" = "A", "B" = 2, "C" = 3))
#' x <- melt(x, "A", c("B", "C"))
#' dcast(x, A ~ variable)

dcast <- function(data, ...) UseMethod("dcast")

#' @export
dcast.data.frame <- function(data, ...) {
  data.table::dcast(data, ...)
}

#' @export
dcast.Survey <- function(data, ...) {
  f <- get("dcast", asNamespace("data.table"))
  data$do(f, capture_dots(...))
}

#' @export
.datatable.aware <- TRUE
