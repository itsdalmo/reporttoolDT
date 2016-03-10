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
#' @param ... Additional parameters passed to \code{melt}.
#' @inheritParams data.table::melt
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # Create a new survey
#' x <- survey_dt(data.frame("A" = "A", "B" = 2, "C" = 3))
#' x <- melt(x, "A", c("B", "C"))
#' x

melt <- function(data, ...) UseMethod("melt")

#' @rdname melt
#' @export
melt.default <- data.table::melt

#' @rdname melt
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
#' @param ... Additional parameters passed to \code{dcast}.
#' @inheritParams data.table::dcast
#' @author Kristian D. Olsen
#' @export
#' @examples
#' # Create a new survey
#' x <- survey_dt(data.frame("A" = "A", "B" = 2, "C" = 3))
#' x <- melt(x, "A", c("B", "C"))
#' dcast(x, A ~ variable)

dcast <- function(data, ...) UseMethod("dcast")

#' @rdname dcast
#' @export
dcast.default <- data.table::dcast

#' @rdname dcast
#' @export
dcast.Survey <- function(data, ...) {
  f <- get("dcast", asNamespace("data.table"))
  data$do(f, capture_dots(...))
}

#' @export
.datatable.aware <- TRUE
