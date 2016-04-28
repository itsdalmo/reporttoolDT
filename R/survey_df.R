#' @rdname survey
#' @export
survey_df <- function(x) {
  if (is.survey(x)) {
    x$as_df()
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
    initialize = function(x, fields = NULL) {
      if (!is.data.frame(x) || data.table::is.data.table(x) || is_tbl(x))
        x <- as.data.frame(x)
      super$initialize(x, fields)
    },
    as_df = function(clone = FALSE) {
      if (clone) {
        self$clone(deep = FALSE)
      } else {
        self
      }
    },
    as_dt = function(...) {
      Survey_dt$new(self$get_data(), fields = self$get_field())
    },
    as_tbl = function(...) {
      Survey_tbl$new(self$get_data(), fields = self$get_field())
    }
  )
)

# S3 methods -------------------------------------------------------------------

#' Survey: rbind
#'
#' \code{\link[base]{rbind}} method for \code{Survey} objects. Labels and associations
#' are also merged if you are binding other surveys.
#'
#' @param x A \code{Survey}.
#' @param ... Additional parameters passed to \code{rbind}.
#' @inheritParams base::rbind
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

rbind.Survey <- function(x, ...) {
  x$do_merge("rbind", list(...))
}

#' Survey: cbind
#'
#' \code{\link[base]{cbind}} method for \code{Survey} objects. Labels and associations
#' are also merged if you are binding other surveys.
#'
#' @param x A \code{Survey}.
#' @param ... Additional parameters passed to \code{cbind}.
#' @inheritParams base::rbind
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

cbind.Survey <- function(x, ...) {
  x$do_merge("cbind", list(...))
}