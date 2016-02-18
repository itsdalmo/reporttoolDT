rm(list = ls(all = TRUE))
x <- reporttool::read_data("./test.sav")

# R6 -------------
#' @importFrom R6 R6Class
Survey <- R6::R6Class("Survey",
  private = list(
    .associations = NULL,
    .labels = NULL,
    .config = NULL,
    .dictionary = NULL
  ),
  public = list(
    .data = NULL,

    initialize = function(df) {
      if (missing(df) || !is.data.frame(df))
        stop("Expecting a data.frame or data.table.", call = FALSE)
      self$.data <- df
    },

    bind_rows = function(...) {
      data <- c(list(self$.data), list(...))
      data <- lapply(data, function(x) {
        if (is.survey(x))
          x <- x$.data
        if (data.table::is.data.table(x)) {
          data.table::copy(x)
        } else {
          x
        }
      })
      self$.data <- do.call("rbind", data)
    },

    bind_cols = function(...) {
      data <- c(list(self$.data), list(...))
      data <- lapply(data, function(x) {
        if (is.survey(x))
          x <- x$.data
        if (data.table::is.data.table(x)) {
          data.table::copy(x)
        } else {
          x
        }
      })
      self$.data <- do.call("cbind", data)
    },

    names = function(...) {
      "Return the name of columns in the data."
      names(self$.data)
    },

    bracket = function(...) {
      `[`(self$.data, ...)
    },

    dbracket = function(...) {
      `[[`(self$.data, ...)
    },

    bracket_repl = function(i, j, value) {
      `[<-`(self$.data, i, j, value)
    },

    dbracket_repl = function(i, j, value) {
      `[[<-`(self$.data, i, j, value)
    },

    print = function(...) {
      print(self$.data)
    }
  )
)

is.survey <- function(x) inherits(x, "Survey")

Survey_dt <- R6::R6Class("Survey_dt",
   inherit = Survey,
   public = list(
     initialize = function(x) {
       if (!requireNamespace("data.table")) {
         stop("data.table package required to use data tables", call. = FALSE)
       }
       if (data.table::is.data.table(x)) {
         if (copy)
           x <- data.table::copy(x)
       } else {
         x <- data.table::as.data.table(x)
       }
       super$initialize(x)
     }
  )
)

Survey_df <- R6::R6Class("Survey_df",
   inherit = Survey,
   public = list(
     initialize = function(x) {
       super$initialize(as.data.frame(x))
     }
   )
)

survey_dt <- function(x) Survey_dt$new(x)
survey_df <- function(x) Survey_df$new(x)

`[.Survey` <- function(x, ...) {
  x$bracket(...)
}

`[[.Survey` <- function(x, ...) {
  x$dbracket(...)
}

`[<-.Survey` <- function(x, i, j, value) {
  x$bracket_repl(i, j, value)
}

`[[<-.Survey` <- function(x, i, j, value) {
  x$dbracket_repl(i, j, value)
}

rbind.Survey <- function(x, ...) {
  x$bind_rows(...)
}

cbind.Survey <- function(x, ...) {
  x$bind_cols(...)
}

names.Survey <- function(x) x$names()

df <- survey_df(x)
dt <- survey_dt(x)

test <- rbind(dt, df); class(test); nrow(test)
test <- rbind(df, df, df); class(test); nrow(test)
test <- rbind(dt, df, df); class(test); nrow(test)

test <- cbind(dt, df); class(test); ncol(test)
test <- cbind(df, df, df); class(test); ncol(test)
test <- cbind(dt, df, df); class(test); ncol(test)
