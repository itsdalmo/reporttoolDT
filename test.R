library(devtools)
library(testthat)
library(stringi)

rm(list = ls(all = TRUE))
x <- reporttool::read_data("./test.sav")

# R6 -------------

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

survey <- function(df) Survey$new(df)

Survey_dt <- R6::R6Class("Survey_dt",
                         inherit = Survey,
                         public = list(
                           initialize = function(df) {
                             self$.data <- data.table::as.data.table(df)
                           }
                         ))

Survey_df <- R6::R6Class("Survey_df", inherit = Survey)

`[.Survey` <- function(x, ...) {
  x$bracket(...)
}

`[[.Survey` <- function(x, ...) {
  x$dbracket(...)
}

# `$.Survey_dt` <- function(x, name) {
#   if (is.character(name) && substr(name, 0, 1) == ".") {
#     # get(name, envir = x)
#     NextMethod()
#   } else {
#     `[[`(x$.data, name)
#   }
# }

`[<-.Survey` <- function(x, i, j, value) {
  x$bracket_repl(i, j, value)
}

`[[<-.Survey` <- function(x, i, j, value) {
  x$dbracket_repl(i, j, value)
}

# TEST

y <- survey(x)
y <- survey(data.table::as.data.table(x))

y <- Survey_dt$new(df)
