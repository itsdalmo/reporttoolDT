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
                        data = NULL,

                        initialize = function(df) {
                          if (missing(df) || !is.data.frame(df))
                            stop("Expecting a data.frame or data.table.", call = FALSE)
                          # if (contains_labelled(df))
                            # df <- convert_labelled(df)
                          self$data <- df
                          private$.labels <- attr(df, "labels")
                        },

                        bracket = function(...) {
                          `[`(self$data, ...)
                        },

                        dbracket = function(...) {
                          `[[`(self$data, ...)
                        },

#                         dollar = function(name) {
#                           `$`(self$data, name)
#                         },
#
                        bracket_repl = function(i, j, value) {
                          `[<-`(self$data, i, j, value)
                        },

                        dbracket_repl = function(i, j, value) {
                          `[[<-`(self$data, i, j, value)
                        },
#
#                         dollar_repl = function(name, value) {
#                           `$<-`(self$data, name, value)
#                         },
#
                        print = function(...) {
                          print(self$data)
                        }
                      )
)


survey <- function(df) Survey$new(df)


`[.Survey` <- function(x, ...) {
  x$bracket(...)
}

`[[.Survey` <- function(x, ...) {
  x$dbracket(...)
}

# `$.Survey` <- function(x, name) {
#   if (name != "data") {
#     x$data[[name]]
#   } else {
#     x$dollar(name)
#   }
# }

`[<-.Survey` <- function(x, i, j, value) {
  x$bracket_repl(i, j, value)
}

`[[<-.Survey` <- function(x, i, j, value) {
  x$dbracket_repl(i, j, value)
}

# `$<-.Survey` <- function(x, name, value) {
#   x$dollar_repl(name, value)
# }

y <- survey(x)
y <- survey(data.table::as.data.table(x))
