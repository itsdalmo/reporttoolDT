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
                          # if (missing(df) || !is.data.frame(df))
                            # stop("Expecting a data.frame or data.table.", call. = FALSE)
                          assign(".data", df, envir = self)
                        },

                        names = function(...) {
                          "Return the name of columns in the data."
                          names(get(".data", envir = self))
                        },

                        bracket = function(...) {
                          `[`(get(".data", envir = self), ...)
                        },

                        dbracket = function(...) {
                          `[[`(get(".data", envir = self), ...)
                        },

#                         dollar = function(name) {
#                           `$`(get(".data", envir = self), name)
#                         },

                        bracket_repl = function(i, j, value) {
                          `[<-`(get(".data", envir = self), i, j, value)
                        },

                        dbracket_repl = function(i, j, value) {
                          `[[<-`(get(".data", envir = self), i, j, value)
                        },

                        print = function(...) {
                          print(get(".data", envir = self))
                        }
                      )
)

survey <- function(x) {
  structure(Survey$new(x), class = c("survey_dt", "Survey", "R6"))
}

`[.Survey` <- function(x, ...) {
  x$bracket(...)
}

`[[.Survey` <- function(x, ...) {
  x$dbracket(...)
}

`$.Survey` <- function(x, name, ...) {
  if (name %in% ls(x) || substr(name, 0, 1) == "." || name %in% ls(Survey)) {
    f <- get(name, envir = x)
    if (is.function(f)) {
      f(...)
      # do.call(f, list(...))
    } else {
      f
    }
  } else {
    `$`(get(name, envir = x), name, ...)
  }
}

`[<-.Survey` <- function(x, i, j, value) {
  x$bracket_repl(i, j, value)
}

`[[<-.Survey` <- function(x, i, j, value) {
  x$dbracket_repl(i, j, value)
}

names.Survey <- function(x) x$names()

y <- survey(x)
y <- survey(data.table::as.data.table(x))
y <- Survey$new(x)
