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

    dollar = function(name) {
      `$`(self$.data, name)
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

survey <- function(x) {
  structure(Survey$new(x), class = c("survey_dt", "Survey", "R6"))
}

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

names.Survey <- function(x) x$names()

y <- survey(x)
y <- survey(data.table::as.data.table(x))
y <- Survey$new(x)
