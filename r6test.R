rm(list = ls(all = TRUE))
x <- reporttool::read_data("./test.sav")
# x <- reporttool::from_labelled(x)$df

# R6 -------------
#' @importFrom R6 R6Class
Survey <- R6::R6Class("Survey",
  private = list(
    .data = NULL,
    .associations = NULL,
    .labels = NULL,
    .config = NULL,
    .dictionary = NULL,
    .marketshares = NULL
  ),
  public = list(

    initialize = function(df) {
      if (missing(df) || !is.data.frame(df))
        stop("Expecting a data.frame or data.table.", call = FALSE)
      private$.data <- df
    },

    update = function() {

      nms <- self$names()
      for (i in names(private)) {

      }
      private$.labels <-
    },

    data = function() {
      private$.data
    },

    associations = function(which = NULL) {
      res <- private$.associations
      if (!is.null(which) && !is.null(res)) {
        res <- res[match_all(which, res)]
      }
      res
    },

    labels = function(which = NULL) {
      res <- private$.labels
      if (!is.null(which) && !is.null(res)) {
        res <- res[match_all(which, res)]
      }
      res
    },

    config = function(which = NULL) {
      res <- private$.config
      if (!is.null(which) && !is.null(res)) {
        res <- res[match_all(which, names(res))]
      }
      res
    },

    dictionary = function(which = NULL) {
      res <- private$.dictionary
      if (!is.null(which) && !is.null(res)) {
        res <- res[match_all(which, names(res))]
      }
      res
    },

    bind_rows = function(...) {
      data <- c(list(private$.data), list(...))
      data <- lapply(data, function(x) { if (is.survey(x)) x$data() else x })
      private$.data <- do.call("rbind", data)
    },

    bind_cols = function(...) {
      data <- c(list(private$.data), list(...))
      data <- lapply(data, function(x) { if (is.survey(x)) x$data() else x })
      private$.data <- do.call("cbind", data)
    },

    names = function(...) {
      "Return the name of columns in the data."
      names(private$.data)
    },

    bracket = function(...) {
      `[`(private$.data, ...)
    },

    dbracket = function(...) {
      `[[`(private$.data, ...)
    },

    bracket_repl = function(i, j, value) {
      `[<-`(private$.data, i, j, value)
    },

    dbracket_repl = function(i, j, value) {
      `[[<-`(private$.data, i, j, value)
    },

    print = function(...) {
      print(private$.data)
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
  x <- x$clone(deep = FALSE)
  x$bind_rows(...)
}

cbind.Survey <- function(x, ...) {
  x <- x$clone(deep = FALSE)
  x$bind_cols(...)
}

names.Survey <- function(x) x$names()

y <- data.table::as.data.table(x)
df <- survey_df(x)
dt <- survey_dt(x)

test <- rbind(dt, df, df); class(test); nrow(test)
test <- cbind(dt, df, df); class(test); ncol(test)





# BENCHMARKS
library(microbenchmark)

srv_dt_c <- function() cbind(dt, dt, dt)
srv_df_c <- function() cbind(df, df, df)
dt_c <- function() cbind(y, y, y)
df_c <- function() cbind(x, x, x)

microbenchmark(srv_dt_c(), srv_df_c(), dt_c(), df_c())

srv_dt_r <- function() rbind(dt, dt, dt)
srv_df_r <- function() rbind(df, df, df)
dt_r <- function() rbind(y, y, y)
df_r <- function() rbind(x, x, x)

microbenchmark(srv_dt_r(), srv_df_r(), dt_r(), df_r())
