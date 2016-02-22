library(devtools)
library(testthat)
library(stringi)

rm(list = ls(all = TRUE))
devtools::load_all()
x <- reporttool::read_data("./test.sav")

y <- survey_df(x)
y <- as.survey(data.table::as.data.table(x))

is.survey(y)
all(names(y) == names(x))
all(class(y) == c("Survey_dt", "Survey", "R6"))
all(dimnames(y)[[2]] == dimnames(x)[[2]])

y2 <- y

y[["Image"]]
y[, "Image"]
y[, lol := "test"]
y[["lol"]] <- "test2"
y[, "lol"] <- "test2"


is.survey(y[["Image"]])
is.survey(y[, "Image"])
y[, lol := "test"]