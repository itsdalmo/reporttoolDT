library(devtools)
library(testthat)
library(stringi)
load_all()

rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)


x <- set_association(x, mainentity = "q1")
x[, percent_missing := Andel_Missing]
x <- set_config(x, cutoff = .3)
get_attributes(x, default$attributes)
entities(x)

x <- set_marketshare(x, "reporttool" = 1)
entities(x)


model(x)
z <- as.list(x, attr = TRUE)
