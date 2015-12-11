library(devtools)
library(testthat)
load_all()

rm(list = ls(all = TRUE))
x <- read_data("test.sav")
y <- survey(x)

y[, percent_missing := Andel_Missing]

y <- set_association(y, mainentity = "q1")
y <- set_config(y, cutoff = .3)
