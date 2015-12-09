library(devtools)
library(testthat)
load_all()

rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)

y <- as.list(x)
