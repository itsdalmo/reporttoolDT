library(devtools)
library(testthat)
library(stringi)
load_all()

rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)
names(x) <- stri_trans_tolower(names(x))
x[, q1 := "Snitt"]


z <- rbind(x, copy(x)[, q1 := "Snitt"])
