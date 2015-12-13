library(devtools)
library(testthat)
library(stringi)
load_all()

rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)
names(x) <- stri_trans_tolower(names(x))

z <- x[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c("q3em")]
z <- x[, q3em]

z <- x[, mean(q3 + 10)]
