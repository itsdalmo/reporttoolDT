library(devtools)
library(testthat)
library(stringi)
load_all()

rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)
names(x) <- stri_trans_tolower(names(x))
x[, q1 := "Snitt"]
x <- set_association(x, mainentity = "q1")

df <- qtable_(x, c("epsi", "loyal"))
df <- qtable_(x, c("q17"))
