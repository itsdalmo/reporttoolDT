library(devtools)
library(testthat)
library(stringi)
load_all()


# DT -------------
rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(data.table::as.data.table(x))
names(x) <- stri_trans_tolower(names(x))
x[, q1 := "Snitt"]
x <- set_association(x, mainentity = "q1")

df <- qtable_(x, c("epsi", "loyal"))
df <- qtable_(x, c("q17"))


# DF -------------
rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)
names(x) <- stri_trans_tolower(names(x))
x$q1 <- "Snitt"
x <- set_association(x, mainentity = "q1")

df <- qtable_(x, c("epsi", "loyal"))
df <- qtable_(x, c("q17"))


# DT -------------
rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(data.table::as.data.table(x))
names(x) <- stri_trans_tolower(names(x))
x[, q1 := "Snitt"]
x <- set_association(x, mainentity = "q1")

df <- qtable_(x, c("epsi", "loyal"))
df <- qtable_(x, c("q17"))



df <- qtable_(x, c("epsi", "loyal"), groups = "q1")
df <- qtable_(x, c("epsi", "loyal"), groups = c("q1", "q17"))
df <- qtable_(x, c("q17"), groups = "q1")


