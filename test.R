library(devtools)
library(testthat)
library(stringi)

load_all()

# DF -------------
rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)
names(x) <- stri_trans_tolower(names(x))
x$q1 <- "Snitt"
x <- set_association(x, mainentity = "q1")

df <- qtable(x, c("epsi", "loyal"))
df <- qtable(x, c("q17"))


# DT -------------
load_all()
rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(data.table::as.data.table(x))
names(x) <- stri_trans_tolower(names(x))
x[, q1 := "Snitt"]
x <- set_association(x, mainentity = "q1")

df <- qtable2(x, "epsi", groups = c("q1", "q17"))
df <- qtable2(x, c("loyal", "epsi"))

df <- qtable2(x, "q17", "q1")
