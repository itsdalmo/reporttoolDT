library(devtools)
library(testthat)
library(stringi)

# DF -------------
rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)
names(x) <- stri_trans_tolower(names(x))
x$q1 <- "Snitt"
x <- set_association(x, mainentity = "q1")

df <- qtable(x, c("epsi", "loyal"))
df <- qtable(x, c("q17"))


load_all()
# DT -------------
rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(data.table::as.data.table(x))
names(x) <- stri_trans_tolower(names(x))
x[, q1 := "Snitt"]
x <- set_association(x, mainentity = "q1")

df <- qtable(x, c("epsi", "loyal"))
df <- qtable(x, c("q17"))
df <- qtable(x, c("epsi", "loyal"), groups = "q1")
df <- qtable(x, c("epsi", "loyal"), groups = c("q1", "q17"))
df <- qtable(x, c("q17"), groups = "q1")
df <- qtable(x, "epsi", groups = c("q1", "q17"))
df <- qtable(x, c("epsi", "loyal"), groups = c("q1", "q17"))

load_all()
df <- qtable(x, c("q7pa", "q7pb"), groups = "q1")

df <- data.table::copy(x)
df[, startdate := as.Date(startdate, format = "%m/%d/%Y %H:%M:%S")]
df <- qtable(df, "startdate", groups = "q1")
