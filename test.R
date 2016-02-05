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
rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(data.table::as.data.table(x))
names(x) <- stri_trans_tolower(names(x))
x[, q1 := "Snitt"]
x <- set_association(x, mainentity = "q1")

setnames(x, "q16em", "q16emLOL")
attributes(x)

z <- dplyr::tbl_dt(x)


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


rm(list = ls(all = TRUE))
x <- read_data("test.sav")
x <- survey(x)

z <- read_data("test.sav")
z <- data.table::as.data.table(z)
z <- survey(z)

identical(as.data.frame(z), as.data.frame(x))

z <- from_labelled(x)
