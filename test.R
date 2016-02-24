library(devtools)
library(testthat)
library(stringi)

rm(list = ls(all = TRUE))
devtools::load_all()
x <- reporttool::read_data("./test.sav")

test <- survey_dt(data.table::as.data.table(x))
test1 <- test
test2 <- test[, .(EPSI)]
test3 <- test[, test := "lol"]

data.table::address(test1$data) # 1 and 3 are identical. I.e., updated by reference.
data.table::address(test2$data)
data.table::address(test3$data)

data.table::address(test1) # 1 and 3 are identical. I.e., updated by reference.
data.table::address(test2)
data.table::address(test3)


test <- survey_df(x)
test1 <- test
test2 <- test[, "EPSI", drop = FALSE]

data.table::address(test1)
data.table::address(test2)
