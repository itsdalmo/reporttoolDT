library(devtools)
library(testthat)
library(stringi)

rm(list = ls(all = TRUE))
x <- reporttool::read_data("./test.sav")

y <- survey_df(x)
y <- survey_dt(x)
