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


test <- survey(x)
test1 <- test
test2 <- test[, "EPSI"]
test3 <- dplyr::mutate(test, test = "lol")

# TODO
# 1. [, "Q1", drop = FALSE] should not only return a new survey, but also retain
# associations, labels etc. Implement: "constructor" for slices.
# 2. Need methods to set labels and associations. Simple lists where var = value will work.
# 3. Update marketshares? Can/should this be done without too much overhead?
# 4. Survey_tbl - should be easy to do. Might need to use the "address" thing in "do" here also (in case it is DT with copy = FALSE)
# 5. Add dplyr and tidyr methods for Survey objects.

# Take a data.frame, load it into R.
# Create a survey if wanted.
# Labels can be set automatically.
# Associations, config, translations have to be set manually. Associations should support common = TRUE.
# set associations etc should include a COPY = TRUE option?
#