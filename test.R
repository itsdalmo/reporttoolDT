library(devtools)
library(testthat)
library(stringi)

rm(list = ls(all = TRUE))
devtools::load_all()
x <- reporttool::read_data("./test.sav")

# TODO
# 1. [, "Q1", drop = FALSE] should not only return a new survey, but also retain
# associations, labels etc. Implement: "constructor" for slices.

dt <- survey_dt(x)
dt2 <- dt[, .(q1, EPSI, Loyal)]
dt2$model()

# 2. Need methods to set labels and associations. Simple lists where var = value will work.

dt <- survey_dt(x)
dt$set_association(new = c("q1" = "mainentity"))
dt$model()
dt$get_association("mainentity")

dt$set_label(new = c("q1" = "test"))
dt$get_labels("q1")
dt$model()


# 3. Update marketshares? Can/should this be done without too much overhead?
# 4. Survey_tbl - should be easy to do. Might need to use the "address" thing in "do" here also (in case it is DT with copy = FALSE)
# 5. Add dplyr and tidyr methods for Survey objects.

# Take a data.frame, load it into R.
# Create a survey if wanted.
# Labels can be set automatically.
# Associations, config, translations have to be set manually. Associations should support common = TRUE.
# set associations etc should include a COPY = TRUE option?
#