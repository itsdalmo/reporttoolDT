library(devtools)
library(testthat)
library(stringi)

rm(list = ls(all = TRUE))
devtools::load_all()
x <- reporttool::read_data("./test.sav")

# TODO
# 1. [, "Q1", drop = FALSE] should not only return a new survey, but also retain
# associations, labels etc. Implement: "constructor" for slices.

# dt <- survey_dt(x)
# dt2 <- dt[, .(q1, EPSI, Loyal)]
# dt2$model()

# 2. Need methods to set labels and associations. Simple lists where var = value will work.

dt <- survey_dt(x)
dt$set_association(new = c(q1 = "mainentity"))
dt$set_marketshare(new = c(reporttool = 1L))
dt$entities()

dt <- survey_dt(x)
dt$set_association(new = c("q1" = "mainentity"))
dt$model()
dt$get_associations("mainentity")

dt$set_label(new = c("q1" = "test"))
dt$get_labels("q1")
dt$model()


# 5. Add dplyr and tidyr methods for Survey objects.

dt <- survey_tbl(x)
dt2 <- dplyr::select(dt, q1, EPSI:w)
class(dt2);class(dt2$data)
dt <- dt$set_association(c(q1 = "mainentity"))
dt3 <- dplyr::select(dplyr::filter(dt, EPSI > 60), q1, one_of("EPSI","Loyal"))
class(dt3);class(dt3$data)
dt3$model();dt3$entities()


tt <- tidyr::gather(dplyr::select(x, q1, one_of("EPSI", "Loyal")), var, score, -q1)
tidyr::spread(tt, q1, score)

dt4 <- tidyr::gather(dplyr::mutate(dt3, row = 1:n()), var, score, -q1, -row)
tidyr::spread(dt4, var, score)
tidyr::spread(dt4$data, var, score)


# Take a data.frame, load it into R.
# Create a survey if wanted.
# Labels can be set automatically.
# Associations, config, translations have to be set manually. Associations should support common = TRUE.
# set associations etc should include a COPY = TRUE option?
#