context("melt/gather and dcast/spread for Survey")

org <- data.frame(Q1 = c("Example 1", "Example 2"),
                  Score1 = c(9, 8),
                  Score2 = c(80, 70),
                  stringsAsFactors = FALSE)

dummy_survey <- function(x) {
  x$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
  x
}

# Gather -----------------------------------------------------------------------
test_that("gather works with Survey_df", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  df <- dummy_survey(survey_df(org))
  df <- tidyr::gather(df, var, score, -Q1)

  expect_s3_class(df, "Survey_df")
  expect_identical(names(df), c("Q1", "var", "score"))
  expect_identical(df$data$score, c(9, 8, 80, 70))

})

test_that("gather works with Survey_dt", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  # tidyr does not have data.table methods. Returns a data.frame.
  dt <- dummy_survey(survey_dt(org))
  expect_warning(
    dt <- tidyr::gather(dt, var, score, -Q1),
    "Class has changed"
  )


  expect_s3_class(dt, "Survey_df")
  expect_identical(names(dt), c("Q1", "var", "score"))
  expect_identical(dt$data$score, c(9, 8, 80, 70))

})

test_that("gather works with Survey_tbl", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- tidyr::gather(tbl, var, score, -Q1)

  expect_s3_class(tbl, "Survey_tbl")
  expect_identical(names(tbl), c("Q1", "var", "score"))
  expect_identical(tbl$data$score, c(9, 8, 80, 70))

})

# Spread -----------------------------------------------------------------------
test_that("spread works with Survey_df", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  df <- dummy_survey(survey_df(org))
  df <- tidyr::gather(df, var, score, -Q1)
  df <- tidyr::spread(df, var, score)

  expect_s3_class(df, "Survey_df")
  expect_identical(names(df), c("Q1", "Score1", "Score2"))
  expect_equal(df$data, org)

})

test_that("spread works with Survey_dt", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  # tidyr does not have data.table methods. Returns a data.frame.
  dt <- dummy_survey(survey_dt(org))
  expect_warning(
    dt <- tidyr::gather(dt, var, score, -Q1),
    "Class has changed"
  )

  dt <- tidyr::spread(dt, var, score)

  expect_s3_class(dt, "Survey_df")
  expect_identical(names(dt), c("Q1", "Score1", "Score2"))
  expect_equal(dt$data, org)

})

test_that("spread works with Survey_tbl", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- tidyr::gather(tbl, var, score, -Q1)
  tbl <- tidyr::spread(tbl, var, score)

  expect_s3_class(tbl, "Survey_tbl")
  expect_identical(names(tbl), c("Q1", "Score1", "Score2"))
  expect_equal(tbl$data, org)

})

# melt -------------------------------------------------------------------------
test_that("melt works with Survey_dt", {
  dt <- dummy_survey(survey_dt(org))
  dt <- melt(dt, "Q1", c("Score1", "Score2"))

  expect_s3_class(dt, "Survey_dt")
  expect_identical(names(dt), c("Q1", "variable", "value"))
  expect_identical(dt$data$value, c(9, 8, 80, 70))

})

# dcast ------------------------------------------------------------------------
test_that("dcast works with Survey_dt", {
  dt <- dummy_survey(survey_dt(org))
  dt <- melt(dt, "Q1", c("Score1", "Score2"))
  dt <- dcast(dt, "Q1 ~ variable", value.var = "value")

  expect_s3_class(dt, "Survey_dt")
  expect_identical(names(dt), c("Q1", "Score1", "Score2"))
  expect_equivalent(dt$data, org)

})
