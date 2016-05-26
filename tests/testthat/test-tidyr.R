context("Other tidyr verbs for Survey")

org <- data.frame(Q1 = c("Example 1", "Example 2"),
                  Score1 = c(9, 8),
                  Score2 = c(80, 70),
                  stringsAsFactors = FALSE)

dummy_survey <- function(x) {
  x$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
  x
}

# Complete ---------------------------------------------------------------------
test_that("complete works with Survey_df", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  df <- dummy_survey(survey_df(org))
  lvl <- c("Example 1", "Example 2", "Example 3")
  df$data$Q1 <- factor(df$data$Q1, levels = lvl)
  df <- tidyr::complete(df, Q1)

  expect_s3_class(df, "Survey")
  expect_identical(as.character(df$data$Q1), lvl)

})

test_that("complete works with Survey_dt", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  dt <- dummy_survey(survey_dt(org))
  lvl <- c("Example 1", "Example 2", "Example 3")
  dt$data$Q1 <- factor(dt$data$Q1, levels = lvl)
  dt <- tidyr::complete(dt, Q1)

  expect_s3_class(dt, "Survey")
  expect_identical(as.character(dt$data$Q1), lvl)

})

test_that("complete works with Survey_tbl", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  tbl <- dummy_survey(survey_tbl(org))
  lvl <- c("Example 1", "Example 2", "Example 3")
  tbl$data$Q1 <- factor(tbl$data$Q1, levels = lvl)
  tbl <- tidyr::complete(tbl, Q1)

  expect_s3_class(tbl, "Survey")
  expect_identical(as.character(tbl$data$Q1), lvl)

})

test_that("expand works with Survey_tbl", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  df <- data.frame(
    Q1 = rep("Example", 7L),
    year   = c(2010, 2010, 2010, 2010, 2012, 2012, 2012),
    qtr    = c(   1,    2,    3,    4,    1,    2,    3),
    return = rnorm(7)
  )

  tbl <- dummy_survey(survey_tbl(df))
  tbl <- tidyr::expand(tbl, Q1, year, qtr)

  expect_s3_class(tbl, "Survey")
  expect_identical(get_association(tbl, "mainentity"), setNames("Q1", "mainentity"))

})

test_that("fill works with Survey_tbl", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  df <- data.frame(Q1 = rep("Example", 12L), Month = 1:12, Year = c(2000, rep(NA, 11)))
  tbl <- dummy_survey(survey_tbl(df))
  tbl <- tidyr::fill(tbl, Year)

  expect_s3_class(tbl, "Survey")
  expect_identical(get_association(tbl, "mainentity"), setNames("Q1", "mainentity"))
})
