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
test_that("complete works with Survey_dt", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  df <- dummy_survey(survey_df(org))
  lvl <- c("Example 1", "Example 2", "Example 3")
  df$data$Q1 <- factor(df$data$Q1, levels = lvl)
  expect_warning(
    df <- tidyr::complete(df, Q1),
    "Class has changed"
  )


  expect_s3_class(df, "Survey_tbl")
  expect_identical(as.character(df$data$Q1), lvl)

})

test_that("complete works with Survey_dt", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  dt <- dummy_survey(survey_dt(org))
  lvl <- c("Example 1", "Example 2", "Example 3")
  dt$data$Q1 <- factor(dt$data$Q1, levels = lvl)
  expect_warning(
    dt <- tidyr::complete(dt, Q1),
    "Class has changed"
  )


  expect_s3_class(dt, "Survey_tbl")
  expect_identical(as.character(dt$data$Q1), lvl)

})

test_that("complete works with Survey_tbl", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")

  tbl <- dummy_survey(survey_tbl(org))
  lvl <- c("Example 1", "Example 2", "Example 3")
  tbl$data$Q1 <- factor(tbl$data$Q1, levels = lvl)
  tbl <- tidyr::complete(tbl, Q1)

  expect_s3_class(tbl, "Survey_tbl")
  expect_identical(as.character(tbl$data$Q1), lvl)

})
