context("Regular dplyr verbs for Survey")

org <- data.frame(Q1 = c("Example 1", "Example 2"),
                  Score = c(9, 8),
                  stringsAsFactors = FALSE)

dummy_survey <- function(x) {
  x$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
  x
}

# Mutate -----------------------------------------------------------------------
test_that("mutate works with Survey_df", {
  skip_if_not_installed("dplyr")

  df <- dummy_survey(survey_df(org))
  df <- dplyr::mutate(df, test = "test")

  expect_s3_class(df, "Survey_df")
  expect_identical(df$data$test, rep("test", 2))
  expect_true("test" %in% df$get_association())

})

test_that("mutate works with Survey_dt", {
  skip_if_not_installed("dplyr")

  dt <- dummy_survey(survey_dt(org))
  dt <- dplyr::mutate(dt, test = "test")

  expect_s3_class(dt, "Survey_dt")
  expect_identical(dt$data$test, rep("test", 2))
  expect_true("test" %in% dt$get_association())

})

test_that("mutate works with Survey_tbl", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::mutate(tbl, test = "test")

  expect_s3_class(tbl, "Survey_tbl")
  expect_identical(tbl$data$test, rep("test", 2))
  expect_true("test" %in% tbl$get_association())

})

# Select -----------------------------------------------------------------------
test_that("select works with Survey_df", {
  skip_if_not_installed("dplyr")

  df <- dummy_survey(survey_df(org))
  df <- dplyr::select(df, Q1)

  expect_s3_class(df, "Survey_df")
  expect_true(ncol(df) == 1L)
  expect_identical(df$get_label(), setNames("test label", "Q1"))
  expect_identical(names(df), "Q1")

})

test_that("select works with Survey_dt", {
  skip_if_not_installed("dplyr")

  dt <- dummy_survey(survey_dt(org))
  dt <- dplyr::select(dt, Q1)

  expect_s3_class(dt, "Survey_dt")
  expect_true(ncol(dt) == 1L)
  expect_identical(dt$get_label(), setNames("test label", "Q1"))
  expect_identical(names(dt), "Q1")

})

test_that("select works with Survey_tbl", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::select(tbl, Q1)

  expect_s3_class(tbl, "Survey_tbl")
  expect_true(ncol(tbl) == 1L)
  expect_identical(tbl$get_label(), setNames("test label", "Q1"))
  expect_identical(names(tbl), "Q1")

})

# Other verbs ------------------------------------------------------------------
# Note: Only checking Survey_tbl for remaining joins. Assuming _df and _dt work.
test_that("filter works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::filter(tbl, Score > 8)

  expect_s3_class(tbl, "Survey_tbl")
  expect_true(nrow(tbl) == 1L)

})

test_that("summarise works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::summarise(tbl, Score = mean(Score))

  expect_s3_class(tbl, "Survey_tbl")
  expect_equal(tbl$data$Score, 8.5)

})

test_that("arrange works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::arrange(tbl, Score)

  expect_s3_class(tbl, "Survey_tbl")
  expect_identical(tbl$data$Q1, c("Example 2", "Example 1"))

})

test_that("tbl_vars works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  expect_identical(dplyr::tbl_vars(tbl), names(tbl))

})

test_that("group_by works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::group_by(tbl, Q1)

  expect_s3_class(tbl, "Survey_tbl")
  expect_identical(as.character(dplyr::groups(tbl)), "Q1")

})

test_that("ungroup works with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::group_by(tbl, Q1)
  tbl <- dplyr::ungroup(tbl)

  expect_s3_class(tbl, "Survey_tbl")
  expect_identical(dplyr::groups(tbl), NULL)

})

test_that("rename works with Survey_tbl (and _df)", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::rename(tbl, entity = Q1)

  expect_s3_class(tbl, "Survey_tbl")
  expect_identical(names(tbl)[1], "entity")
  expect_identical(tbl$get_association("mainentity"), setNames("entity", "mainentity"))

})

test_that("rename works with Survey_dt", {
  skip_if_not_installed("dplyr")

  dt <- dummy_survey(survey_dt(org))
  dt <- dplyr::rename(dt, entity = Q1)

  expect_s3_class(dt, "Survey_dt")
  expect_identical(names(dt)[1], "entity")
  expect_identical(dt$get_association("mainentity"), setNames("entity", "mainentity"))

})
