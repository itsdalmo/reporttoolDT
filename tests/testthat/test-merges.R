context("binds/merges/joins for Survey")

org <- data.frame(Q1 = c("Example 1", "Example 2"),
                  Score = c(9, 8),
                  stringsAsFactors = FALSE)

dummy_survey <- function(x) {
  x$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
  x
}

# Bind rows --------------------------------------------------------------------
test_that("bind_rows work with Survey_df", {
  skip_if_not_installed("dplyr")

  df <- dummy_survey(survey_df(org))
  df <- bind_rows(df, dplyr::mutate(df, test = "test"))

  expect_s3_class(df, "Survey")
  expect_identical(names(df), c("Q1", "Score", "test"))
  expect_identical(df$data$test, c(NA, NA, "test", "test"))

})

test_that("bind_rows work with Survey_dt", {
  skip_if_not_installed("dplyr")

  dt <- dummy_survey(survey_dt(org))
  dt <- bind_rows(dt, dplyr::mutate(dt, test = "test"))

  expect_s3_class(dt, "Survey")
  expect_identical(names(dt), c("Q1", "Score", "test"))
  expect_identical(dt$data$test, c(NA, NA, "test", "test"))

})

test_that("bind_rows work with Survey_tbl", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- bind_rows(tbl, dplyr::mutate(tbl, test = "test"))

  expect_s3_class(tbl, "Survey")
  expect_identical(names(tbl), c("Q1", "Score", "test"))
  expect_identical(tbl$data$test, c(NA, NA, "test", "test"))

})

# Bind cols --------------------------------------------------------------------
test_that("bind_cols work with Survey_df", {
  skip_if_not_installed("dplyr")

  df <- dummy_survey(survey_df(org))
  df <- bind_cols(df, dplyr::mutate(df, test = "test"))

  expect_s3_class(df, "Survey")
  expect_identical(df$get_association("mainentity"), setNames(c("Q1", "Q1"), c("mainentity", "mainentity")))
  expect_identical(df$get_label("Q1"), setNames(c("test label", "test label"), c("Q1", "Q1")))

})

test_that("bind_cols work with Survey_dt", {
  skip_if_not_installed("dplyr")

  dt <- dummy_survey(survey_dt(org))
  dt <- bind_cols(dt, dplyr::mutate(dt, test = "test"))

  expect_s3_class(dt, "Survey")
  expect_identical(dt$get_association("mainentity"), setNames(c("Q1", "Q1"), c("mainentity", "mainentity")))
  expect_identical(dt$get_label("Q1"), setNames(c("test label", "test label"), c("Q1", "Q1")))

})

test_that("bind_cols work with Survey_tbl", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- bind_cols(tbl, dplyr::mutate(tbl, test = "test"))

  expect_s3_class(tbl, "Survey")
  expect_identical(tbl$get_association("mainentity"), setNames(c("Q1", "Q1"), c("mainentity", "mainentity")))
  expect_identical(tbl$get_label("Q1"), setNames(c("test label", "test label"), c("Q1", "Q1")))

})

# Left join --------------------------------------------------------------------
test_that("left_join work with Survey_df", {
  skip_if_not_installed("dplyr")

  df <- dummy_survey(survey_df(org))
  df <- dplyr::left_join(df, dplyr::mutate(df[1, ], test = "test"))

  expect_s3_class(df, "Survey")
  expect_identical(df$get_association("mainentity"),  setNames("Q1", "mainentity"))
  expect_identical(df$data$test, c("test", NA))
  expect_identical(df$get_label("Q1"),  setNames("test label", "Q1"))

})

test_that("left_join work with Survey_dt", {
  skip_if_not_installed("dplyr")

  dt <- dummy_survey(survey_dt(org))
  dt <- dplyr::left_join(dt, dplyr::mutate(dt[1, ], test = "test"))


  expect_s3_class(dt, "Survey")
  expect_identical(dt$get_association("mainentity"),  setNames("Q1", "mainentity"))
  expect_identical(dt$data$test, c("test", NA))
  expect_identical(dt$get_label("Q1"),  setNames("test label", "Q1"))

})

test_that("left_join work with Survey_tbl", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::left_join(tbl, dplyr::mutate(tbl[1, ], test = "test"))

  expect_s3_class(tbl, "Survey")
  expect_identical(tbl$get_association("mainentity"),  setNames("Q1", "mainentity"))
  expect_identical(tbl$data$test, c("test", NA))
  expect_identical(tbl$get_label("Q1"),  setNames("test label", "Q1"))

})

# Other joins ------------------------------------------------------------------
# Note: Only checking Survey_tbl for remaining joins. Assuming _df and _dt work.
test_that("right_join work with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::right_join(tbl, dplyr::mutate(tbl[1, ], test = "test"))

  expect_s3_class(tbl, "Survey")
  expect_identical(tbl$get_label("Q1"),  setNames("test label", "Q1"))
  expect_identical(tbl$data$test, "test")
  expect_identical(tbl$data$Score, 9)

})

test_that("full_join work with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::full_join(tbl, dplyr::mutate(tbl[1, ], Q1 = "Example 3", test = "test"))

  expect_s3_class(tbl, "Survey")
  expect_identical(tbl$get_label("Q1"),  setNames("test label", "Q1"))
  expect_identical(tbl$data$test, c(NA, NA, "test"))
  expect_identical(tbl$data$Q1, c("Example 1", "Example 2", "Example 3"))

})

test_that("semi_join work with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::semi_join(tbl, dplyr::mutate(tbl[1, ], test = "test"))

  expect_s3_class(tbl, "Survey")
  expect_identical(tbl$get_label("Q1"),  setNames("test label", "Q1"))
  expect_identical(names(tbl), c("Q1", "Score"))
  expect_identical(nrow(tbl), 1L)

})

test_that("anti_join work with Survey", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- dplyr::anti_join(tbl, dplyr::mutate(tbl[1, ], test = "test"))

  expect_s3_class(tbl, "Survey")
  expect_identical(tbl$get_label("Q1"),  setNames("test label", "Q1"))
  expect_identical(names(tbl), c("Q1", "Score"))
  expect_identical(tbl$data$Q1, "Example 2")

})
