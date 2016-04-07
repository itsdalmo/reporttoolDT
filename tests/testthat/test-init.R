context("Initializing a Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)
sav <- officeR::read_data(system.file("extdata", "sample.sav", package = "reporttoolDT"))

# Initialize -------------------------------------------------------------------
test_that("We can initialize from data.frame", {

  df <- survey_df(org)
  expect_is(df, "Survey_df")
  expect_equal(df, survey(org))

})

test_that("We can initialize from data.table", {

  dt <- survey_dt(org)
  expect_is(dt, "Survey_dt")
  expect_equal(dt, survey(data.table::as.data.table(org)))

})

test_that("We can initialize from tbl (dplyr)", {
  skip_if_not_installed("dplyr")

  tbl <- survey_tbl(org)
  expect_is(tbl, "Survey_tbl")
  expect_equal(tbl, survey(dplyr::as.tbl(org)))

})

test_that("Initializing on a Survey returns the Survey", {
  df <- survey_df(org)
  dt <- survey_dt(org)
  tbl <- survey_tbl(org)

  expect_equal(survey_df(df), df)
  expect_equal(survey_dt(dt), dt)
  expect_equal(survey_tbl(tbl), tbl)

})

test_that("We can initialize a Survey using as.survey", {
  df <- as.survey(org)
  expect_is(df, "Survey")
  expect_identical(as.survey(df), df)
})

# Initialize w/labels ----------------------------------------------------------
test_that("We can initialize with Survey_df with labels", {

  df <- survey_df(sav)
  expect_is(df, "Survey_df")
  labels <- df$get_label()
  expect_identical(labels[!is.na(labels)], unlist(lapply(sav, attr, "label")))

})

test_that("We can initialize with Survey_dt with labels", {

  dt <- survey_dt(sav)
  expect_is(dt, "Survey_dt")
  labels <- dt$get_label()
  expect_identical(labels[!is.na(labels)], unlist(lapply(sav, attr, "label")))

})

test_that("We can initialize with Survey_tbl with labels", {
  skip_if_not_installed("dplyr")

  tbl <- survey_tbl(sav)
  expect_is(tbl, "Survey_tbl")
  labels <- tbl$get_label()
  expect_identical(labels[!is.na(labels)], unlist(lapply(sav, attr, "label")))

})