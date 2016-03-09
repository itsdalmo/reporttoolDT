context("Initialization/Public methods for Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)
sav <- haven::read_sav(system.file("extdata", "sample.sav", package = "reporttoolDT"))

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

  expect_true(
    identical(survey_df(df), df) &&
    identical(survey_dt(dt), dt) &&
    identical(survey_tbl(tbl), tbl)
  )

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

# Descriptives -----------------------------------------------------------------
test_that("names/dimnames/names work for Survey_df.", {

  df <- survey_df(org)
  expect_identical(dim(df), c(2L, 2L))
  expect_identical(dimnames(df), list(c("1", "2"), c("Q1", "Score")))
  expect_identical(names(df), c("Q1", "Score"))

})

test_that("names/dimnames/names work for Survey_dt.", {

  dt <- survey_dt(org)
  expect_identical(dim(dt), c(2L, 2L))
  expect_identical(dimnames(dt), list(NULL, c("Q1", "Score")))
  expect_identical(names(dt), c("Q1", "Score"))

})

test_that("names/dimnames/names work for Survey_tbl.", {
  skip_if_not_installed("dplyr")

  tbl <- survey_tbl(org)
  expect_identical(dim(tbl), c(2L, 2L))
  expect_identical(dimnames(tbl), list(c("1", "2"), c("Q1", "Score")))
  expect_identical(names(tbl), c("Q1", "Score"))

})

# Renaming ---------------------------------------------------------------------
test_that("Hidden fields are updated when Survey_df is renamed.", {

  df <- survey_df(org)
  df <- df$set_names(c("a", "b"))
  expect_identical(names(df$get_association()), c("a", "b"))

})

test_that("Hidden fields are updated when Survey_dt is renamed.", {

  dt <- survey_dt(org)
  dt <- dt$set_names(c("a", "b"))
  expect_identical(names(dt$get_association()), c("a", "b"))

})

test_that("Hidden fields are updated when Survey_dt is renamed.", {
  skip_if_not_installed("dplyr")

  tbl <- survey_tbl(org)
  tbl <- tbl$set_names(c("a", "b"))
  expect_identical(names(tbl$get_association()), c("a", "b"))

})

