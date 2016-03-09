context("Public methods and descriptives for Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)
sav <- haven::read_sav(system.file("extdata", "sample.sav", package = "reporttoolDT"))

# Public methods ---------------------------------------------------------------
test_that("Entities", {
  df <- survey_df(org)$set_association(mainentity = "Q1")
  expect_identical(df$entities(), entities(df))
})

test_that("Model", {
  df <- survey_df(org)
  expect_identical(df$model(), model(df))
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

# Descriptives -----------------------------------------------------------------
test_that("head/tail works for Survey_df", {
  df <- survey_df(org)
  expect_is(head(df, 1), "Survey_df")
  expect_equal(head(df, 1), df[1, ])

  expect_is(tail(df, 1), "Survey_df")
  expect_equal(tail(df, 1), df[2, ])
})

test_that("head/tail works for Survey_dt", {
  dt <- survey_dt(org)
  expect_is(head(dt, 1), "Survey_dt")
  expect_equal(head(dt, 1), dt[1, ])

  expect_is(tail(dt, 1), "Survey_dt")
  expect_equal(tail(dt, 1), dt[2, ])
})

test_that("head/tail works for Survey_tbl", {
  tbl <- survey_tbl(org)
  expect_is(head(tbl, 1), "Survey_tbl")
  expect_equal(head(tbl, 1), tbl[1, ])

  expect_is(tail(tbl, 1), "Survey_tbl")
  expect_equal(tail(tbl, 1), tbl[2, ])
})

test_that("length/dim work for Survey_df.", {

  df <- survey_df(org)
  expect_identical(dim(df), c(2L, 2L))
  expect_identical(length(df), 2L)

})

test_that("length/dim work for Survey_dt.", {

  dt <- survey_dt(org)
  expect_identical(dim(dt), c(2L, 2L))
  expect_identical(length(dt), 2L)

})

test_that("length/dim work for Survey_tbl.", {
  skip_if_not_installed("dplyr")

  tbl <- survey_tbl(org)
  expect_identical(dim(tbl), c(2L, 2L))
  expect_identical(length(tbl), 2L)

})

test_that("names/dimnames work for Survey_df.", {

  df <- survey_df(org)
  expect_identical(dimnames(df), list(c("1", "2"), c("Q1", "Score")))
  expect_identical(names(df), c("Q1", "Score"))

})

test_that("names/dimnames work for Survey_dt.", {

  dt <- survey_dt(org)
  expect_identical(dimnames(dt), list(NULL, c("Q1", "Score")))
  expect_identical(names(dt), c("Q1", "Score"))

})

test_that("names/dimnames work for Survey_tbl.", {
  skip_if_not_installed("dplyr")

  tbl <- survey_tbl(org)
  expect_identical(dimnames(tbl), list(c("1", "2"), c("Q1", "Score")))
  expect_identical(names(tbl), c("Q1", "Score"))

})



