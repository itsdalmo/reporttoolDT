context("Public methods and descriptives for Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)
sav <- seamless::read_data(system.file("extdata", "sample.sav", package = "reporttoolDT"))


# Public methods ---------------------------------------------------------------
test_that("Entities", {
  df <- survey_df(org)$set_association(mainentity = "Q1")
  en <- df$entities()
  expect_identical(en, entities(df))
  expect_output(print(en), "Entities")
})

test_that("Model", {
  df <- survey_df(org)
  mm <- df$model()
  expect_identical(mm, model(df))
  expect_output(print(mm), "Measurement model")
})

# Renaming ---------------------------------------------------------------------
test_that("Hidden fields are updated when Survey_df is renamed.", {

  df <- survey_df(org)
  df <- df$set_names(c("a", "b"))
  expect_identical(unname(df$get_association()), c("a", "b"))

})

test_that("Hidden fields are updated when Survey_dt is renamed.", {

  dt <- survey_dt(org)
  dt <- dt$set_names(c("a", "b"))
  expect_identical(unname(dt$get_association()), c("a", "b"))

})

test_that("Hidden fields are updated when Survey_dt is renamed.", {
  skip_if_not_installed("dplyr")

  tbl <- survey_tbl(org)
  tbl <- tbl$set_names(c("a", "b"))
  expect_identical(unname(tbl$get_association()), c("a", "b"))

})

# Descriptives -----------------------------------------------------------------
test_that("head/tail works for Survey_df", {
  df <- survey_df(org)
  expect_s3_class(head(df, 1), "Survey")
  expect_equal(head(df, 1), df[1, ])

  expect_s3_class(tail(df, 1), "Survey")
  expect_equal(tail(df, 1), df[2, ])
})

test_that("head/tail works for Survey_dt", {
  dt <- survey_dt(org)
  expect_s3_class(head(dt, 1), "Survey")
  expect_equal(head(dt, 1), dt[1, ])

  expect_s3_class(tail(dt, 1), "Survey")
  expect_equal(tail(dt, 1), dt[2, ])
})

test_that("head/tail works for Survey_tbl", {
  tbl <- survey_tbl(org)
  expect_s3_class(head(tbl, 1), "Survey")
  expect_equal(head(tbl, 1), tbl[1, ])

  expect_s3_class(tail(tbl, 1), "Survey")
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



