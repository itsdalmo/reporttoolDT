context("Initializing a Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)
sav <- haven::read_sav("./test.sav")

test_that("We can initialize Surveys.", {

  df <- survey_df(org)
  dt <- survey_dt(org)
  tb <- survey_tbl(org)

  expect_identical(class(df), c("Survey_df", "Survey", "R6"))
  expect_identical(class(dt), c("Survey_dt", "Survey", "R6"))
  expect_identical(class(tb), c("Survey_tbl", "Survey", "R6"))

  expect_equal(df, survey(org))
  expect_equal(dt, survey(data.table::as.data.table(org)))
  expect_equal(tb, survey(dplyr::as.tbl(org)))

})

test_that("We can initialize Surveys read from haven (labelled).", {

  df <- survey_df(sav)
  dt <- survey_dt(sav)
  tb <- survey_tbl(sav)

  expect_identical(class(df), c("Survey_df", "Survey", "R6"))
  expect_identical(class(dt), c("Survey_dt", "Survey", "R6"))
  expect_identical(class(tb), c("Survey_tbl", "Survey", "R6"))

  expect_equal(df, survey(as.data.frame(sav)))
  expect_equal(dt, survey(data.table::as.data.table(sav)))
  expect_equal(tb, survey(dplyr::as.tbl(sav)))

})

test_that("Labels are retained when converting to Survey.", {

  df <- survey_df(sav)
  lbls <- df$get_label()
  expect_identical(lbls[!is.na(lbls)], unlist(lapply(sav, attr, which = "label")))

})

test_that("names/dimnames/names work for Survey.", {

  df <- survey_df(org)
  dt <- survey_dt(org)

  expect_identical(dim(df), c(2L, 2L))
  expect_identical(dim(dt), c(2L, 2L))

  expect_identical(dimnames(df), list(c("1", "2"), c("Q1", "Score")))
  expect_identical(dimnames(dt), list(NULL, c("Q1", "Score")))

  expect_identical(names(df), c("Q1", "Score"))
  expect_identical(names(dt), c("Q1", "Score"))

})

test_that("We can rename the data.", {

  df <- survey_df(org)
  dt <- survey_dt(org)

  df <- df$update_names(c("a", "b"))
  dt <- dt$update_names(c("a", "b"))

  expect_identical(names(df$get_association()), c("a", "b"))
  expect_identical(names(dt$get_association()), c("a", "b"))

})

