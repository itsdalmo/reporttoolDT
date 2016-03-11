context("Survey/R6 accessors")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)

# Merge attributes -------------------------------------------------------------
test_that("merge_attributes", {

  default <- c("a", "b", "c", "d")
  lst <- c("a" = 1, list(c("b" = 2), "d" = 4))
  res <- merge_vectors(lst, default = default)
  expect_identical(res, c("a" = 1, "b" = 2, "c" = NA, "d" = 4))

  lst[[2]] <- c(lst[[2]], "a" = "test")
  res <- merge_vectors(lst, default = default) # Mode ends up being character.
  expect_identical(res, c("a" = "1", "b" = "2", "c" = NA, "d" = "4"))

  expect_error(merge_vectors("a", lst = 1))
  expect_error(merge_vectors("a", lst = list(1)))

})

# Label ------------------------------------------------------------------------
test_that("setting/getting label works for Survey_df", {
  df <- set_label(survey_df(org), Q1 = "test")
  expect_identical(get_label(df, "Q1"), c("Q1" = "test"))
})

test_that("setting/getting label works for Survey_dt", {
  dt <- set_label(survey_dt(org), Q1 = "test")
  expect_identical(get_label(dt, "Q1"), c("Q1" = "test"))
})

test_that("setting/getting label works for Survey_tbl", {
  tbl <- set_label(survey_tbl(org), Q1 = "test")
  expect_identical(get_label(tbl, "Q1"), c("Q1" = "test"))
})

# Associations -----------------------------------------------------------------
test_that("setting/getting association works for Survey_df", {
  df <- set_association(survey_df(org), mainentity = c("Q1", "Score"))
  expect_identical(get_association(df), c("Q1" = "mainentity", "Score" = "mainentity"))
})

test_that("setting/getting association works for Survey_dt", {
  dt <- set_association(survey_dt(org), mainentity = c("Q1", "Score"))
  expect_identical(get_association(dt), c("Q1" = "mainentity", "Score" = "mainentity"))
})

test_that("setting/getting association works for Survey_tbl", {
  tbl <- set_association(survey_tbl(org), mainentity = c("Q1", "Score"))
  expect_identical(get_association(tbl), c("Q1" = "mainentity", "Score" = "mainentity"))
})

# Marketshare ------------------------------------------------------------------
test_that("We can't set marketshares without specifying mainentity first.", {
  expect_error(survey_df(org)$set_marketshare("Example 1" = .5, "Example 2" = .3))
})

test_that("setting/getting marketshare works for Survey_df", {
  df <- set_association(survey_df(org), mainentity = "Q1")
  df <- set_marketshare(df, "Example 1" = .5)
  expect_identical(get_marketshare(df), c("Example 1" = 0.5, "Example 2" = NA))
})

test_that("setting/getting marketshare works for Survey_dt", {
  dt <- set_association(survey_df(org), mainentity = "Q1")
  dt <- set_marketshare(dt, "Example 1" = .5)
  expect_identical(get_marketshare(dt), c("Example 1" = 0.5, "Example 2" = NA))
})

test_that("setting/getting marketshare works for Survey_tbl", {
  tbl <- set_association(survey_df(org), mainentity = "Q1")
  tbl <- set_marketshare(tbl, "Example 1" = .5)
  expect_identical(get_marketshare(tbl), c("Example 1" = 0.5, "Example 2" = NA))
})

# Config -----------------------------------------------------------------------
test_that("setting/getting config works", {

  # TODO

})

# Translation ------------------------------------------------------------------
test_that("setting/getting translation works", {

  # TODO

})
