context("qtable (quick table)")

sav <- officeR::read_data(system.file("extdata", "raw_data.sav", package = "reporttoolDT"))
df <- get_data(survey_dt(sav))
df[, `:=`(c("q3", "q15b"), lapply(.SD, clean_scale)), .SDcols = c("q3", "q15b")]
df[, q4a := spread_10(clean_scale(q4a))][, q4b := spread_10(clean_scale(q4b))]

# Numeric ----------------------------------------------------------------------
test_that("single numeric", {
  x <- qtable(df, vars = "q3")
  expect_equal(dim(x), c(1, 2))
  expect_equal(names(x), c("n", "q3"))
})

test_that("single numeric with groups", {
  x <- qtable(df, vars = "q3", groups = "q1")
  expect_equal(dim(x), c(4, 3))
  expect_equal(names(x), c("q1", "n", "q3"))

  x <- qtable(df, vars = "q3", groups = c("q1", "q17"))
  expect_equal(dim(x), c(4, 5)) # Will fail when complete_df is done.
  expect_equal(names(x)[c(1, 2, 4, 5)], c("q1", "n", "Nei", "Vet ikke"))
})

test_that("multiple numerics", {
  x <- qtable(df, vars = c("q3", "q15b"))
  expect_equal(dim(x), c(1,3))
  expect_equal(names(x), c("n", "q3", "q15b"))
})

test_that("multiple numerics with groups", {
  x <- qtable(df, vars = c("q3", "q15b"), groups = "q1")
  expect_equal(dim(x), c(4, 4))
  expect_equal(names(x), c("q1", "n", "q3", "q15b"))

  x <- qtable(df, vars = c("q3", "q15b"), groups = c("q1", "q17"))
  expect_equal(dim(x), c(9, 5)) # Will fail when complete_df is done.
  expect_equal(names(x), c("q1", "q17",  "n", "q3", "q15b"))
})

# Factor/character -------------------------------------------------------------
test_that("single factor", {
  x <- qtable(df, vars = "q4a")
  # TODO: Don't spread if not grouped.
  # expect_equal(dim(x), c(1, 8))
})

test_that("single factor with groups", {
  x <- qtable(df, vars = "q4a", groups = "q1")
  expect_equal(dim(x), c(4, 6))
  expect_equal(names(x), c("q1", "n", "1-6", "7-8", "9-10", "NA"))

  x <- qtable(df, vars = "q4a", groups = c("q1", "q17"))
  expect_equal(dim(x), c(9, 7)) # Will fail when complete_df is done.
  expect_equal(names(x)[c(1:4)], c("q1", "q17", "n", "1-6"))
})

test_that("multiple factors", {
  expect_error(x <- qtable(df, vars = c("q4a", "q5a")), "identical levels")
  x <- qtable(df, vars = c("q4a", "q4b"))
  expect_equal(dim(x), c(2, 6))
})

test_that("multiple factors", {
  x <- qtable(df, vars = c("q4a", "q4b"), groups = c("q1", "q17"))
  expect_equal(dim(x), c(18, 8))
})

# Date -------------------------------------------------------------------------
# TODO
