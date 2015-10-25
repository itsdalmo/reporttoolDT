context("Survey")

x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)

test_that("Creating a new survey", {

  y <- survey(x)
  expect_true(inherits(y, "survey"))
  expect_true(inherits(y, "data.table"))

})

test_that("Working with survey using [[", {

  y <- survey(x)
  y[, "test" := 5]

  expect_true(inherits(y, "survey"))
  expect_true("test" %in% names(y))
  expect_identical(y$test, c(5, 5))

  # After setting attributes
  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  y[, "test" := 5]

  expect_true(inherits(y, "survey"))
  expect_identical(attr(y, "association"), c("mainentity", NA))

})

test_that("Setting association works", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")

  expect_true(inherits(y, "survey"))
  expect_identical(attr(y, "association"), c("mainentity", NA))

})

test_that("Getting associations work", {

  y <- survey(x)

  # No associations (NULL)
  res <- get_association(y, "mainentity")
  expect_true(is.null(res))

  # Found association
  y <- set_association(y, mainentity = "Q1")
  res <- get_association(y, "mainentity")
  expect_identical(res, "Q1")

  # Non existing association (NULL)
  res <- get_association(y, "test")
  expect_true(is.null(res))

})