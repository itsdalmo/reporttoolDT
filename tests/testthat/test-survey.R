context("Survey")

x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)

test_that("Creating a new survey", {

  y <- survey(x)
  expect_true(inherits(y, "survey"))
  expect_true(inherits(y, "data.table"))

})

test_that("Working with survey using [", {

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
  expect_identical(unname(attr(y, "associations")), c("mainentity", NA))

})

test_that("Working with survey using [", {

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
  expect_identical(unname(attr(y, "associations")), c("mainentity", NA))

})

test_that("Working with survey using [[", {

  y <- survey(x)
  y[["Q1"]] <- c("a", "b")

  expect_identical(y[["Q1"]], c("a", "b"))

})

test_that("Working with survey using $", {

  y <- survey(x)
  y$Q1 <- c("a", "b")

  expect_identical(y$Q1, c("a", "b"))

})