context("Init/mutate surveys")

x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)

test_that("Creating a new survey", {

  y <- survey(x)
  expect_true(inherits(y, "survey"))
  expect_true(inherits(y, "data.table"))
  expect_true(all(c("labels", "associations", "translations", "config") %in% names(attributes(y))))

})

test_that("We can add columns to a survey with :=", {
  y <- survey(x)
  set_association(y, mainentity = "Q1")

  y[, test := 1L]
  expect_identical(names(attr(y, "associations")), names(y))
  expect_identical(get_association(y, "mainentity"), "Q1")
})

test_that("We can add columns to a survey with [", {
  y <- survey(x)
  set_association(y, mainentity = "Q1")

  y[, "test"] <- 1L
  expect_identical(names(attr(y, "associations")), names(y))
  expect_identical(get_association(y, "mainentity"), "Q1")
})

test_that("We can add columns to a survey with $", {
  y <- survey(x)
  set_association(y, mainentity = "Q1")

  y$test <- 1L
  expect_identical(names(attr(y, "associations")), names(y))
  expect_identical(get_association(y, "mainentity"), "Q1")
})

test_that("We can add columns to a survey with [[", {
  y <- survey(x)
  set_association(y, mainentity = "Q1")

  y$test <- 1L
  expect_identical(names(attr(y, "associations")), names(y))
  expect_identical(get_association(y, "mainentity"), "Q1")
})

# test_that("survey can be converted to list", {
#   y <- survey(x)
#   set_association(y, mainentity = "Q1")
#
#   z <- as.list(y)
#   expect_identical(z, list("df" = x))
#
# })