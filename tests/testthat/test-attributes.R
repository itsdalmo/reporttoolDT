context("update/merge attributes")

x <- list("config" = setNames(c("yes", "no", NA), c("foo", "bar", "zoo")))
y <- list("config" = setNames(c("no", NA, "new"), c("foo", "bar", "zoo")))


test_that("Updating attributes works", {
  z <- update_attribute(x$config, y$config)
  expect_identical(z, c(x$config[1:2], y$config[3]))

  z <- update_attribute(y$config, x$config)
  expect_identical(z, c(y$config[1], x$config[2], y$config[3]))

})

test_that("Merging attributes works", {
  z <- merge_survey_attributes(list(x, y))
  expect_identical(z$config, c(x$config[1:2], y$config[3]))

  z <- merge_survey_attributes(list(y, x))
  expect_identical(z$config, c(y$config[1], x$config[2], y$config[3]))

})


# Specific attributes ----------------------------------------------------------
x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)

test_that("Setting association works", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")

  expect_true(inherits(y, "survey"))
  expect_identical(unname(attr(y, "associations")), c("mainentity", NA))

})

test_that("Getting associations work", {

  y <- survey(x)

  # No associations (NULL)
  expect_identical(get_association(y, "mainentity"), NULL)

  # Found association
  y <- set_association(y, mainentity = "Q1")
  res <- get_association(y, "mainentity")
  expect_identical(res, "Q1")

})

test_that("Setting marketshares works", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  y <- set_marketshare(y, "Example 1" = .5, "Example 2" = .5)

  expect_true(inherits(y, "survey"))
  expect_identical(unname(attr(y, "associations")), c("mainentity", NA))

})

test_that("Getting marketshares work", {

  y <- survey(x)

  # No associations (NULL)
  expect_identical(get_association(y, "mainentity"), NULL)

  # Found association
  y <- set_association(y, mainentity = "Q1")
  res <- get_association(y, "mainentity")
  expect_identical(res, "Q1")

})