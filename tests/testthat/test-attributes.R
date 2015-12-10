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
