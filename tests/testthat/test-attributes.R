context("update/merge attributes")

x <- list("config" = setNames(c("yes", "no", NA), c("foo", "bar", "zoo")))
y <- list("config" = setNames(c("no", NA, "new"), c("foo", "bar", "zoo")))

test_that("Updating attributes works", {
  z <- update_named_attr(x$config, y$config)
  expect_identical(z, c(x$config[1:2], y$config[3]))
})

test_that("Merging attributes works", {
  z <- merge_attributes(list(x, y))
  expect_identical(z$config, c(x$config[1:2], y$config[3]))
})
