context("update/merge attributes")

x <- list("config" = setNames(c("yes", "no", NA), c("foo", "bar", "zoo")))
y <- list("config" = setNames(c("no", NA, "new"), c("foo", "bar", "zoo")))

test_that("Merging attributes works", {

  z <- merge_attributes(list(x, y))
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