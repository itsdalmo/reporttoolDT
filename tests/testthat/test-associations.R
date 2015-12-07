context("set/get associations")

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
  expect_error(get_association(y, "mainentity"))

  # Found association
  y <- set_association(y, mainentity = "Q1")
  res <- get_association(y, "mainentity")
  expect_identical(res, "Q1")

})