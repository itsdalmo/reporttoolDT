context("Join/bind surveys")

x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)

test_that("rbind works with surveys", {
  y <- survey(x)
  set_association(y, mainentity = "Q1")

  z <- rbind(y, x)
  expect_identical(names(attr(z, "associations")), names(y))
  expect_identical(get_association(z, "mainentity"), "Q1")
})

test_that("cbind works with surveys", {
  y <- survey(x)
  set_association(y, mainentity = "Q1")

  z <- cbind(y, data.table("test" = c(7, 7)))
  expect_identical(names(attr(z, "associations")), names(z))
  expect_identical(get_association(z, "mainentity"), "Q1")
})

test_that("Joins using [ works with surveys", {
  y <- survey(x)
  set_association(y, mainentity = "Q1")
  setkey(y, "Q1")

  z <- y[x]
  expect_identical(names(attr(z, "associations")), names(z))
  expect_identical(get_association(z, "mainentity"), "Q1")
})

test_that("Merge works with surveys", {
  y <- survey(x)
  set_association(y, mainentity = "Q1")
  setkey(y, "Q1")

  z <- merge(y, x)
  expect_identical(names(attr(z, "associations")), names(z))
  expect_identical(get_association(z, "mainentity"), "Q1")
})