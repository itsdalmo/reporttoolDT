context("Mutating surveys")

x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)

test_that("Initializing a survey works", {
  y <- survey(x)
  expect_true(is.survey(y))
  expect_true(all(c("labels", "associations", "translations", "config") %in% names(attributes(y))))
})

test_that("Adding columns to a survey", {

  y <- survey(x)
  y[, test := 1L]
  expect_identical(names(attr(y, "associations")), names(y))

  y <- survey(x)
  y$test <- 1L
  expect_identical(names(attr(y, "associations")), names(y))

  y <- survey(x)
  y[["test"]] <- 1L
  expect_identical(names(attr(y, "associations")), names(y))

})

test_that("Joins via [-method for survey", {

  y <- setkey(survey(x), "Q1")
  z <- y[x]
  expect_identical(names(attr(z, "associations")), names(z))

  # With attributes
  y <- setkey(survey(x), "Q1")
  set_association(y, mainentity = "Q1")
  z <- y[x]
  expect_identical(get_association(z, "mainentity"), "Q1")

})


test_that("rbind works with surveys", {

  # Simple rbind
  y <- survey(x)


  z <- rbind(y, x)

})