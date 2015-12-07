context("Split/join")

x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)

test_that("Creating a new survey", {

  # Same columns
  y <- survey(x)
  z <- rbind(y, x)
  expect_true(is.survey(z))
  expect_true(all(default$attributes %in% names(attributes(z))))

  # New columns in first DT
  y[, d := "test"]
  y <- set_association(y, "test" = "d")
  z <- rbind(y, x, fill = TRUE)
  expect_identical(unname(attr(z, "associations")), c(NA, NA, "test"))

  # New columns in second DT
  z <- rbind(survey(x), y, fill = TRUE)
  expect_identical(unname(attr(z, "associations")), c(NA, NA, "test"))

  # Joins
  z <- merge(y, x, by = c("Q1", "Score"))

})