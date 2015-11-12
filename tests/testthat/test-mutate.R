context("Split/join")

x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)

test_that("Creating a new survey", {

  # Same columns
  y <- survey(x)
  z <- rbind(y, x)

  # New columns
  expect_error(rbind(z, y[, d := "test"]))
  z <- rbind(z, y, fill = TRUE)

})