rm(list = ls(all = TRUE))
library(devtools)
library(testthat)
load_all()

x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)
y <- survey(x)

y[, d := "test"]
y <- set_association(y, "test" = "d")
z <- rbind(y, x, fill = TRUE)
expect_identical(unname(attr(z, "associations")), c(NA, NA, "test"))

attributes(z)

names(z)[3] <- "lol"
x <- z
setattr(x, "labels", setNames(attr(x, "labels"), names(x)))
setattr(x, "associations", setNames(attr(x, "associations"), names(x)))
x