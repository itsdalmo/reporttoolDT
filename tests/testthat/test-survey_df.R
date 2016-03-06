# context("Working with survey_df")
#
# x <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)
#
# check_df <- function(x) {
#   expect_true(all(c("labels", "associations", "translations", "config") %in% names(attributes(x))))
#   expect_identical(class(x), c("survey_df", "survey", "data.frame"))
#   expect_identical(names(attr(x, "associations")), names(x))
#   expect_identical(names(attr(x, "labels")), names(x))
#   expect_identical(get_association(x, "mainentity"), "Q1")
# }
#
# # ------------------------------------------------------------------------------
#
# test_that("Creating a new survey from data.frame", {
#
#   x <- survey(x)
#   a <- names(attributes(x))
#
#   expect_identical(class(x), c("survey_df", "survey", "data.frame"))
#   expect_true(all(c("labels", "associations", "translations", "config") %in% a))
#
# })
#
# test_that("We can add columns to a survey_df with [", {
#
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   y[, "test"] <- 1L
#
#   check_df(y)
#
# })
#
# test_that("We can add columns to survey_df with $", {
#
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   y$test <- 1L
#
#   check_df(y)
#
# })
#
# test_that("We can add columns to a survey_df with [[", {
#
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   y[["test"]] <- 1L
#
#   check_df(y)
#
# })
#
# test_that("We can rename survey_df with <- or setnames", {
#
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#
#   setnames(y, "Q1", "entity")
#   expect_identical(class(y), c("survey_df", "survey", "data.frame"))
#   expect_identical(names(attr(y, "associations")), names(y))
#   expect_identical(get_association(y, "mainentity"), "entity")
#
#   names(y)[1] <- "Q1"
#   check_df(y)
#
# })
#
# test_that("rbind works with survey_df", {
#
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   z <- rbind(y, x)
#
#   check_df(z)
#
# })
#
# test_that("cbind works with survey_df", {
#
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   z <- cbind(y, data.frame("test" = c(7, 7)))
#
#   check_df(z)
#
# })
#
# test_that("Merge works with survey_df", {
#
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   z <- merge(y, x, by = "Q1")
#
#   check_df(z)
#
# })
#
# test_that("rbind works with multiple survey_df", {
#
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   y$test <- "test"
#
#   z <- set_association(y, "works" = "test")
#   z$extra <- "extra"
#
#   expect_error(rbind(y,z))
#   y$extra <- "ncol?"
#
#   z <- rbind(y, z)
#   expect_identical(attributes(z)$associations[3], setNames("works", "test"))
#
#   check_df(z)
#
# })
