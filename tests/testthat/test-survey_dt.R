context("Working with survey_dt")

x <- data.table::data.table("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9))

check_dt <- function(x) {
  expect_true(all(c("labels", "associations", "translations", "config") %in% names(attributes(x))))
  expect_identical(class(x), c("survey_dt", "survey", "data.table", "data.frame"))
  expect_identical(names(attr(x, "associations")), names(x))
  expect_identical(names(attr(x, "labels")), names(x))
  expect_identical(get_association(x, "mainentity"), "Q1")
}

# ------------------------------------------------------------------------------

test_that("Creating a new survey from data.table", {

  y <- survey(x)
  a <- names(attributes(y))

  expect_identical(class(y), c("survey_dt", "survey", "data.table", "data.frame"))
  expect_true(all(c("labels", "associations", "translations", "config") %in% a))

})


test_that("We can add columns to a survey_dt with :=", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  y[, test := 1L]

  check_dt(y)

})

test_that("We can add columns to a survey_dt with [", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  y[, "test"] <- 1L

  check_dt(y)

})

test_that("We can add columns to survey_dt with $", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  y$test <- 1L

  check_dt(y)

})

test_that("We can add columns to a survey_dt with [[", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  y[["test"]] <- 1L

  check_dt(y)

})

test_that("We can rename survey_dt with <- or setnames", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")

  setnames(y, "Q1", "entity")
  expect_identical(class(y), c("survey_dt", "survey", "data.table", "data.frame"))
  expect_identical(names(attr(y, "associations")), names(y))
  expect_identical(get_association(y, "mainentity"), "entity")

  names(y)[1] <- "Q1"
  check_dt(y)

})


test_that("rbind works with survey_dt", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  z <- rbind(y, x)

  check_dt(z)

})

test_that("cbind works with survey_dt", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  z <- cbind(y, data.frame("test" = c(7, 7)))

  check_dt(z)

})

test_that("Merge works with survey_dt", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  z <- merge(y, x, by = "Q1")

  check_dt(z)

})

test_that("rbind works with multiple survey_dt", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  y[, test := "test"]

  z <- data.table::copy(y)
  z <- set_association(z, "works" = "test")
  z[, extra := "extra"]

  expect_error(rbind(y,z))
  z <- rbind(y, z, fill = TRUE)

  expect_identical(attributes(z)$associations[3], setNames("works", "test"))

  check_dt(z)

})

test_that("Joins using [ works with survey (data.table)", {

  y <- survey(x)
  y <- set_association(y, mainentity = "Q1")
  data.table::setkey(y, "Q1")
  z <- y[x]

  check_dt(z)

})