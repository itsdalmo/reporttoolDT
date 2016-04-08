context("recode and recode_")

test_that("recode_", {

  x <- c("a", "b", "c", "d")
  y <- c(bar = "c", foo = "a")
  expect_identical(recode_(x, as.list(y)), c("foo", "b", "bar", "d"))

  y <- list(bar = c("a", "c"), foo = "b")
  expect_identical(recode_(x, y), c("bar", "foo", "bar", "d"))

  y <- list(bar = "a")
  expect_identical(recode_(x, y), c("bar", "b", "c", "d"))

  # Using "by"
  y <- list("bar" = c("a", "c"), "foo" = "b")
  expect_identical(recode_(1:4, y, by = x), c("bar", "foo", "bar", 4))

})

test_that("recode works successively", {

  x <- 1:3
  y <- recode(x, "<3.5" = . < 3.5, "<2.5" = . < 2.5, "<1.5" = . < 1.5, factor = FALSE)

  expect_identical(y, c("<1.5", "<2.5", "<3.5"))

})

test_that("recode works with 'by'", {

  x <- c("a", "b", "c")
  y <- c("d", "e", "f")

  z <- recode(x, "a" = . %in% "f", by = y, factor = FALSE)
  expect_identical(z, c("a", "b", "a"))

})

test_that("recode works with 'by' when converting to factor", {

  x <- c("a", "b", "c")
  y <- c("d", "e", "f")

  # Intended functionality. Make x a factor before recoding to keep all levels.
  z <- recode(x, "a" = . %in% "f", by = y, factor = TRUE)
  expect_identical(z, factor(c("a", NA, "a"), levels = "a"))


})

test_that("recode works for integer", {

  x <- c(1L, 2L, 3L)
  y <- recode(x, "test" = . %in% 1:2, factor = FALSE)
  expect_identical(y, c("test", "test", "3"))

})

test_that("recode works for numerics", {

  x <- c(0, 50, 100)
  x <- x + .5
  y <- recode(x, "test" = . <= 50.5, factor = FALSE)
  expect_identical(y, c("test", "test", "100.5"))

})

test_that("recode works for character", {

  x <- c("a", "b", "c")
  y <- recode(x, "a" = . %in% "c", factor = FALSE)
  expect_identical(y, c("a", "b", "a"))

})

test_that("recode with factor works for integer/numeric/character", {

  # Integer
  x <- c(1L, 2L, 3L)
  y <- recode(x, "test" = 1:3, factor = TRUE)
  expect_identical(y, factor(rep("test", 3), levels = "test"))

  # Numeric
  x <- c(0, 50, 100)
  x <- x + .5
  y <- recode(x, "test" = . <= 50.5, factor = TRUE)
  expect_identical(y, factor(c("test", "test", NA), levels = "test"))

  # Character
  x <- c("a", "b", "c")
  y <- recode(x, "a" = . %in% "c", factor = TRUE)
  expect_identical(y, factor(c("a", NA, "a"), levels = "a"))

})


test_that("recode works for factors", {

  x <- factor(c("a", "b", "c"))
  y <- recode(x, "a" = . %in% "c", drop = FALSE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b", "c"))

  # Add works
  y <- recode(x, "d" = . %in% "a", drop = TRUE, add = TRUE)
  expect_identical(y, factor(c("d", "b", "c"), levels = c("b", "c", "d")))
  y <- recode(x, "d" = . %in% "a", drop = FALSE, add = TRUE)
  expect_identical(y, factor(c("d", "b", "c"), levels = c("a", "b", "c", "d")))

  # Drop works
  y <- recode(x, "a" = . %in% "c", drop = TRUE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b"))

  # But only if it is explicitly recoded
  levels(x) <- c(levels(x), "d")
  y <- recode(x, "a" = . %in% "c", drop = TRUE)
  expect_identical(as.character(y), c("a", "b", "a"))
  expect_identical(levels(y), c("a", "b", "d"))

})

test_that("spread_10 works", {
  x <- c(3, 7, 10)
  expect_identical(spread_10(x), factor(c("1-6", "7-8", "9-10")))
  expect_error(spread_10("test"))
  expect_error(spread_10(1.5))
})

test_that("spread_100 works", {
  x <- c(3, 60, 75.001)
  expect_identical(spread_100(x), factor(c("-60", "60-75", "75-100")))
  expect_error(spread_100("test"))
})

test_that("spread_age works", {
  x <- c(15, 54, 76)

  # as.character to avoid specifying levels. 15 turns into NA because
  # large age groups do not include 15.
  expect_identical(as.character(spread_age(x)), c(NA, "45-59", "60+"))
  expect_identical(as.character(spread_age(x, small = TRUE)), c("15-24", "50-54", "75+"))
  expect_error(spread_age("test"))
  expect_error(spread_age(50.5))

})