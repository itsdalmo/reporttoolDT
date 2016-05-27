context("Internal functions")

test_that("str_list", {
  expect_error(str_list(1))
  expect_identical(str_list("A"), "A")
  expect_identical(str_list(c("A", "B")), "'A' and 'B'")
})

test_that("str_just", {
  x <- paste(letters[1:10], collapse = "")
  expect_error(str_just(1))
  expect_identical(str_just(x, n = 8), "abcde...")
  expect_identical(str_just(x, n = 11, pad = " "), "abcdefghij ")
  expect_identical(str_just(NA_character_), NA_character_)
})

test_that("clean scale", {

  x <- c("1 Not happy", "Very happy 10", "Invalid 11", "Don't know")
  expect_identical(clean_scale(x), c(1, 10, NA, NA))

})

test_that("rescaling scores", {

  x <- c("1 Not happy", "Very happy 10", "Invalid 11", "Don't know")
  expect_identical(clean_scale(x), c(1, 10, NA, NA))
  expect_identical(rescale_100(clean_scale(x)), c(0, 100, NA, NA))
  expect_identical(rescale_10(rescale_100(clean_scale(x))), c(1, 10, NA, NA))

})

test_that("as_scale works", {
  x <- c("10 such happy", "1 Not happy", "1 Not at all happy", "Don't know")
  y <- as_scale(x)
  expect_identical(levels(y), c("1 Not happy", "10 such happy", "Don't know"))
  expect_identical(as.character(y), c("10 such happy", "1 Not happy", "1 Not happy", "Don't know"))
})

test_that("as_scale also works with numerics", {
  x <- c(1L, 3L, 2L)
  y <- as_scale(x)
  expect_identical(as.character(y), c("1", "3", "2"))
  expect_identical(levels(y), c("1", "2", "3"))
})

test_that("as_scale does not touch labels if no numerals are found", {
  x <- c("Don't know", "wow")
  y <- as_scale(x)
  expect_identical(y, as.factor(x))
})