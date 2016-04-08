context("Internal functions")

test_that("str_list", {
  expect_error(str_list(1))
  expect_identical(str_list("A"), "A")
  expect_identical(str_list(c("A", "B")), "A and B")
})

test_that("str_just", {
  x <- paste(letters[1:10], collapse = "")
  expect_error(str_just(1))
  expect_identical(str_just(x, n = 8), "abcde...")
  expect_identical(str_just(x, n = 11, pad = " "), "abcdefghij ")
  expect_identical(str_just(NA_character_), NA_character_)
})

test_that("clean scores", {

  x <- c("1 Not happy", "Very happy 10", "Invalid 11", "Don't know")
  expect_identical(clean_scale(x), c(1L, 10L, NA, NA))

})

test_that("rescaling scores", {

  x <- c("1 Not happy", "Very happy 10", "Invalid 11", "Don't know")
  expect_identical(clean_scale(x), c(1L, 10L, NA, NA))
  expect_identical(rescale_100(clean_scale(x)), c(0, 100, NA, NA))

})
