context("Utility functions")

test_that("any_fractions", {
  expect_true(any_fractions(c(1L, NA, 1.3)))
  expect_false(any_fractions(c(1L, NA, 11)))
})

test_that("get_default", {

  expect_error(get_default(1L))
  expect_error(get_default("pa"), exact = TRUE) # Matches both palette and pattern
  expect_identical(get_default("pal"), internal_defaults$palette)
  expect_identical(get_default("pale"), internal_defaults$palette)

})

test_that("default_palette", {
  expect_identical(default_palette(), internal_defaults$palette)
})

test_that("default_latents", {
  expect_identical(default_latents(), internal_defaults$latents)
})

test_that("clean_path", {
  expect_error(clean_path(c("test", "file")))
})

test_that("basename_sans_ext", {
  expect_identical(basename_sans_ext("test.sav"), "test")
})

# Scoping issues ---------------------------------------------------------------
org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)

test_that("capture_dots can be used inside functions", {
  df <- survey_df(org)$set_association(mainentity = "Q1")
  variable <- "Score"
  f <- function(x) {
    env <- list2env(list(srv = x, parent = environment()))
    variable <- "Q1"
    x[[variable]]
  }
  expect_identical(f(df$data), f(df))

})

test_that("capture_dots uses regular scoping", {
  df <- survey_df(org)$set_association(mainentity = "Q1")
  f <- function(x) {
    variable <- "Q1"
    x[[variable]]
  }
  f2 <- function(x) {
    f(x)
  }
  expect_identical(f2(df$data), f2(df))

})
