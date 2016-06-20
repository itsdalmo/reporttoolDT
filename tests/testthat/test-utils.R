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
  expect_identical(clean_path("./"), getwd())
  expect_identical(clean_path(stri_c(getwd(), "/")), getwd())
})

test_that("basename_sans_ext", {
  expect_identical(basename_sans_ext("test.sav"), "test")
})

test_that("%||%", {
  expect_identical(NULL %||% "test", "test")
})
