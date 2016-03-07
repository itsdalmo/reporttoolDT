context("Utility functions")

test_that("replace_all", {

  x <- c("a", "b", "c", "d")

  y <- c(bar = "c", foo = "a")
  expect_identical(replace_all(x, y), c("foo", "b", "bar", "d"))

  y <- list(bar = c("a", "c"), foo = "b")
  expect_identical(replace_all(x, y), c("bar", "foo", "bar", "d"))

  y <- list(bar = "a")
  expect_identical(replace_all(x, y), c("bar", "b", "c", "d"))

  # Using "by"
  y <- list("bar" = c("a", "c"), "foo" = "b")
  expect_identical(replace_all(1:4, y, by = x), c("bar", "foo", "bar", 4))

})

test_that("clean scores", {

  expect_true(clean_score("1 aa") == "1")
  expect_identical(clean_score(c("1 aa", "bb 1", "10 cc")), c(1, NA, 10))

})

test_that("rescaling scores", {

  expect_true(clean_score("1 aa") == "1")
  expect_identical(clean_score(c("1 aa", "bb 1", "10 cc")), c(1, NA, 10))
  expect_identical(rescale_score(c("1", 10)), c(0, 100))

})

test_that("get_default", {

  expect_identical(get_default("pal"), internal_defaults$palette)
  expect_identical(get_default("laten"), internal_defaults$latents)
  expect_identical(get_default("latent"), internal_defaults$latents)

})

test_that("clean path", {

  path <- system.file("tests/testthat/xlsx.xlsx", package="reporttool")
  expect_false(stringi::stri_detect(clean_path(paste0(path, "/")), regex = "/$"))

  expect_error(validate_path(rep(path, 2)))
  expect_error(validate_path(numeric(1)))

})


test_that("intranet link", {

  x <- "https://test.se/Sharepoint/Folder"
  x_w <- "\\\\test.se@SSL/DavWWWRoot/Sharepoint/Folder"

  if (Sys.info()["sysname"] == "Windows") {
    expect_identical(intranet_link(x), x_w)
  } else {
    expect_identical(x, x)
  }

})

