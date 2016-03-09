context("Internal functions")

test_that("intranet_link", {
  expect_error(intranet_link("sp.com/test/"))
  expect_identical(intranet_link("https://sp.com/test/"), "\\\\sp.com@SSL/DavWWWRoot/test/")
})