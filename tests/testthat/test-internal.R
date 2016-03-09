context("Internal functions")

test_that("intranet_link", {
  if (!on_windows()) {
    expect_warning(intranet_link("https://sp.com/test/"))
  }

  expect_error(intranet_link("sp.com/test/"))
  expect_identical(intranet_link("https://sp.com/test/"), "\\\\sp.com@SSL/DavWWWRoot/test/")
})