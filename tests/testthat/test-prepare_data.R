context("Prepare data (mean/pls-latents)")

sav <- seamless::read_data(system.file("extdata", "raw_data.sav", package = "reporttoolDT"))

srv <- survey(sav)
srv <- set_association(srv, common = TRUE)
srv <- set_config(srv, name = "Example", segment = "B2C", cutoff = .3)
srv <- set_marketshare(srv, CompanyA = .3, CompanyB = .5, CompanyC = .2)
srv <- set_translation(srv, language = "english")

test_that("latents_mean", {
  x <- latents_mean(srv)
  expect_true(all(default_latents() %in% names(x)))
  expect_identical(unname(get_association(x, "percent_missing")), "percent_missing")
  expect_identical(unname(get_label(x, "prodq")), "Product quality")
})

test_that("latents_pls", {
  x <- latents_pls(srv)
  expect_true(!any(default_latents() %in% names(x)))
  expect_identical(round(x$data$percent_missing, 1)[1:3], c(0.2, 0.1, 0.0))
  expect_identical(unname(get_association(x, "percent_missing")), "percent_missing")
})

# Add latents before next tests
srv <- latents_mean(srv)

test_that("add_weight", {
  x <- add_weight(srv)
  expect_identical(round(x$data$w[c(1, 15, 17)], 1), c(.6, 2.0, .8))
})

test_that("add_latent_spread", {
  x <- add_latent_spread(srv)
  expect_true(all(paste0(default_latents(), "_spread") %in% names(x)))
  expect_identical(levels(x$data$epsi_spread), c("-60", "60-75", "75-100"))
  expect_identical(x$data$epsi_spread[1:3], factor(c(3,2,1),  labels = c("-60", "60-75", "75-100")))
})