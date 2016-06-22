context("Regular dplyr verbs for Survey")

sav <- seamless::read_data(system.file("extdata", "raw_data.sav", package = "reporttoolDT"))
srv <- survey_tbl(sav)
srv <- set_config(srv, cutoff = .3)
srv <- set_association(srv, .common = TRUE)
srv <- latents_mean(srv)

# Simple tests just to see that the code runs without errors.

test_that("latent_plot", {
  out <- latent_plot(srv, groups = "q1")
  expect_is(out, "ggplot")
})

test_that("manifest_plot", {
  out <- manifest_plot(srv, groups = "q1")
  expect_is(out, "ggplot")
})

test_that("bar_chart_", {
  out <- bar_chart_(srv, vars = "q17", groups = "q1")
  expect_is(out, "ggplot")
})

test_that("line_chart_", {
  out <- line_chart_(srv, vars = default_latents(), groups = "q1")
  expect_is(out, "ggplot")
})

test_that("flow_chart", {
  weights <- vector("list", length = 7L)
  weights <- setNames(lapply(weights, rep, x = 0L, 7), default_latents())
  weights <- c(list(origin = default_latents()), weights)
  weights <- as.data.frame(weights)

  scores <- data.frame(variable = default_latents(), value = rep(90, 7), stringsAsFactors = FALSE)

  out <- flow_chart(weights, scores = scores)
  expect_is(out, "ggplot")
})