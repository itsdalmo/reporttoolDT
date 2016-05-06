context("Survey tables")

sav <- officeR::read_data(system.file("extdata", "raw_data.sav", package = "reporttoolDT"))
srv <- survey_tbl(sav)$set_association(common = TRUE)

# Merge attributes -------------------------------------------------------------
test_that("qtable method for survey", {

  out <- stable(srv, vars = "q17", groups = "q1", wide = TRUE)
  expect_identical(names(out)[1L], "q1: Supplier")
  expect_identical(out$n, c(10L, 5L, 5L, 20L))
  expect_identical(out$Nei, c(.9, .8, .8, .85))
})

test_that("manifest table for survey", {

  out <- srv
  out[["q3EM"]] <- rescale_100(clean_scale(out[["q3"]]))
  expect_warning(out <- manifest_table(out), "Margin is unweighted.")
  expect_identical(round(out$q3EM, digits = 1L), 75.6)

})

test_that("latent table for survey", {

  out <- srv
  out[["epsi"]] <- 100L
  expect_warning(out <- latent_table(out), "Margin is unweighted.")
  expect_identical(out$epsi, 100)

})