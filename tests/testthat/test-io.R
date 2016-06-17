context("input/output for Survey's")

sav <- seamless::read_data(system.file("extdata", "raw_data.sav", package = "reporttoolDT"))

test_that("read/write survey to .sav", {
  df <- survey_tbl(sav)$set_association(common = TRUE)
  fileName <- file.path(tempdir(), "survey.sav")
  write_survey(df, fileName)
  dr <- read_survey(fileName)

  expect_equal(df, dr)
  unlink(fileName, recursive = TRUE, force = TRUE)
})

test_that("read/write survey to directory with PLS input.", {
  df <- survey_tbl(sav)
  df <- set_association(df, common = TRUE)
  df <- set_config(df, name = "survey", cutoff = .3)
  df <- latents_pls(df)

  fileName <- file.path(tempdir(), "survey")
  dir.create(fileName, showWarnings = FALSE)
  expect_warning(write_survey(df, fileName), "'Data' and 'Input'")

  fname <- paste0("survey ", format(Sys.Date(), "%Y"), "EM.sav")
  dr <- read_survey(file.path(fileName, "Data", fname))

  expect_equal(df, dr)
  unlink(fileName, recursive = TRUE, force = TRUE)
})

test_that("read/write survey to directory with mean only.", {
  df <- survey_tbl(sav)
  df <- set_association(df, common = TRUE)
  df <- set_config(df, name = "survey", cutoff = .3)
  df <- latents_mean(df)

  fileName <- file.path(tempdir(), "survey")
  dir.create(fileName, showWarnings = FALSE)
  expect_warning(write_survey(df, fileName), "'Data'")

  fname <- paste0("survey ", format(Sys.Date(), "%Y"), "EM.sav")
  dr <- read_survey(file.path(fileName, "Data", fname))

  expect_equal(df, dr)
  unlink(fileName, recursive = TRUE, force = TRUE)
})
