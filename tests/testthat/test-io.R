context("input/output for Survey's")

sav <- officeR::read_data(system.file("extdata", "raw_data.sav", package = "reporttoolDT"))

test_that("read/write survey to .sav", {
  df <- survey_tbl(sav)$set_association(common = TRUE)
  fileName <- file.path(tempdir(), "survey.sav")
  write_survey(df, fileName)
  dr <- read_survey(fileName)

  expect_equal(df, dr)
  unlink(fileName, recursive = TRUE, force = TRUE)
})

test_that("read/write survey to directory", {
  df <- survey_tbl(sav)$set_association(common = TRUE)$set_config(name = "survey")
  df[["pct_missing"]] <- 0; df[["coderesp"]] <- as.numeric(1:nrow(df))
  df$set_association(percent_missing = "pct_missing")

  fileName <- file.path(tempdir(), "survey")
  dir.create(fileName, showWarnings = FALSE)
  expect_warning(write_survey(df, fileName), "Cutoff had not been set.")
  fname <- paste0("survey ", format(Sys.Date(), "%Y"), ".sav")
  dr <- read_survey(file.path(fileName, "Data", fname))

  # Cutoff is set when writing. Update original.
  df$set_config(cutoff = .3)
  expect_equal(df, dr)

  unlink(fileName, recursive = TRUE, force = TRUE)
})
