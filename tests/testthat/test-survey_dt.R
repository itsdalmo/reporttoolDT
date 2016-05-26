context("Base/Bracket methods for Survey_dt")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)

dummy_survey <- function(x) {
  x$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
  x
}

# Survey_dt --------------------------------------------------------------------
test_that(":= works with Survey_dt", {

  dt <- dummy_survey(survey_dt(org))
  dt[, test := "test"]

  expect_s3_class(dt, "Survey")
  expect_true("test" %in% dt$names())
  expect_true("test" %in% dt$get_association())

})

test_that("[<- works with Survey_dt", {

  dt <- dummy_survey(survey_dt(org))
  dt[, "test"] <- "test"

  expect_s3_class(dt, "Survey")
  expect_true("test" %in% dt$names())
  expect_true("test" %in% dt$get_association())

})

test_that("[[<- works with Survey_dt", {

  dt <- dummy_survey(survey_dt(org))
  dt[["test"]] <- "test"

  expect_s3_class(dt, "Survey")
  expect_true("test" %in% dt$names())
  expect_true("test" %in% dt$get_association())

})

test_that("[ works with Survey_dt", {

  dt <- dummy_survey(survey_dt(org))
  dt <- dt[, .(Q1)]

  expect_s3_class(dt, "Survey")
  expect_identical(names(dt), "Q1")

})

test_that("[[ works with Survey_dt", {

  dt <- dummy_survey(survey_dt(org))
  expect_identical(dt[["Score"]], c(9, 8))
  expect_identical(dt[, Score], c(9, 8))

})

test_that("names<- works with Survey_dt", {

  dt <- dummy_survey(survey_dt(org))
  names(dt) <- c("entity", "score")

  expect_s3_class(dt, "Survey")
  expect_identical(dt$get_label("entity"), setNames("test label", "entity"))
  expect_identical(dt$get_association("mainentity"), setNames("entity", "mainentity"))

})