context("Base/Bracket methods for Survey_tbl")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)

dummy_survey <- function(x) {
  x$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
  x
}

# Survey_tbl --------------------------------------------------------------------
test_that("[<- works with Survey_tbl", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl[, "test"] <- "test"

  expect_is(tbl, "Survey_tbl")
  expect_true("test" %in% tbl$names())
  expect_true("test" %in% names(tbl$get_association()))

})

test_that("[[<- works with Survey_tbl", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  tbl[["test"]] <- "test"

  expect_is(tbl, "Survey_tbl")
  expect_true("test" %in% tbl$names())
  expect_true("test" %in% names(tbl$get_association()))

})

test_that("[ works with Survey_tbl", {

  tbl <- dummy_survey(survey_tbl(org))
  tbl <- tbl[, "Q1", drop = FALSE]

  expect_is(tbl, "Survey_tbl")
  expect_identical(names(tbl), "Q1")

})

test_that("[[ works with Survey_tbl", {

  tbl <- dummy_survey(survey_tbl(org))
  expect_identical(tbl[["Score"]], c(9, 8))

})

test_that("names<- works with Survey_tbl", {
  skip_if_not_installed("dplyr")

  tbl <- dummy_survey(survey_tbl(org))
  names(tbl) <- c("entity", "score")

  expect_is(tbl, "Survey_tbl")
  expect_identical(tbl$get_label("entity"), setNames("test label", "entity"))
  expect_identical(tbl$get_association("mainentity"), setNames("mainentity", "entity"))

})
