context("Base/Bracket methods for Survey_df")

org <- data.frame(Q1 = c("Example 1", "Example 2"), Score = c(9, 8), stringsAsFactors = FALSE)

dummy_survey <- function(x) {
  x$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
  x
}

# Survey_df --------------------------------------------------------------------
test_that("[<- works with Survey_df", {

  df <- dummy_survey(survey_df(org))
  df[, "test"] <- "test"

  expect_s3_class(df, "Survey_df")
  expect_true("test" %in% df$names())
  expect_true("test" %in% df$get_association())

})

test_that("[[<- works with Survey_df", {

  df <- dummy_survey(survey_df(org))
  df[["test"]] <- "test"

  expect_s3_class(df, "Survey_df")
  expect_true("test" %in% df$names())
  expect_true("test" %in% df$get_association())

})

test_that("[ works with Survey_df", {

  df <- dummy_survey(survey_df(org))
  df <- df[, "Q1", drop = FALSE]

  expect_s3_class(df, "Survey_df")
  expect_identical(names(df), "Q1")

})

test_that("[[ works with Survey_df", {

  df <- dummy_survey(survey_df(org))
  expect_identical(df[["Score"]], c(9, 8))

})


test_that("names<- works with Survey_df", {

  df <- dummy_survey(survey_df(org))
  names(df) <- c("entity", "score")

  expect_s3_class(df, "Survey_df")
  expect_identical(df$get_label("entity"), setNames("test label", "entity"))
  expect_identical(df$get_association("mainentity"), setNames("entity", "mainentity"))

})

test_that("rbind works with Survey_df", {

  # TODO

})

test_that("cbind works with Survey_df", {

  # TODO

})

test_that("Merge works with Survey_df", {

  # TODO
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   z <- merge(y, x, by = "Q1")
#
#   check_df(z)

})

test_that("rbind works with multiple Survey objects", {

  # TODO
#   y <- survey(x)
#   y <- set_association(y, mainentity = "Q1")
#   y$test <- "test"
#
#   z <- set_association(y, "works" = "test")
#   z$extra <- "extra"
#
#   expect_error(rbind(y,z))
#   y$extra <- "ncol?"
#
#   z <- rbind(y, z)
#   expect_identical(attributes(z)$associations[3], setNames("works", "test"))
#
#   check_df(z)

})
