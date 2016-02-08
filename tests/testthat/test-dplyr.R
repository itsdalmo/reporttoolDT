context("Using dplyr with a survey")

df <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(8, 9), stringsAsFactors = FALSE)
dt <- data.table::as.data.table(df)

df <- set_association(survey(df), mainentity = "Q1")
dt <- set_association(survey(dt), mainentity = "Q1")

check_dplyr <- function(x) {
  expect_true(all(c("labels", "associations", "translations", "config") %in% names(attributes(x))))
  expect_identical(names(attr(x, "associations")), names(x))
  expect_identical(names(attr(x, "labels")), names(x))
  expect_identical(get_association(x, "mainentity"), "Q1")
}

# ------------------------------------------------------------------------------

test_that("rename", {

  x <- dplyr::rename(df, entity = Q1)
  y <- dplyr::rename(dt, entity = Q1)

  expect_identical(class(x), c("survey_df", "survey", "tbl_df", "tbl", "data.frame"))
  expect_identical(x$entity, c("Example 1", "Example 2"))
  expect_true(all(c("labels", "associations", "translations", "config") %in% names(attributes(x))))
  expect_identical(names(attr(x, "associations")), names(x))
  expect_identical(names(attr(x, "labels")), names(x))
  expect_identical(get_association(x, "mainentity"), "entity")

  expect_identical(class(y), c("survey_dt", "survey", "tbl_dt", "tbl",  "data.table", "data.frame"))
  expect_identical(y$entity, c("Example 1", "Example 2"))
  expect_true(all(c("labels", "associations", "translations", "config") %in% names(attributes(y))))
  expect_identical(names(attr(y, "associations")), names(y))
  expect_identical(names(attr(y, "labels")), names(y))
  expect_identical(get_association(y, "mainentity"), "entity")

})

test_that("mutate", {

  x <- dplyr::mutate(df, "test" = "df")
  y <- dplyr::mutate(dt, "test" = "dt")

  expect_identical(x$test, rep("df", 2))
  expect_identical(y$test, rep("dt", 2))

  expect_identical(class(x), c("survey_df", "survey", "tbl_df", "tbl", "data.frame"))
  expect_identical(class(y), c("survey_dt", "survey", "tbl_dt", "tbl",  "data.table", "data.frame"))

  check_dplyr(x)
  check_dplyr(y)

})

test_that("select", {

  x <- dplyr::select(df, Q1)
  y <- dplyr::select(dt, Q1)

  expect_true(ncol(x) == 1L)
  expect_true(ncol(y) == 1L)

  expect_identical(names(x), "Q1")
  expect_identical(names(y), "Q1")

  expect_identical(class(x), c("survey_df", "survey", "tbl_df", "tbl", "data.frame"))
  expect_identical(class(y), c("survey_dt", "survey", "tbl_dt", "tbl",  "data.table", "data.frame"))

  check_dplyr(x)
  check_dplyr(y)

})

test_that("filter", {

  x <- dplyr::filter(df, Score > 8)
  y <- dplyr::filter(dt, Score > 8)

  expect_true(nrow(x) == 1L)
  expect_true(nrow(y) == 1L)

  expect_identical(class(x), c("survey_df", "survey", "tbl_df", "tbl", "data.frame"))
  expect_identical(class(y), c("survey_dt", "survey", "tbl_dt", "tbl",  "data.table", "data.frame"))

  check_dplyr(x)
  check_dplyr(y)

})

test_that("arrange", {

  x <- dplyr::mutate(df, Q1 = c("Test 2", "Test 1"))
  y <- dplyr::mutate(dt, Q1 = c("Test 2", "Test 1"))

  x <- dplyr::arrange(x, Q1)
  y <- dplyr::arrange(y, Q1)

  expect_identical(x$Score, c(9, 8))
  expect_identical(y$Score, c(9, 8))

  expect_identical(class(x), c("survey_df", "survey", "tbl_df", "tbl", "data.frame"))
  expect_identical(class(y), c("survey_dt", "survey", "tbl_dt", "tbl",  "data.table", "data.frame"))

  check_dplyr(x)
  check_dplyr(y)

})

test_that("group_by", {

  x <- dplyr::group_by(df, Q1)
  y <- dplyr::group_by(dt, Q1)

  expect_identical(as.character(dplyr::groups(x)), "Q1")
  expect_identical(as.character(dplyr::groups(x)), "Q1")

  expect_identical(class(x), c("survey_df", "survey", "grouped_df", "tbl_df", "tbl", "data.frame"))
  expect_identical(class(y), c("survey_dt", "survey", "grouped_dt", "tbl_dt", "tbl",  "data.table", "data.frame"))

  check_dplyr(x)
  check_dplyr(y)

})

test_that("bind_rows", {

  # TODO

})

test_that("bind_cols", {

  # TODO

})

test_that("left_join", {

  # TODO

})

test_that("gather", {

  # TODO

})

test_that("spread", {

  # TODO

})