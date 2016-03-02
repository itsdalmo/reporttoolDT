context("dplyr methods for Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)

df_org <- survey_df(org)$set_association(list(mainentity = "Q1"))$set_label(list(Q1 = "test label"))
dt_org <- survey_dt(org)$set_association(list(mainentity = "Q1"))$set_label(list(Q1 = "test label"))
tb_org <- survey_tbl(org)$set_association(list(mainentity = "Q1"))$set_label(list(Q1 = "test label"))

test_dplyr <- function(x) {

  # Class
  expect_true(all(c("Survey", "R6") %in% class(x)))
  expect_true(any(c("Survey_df", "Survey_dt", "Survey_tbl") %in% class(x)))

  if ("Survey_df" %in% class(x))
    expect_identical(class(x$data), "data.frame")
  if ("Survey_dt" %in% class(x))
    expect_identical(class(x$data), c("data.table", "data.frame"))
  if ("Survey_tbl" %in% class(x))
    expect_is(x$data, c("tbl_df", "tbl_dt", "tbl"))

  # Association and label
  expect_identical(x$get_association("mainentity"), names(x)[1L])
  expect_identical(x$get_label(names(x)[1L]), "test label")
  expect_identical(names(x$get_association()), names(x))
  expect_identical(names(x$get_labels()), names(x))

}

# ------------------------------------------------------------------------------

test_that("mutate", {

  df <- dplyr::mutate(df_org, "test" = "df")
  dt <- dplyr::mutate(dt_org, "test" = "dt")
  tb <- dplyr::mutate(tb_org, "test" = "tb")

  expect_identical(df$test, rep("df", 2))
  expect_identical(dt$test, rep("dt", 2))
  expect_identical(tb$test, rep("tb", 2))

  expect_true("test" %in% names(df$get_association()))
  expect_true("test" %in% names(dt$get_association()))
  expect_true("test" %in% names(tb$get_association()))

})

test_that("select", {

  df <- dplyr::select(df_org, Q1)
  dt <- dplyr::select(dt_org, Q1)
  tb <- dplyr::select(tb_org, Q1)

  test_dplyr(df)
  test_dplyr(dt)
  test_dplyr(tb)

  expect_true(ncol(df) == 1L)
  expect_true(ncol(dt) == 1L)
  expect_true(ncol(tb) == 1L)

  expect_identical(names(df), "Q1")
  expect_identical(names(dt), "Q1")
  expect_identical(names(tb), "Q1")

})

test_that("filter", {

  df <- dplyr::filter(df_org, Score > 8)
  dt <- dplyr::filter(dt_org, Score > 8)
  tb <- dplyr::filter(tb_org, Score > 8)

  test_dplyr(df)
  test_dplyr(dt)
  test_dplyr(tb)

  expect_true(nrow(df) == 1L)
  expect_true(nrow(dt) == 1L)
  expect_true(nrow(tb) == 1L)

})

test_that("arrange", {

  df <- dplyr::arrange(df_org, Score)
  dt <- dplyr::arrange(dt_org, Score)
  tb <- dplyr::arrange(tb_org, Score)

  test_dplyr(df)
  test_dplyr(dt)
  test_dplyr(tb)

  expect_identical(df$data$Score, c("Example 2", "Example 1"))
  expect_identical(dt$data$Score, c("Example 2", "Example 1"))
  expect_identical(tb$data$Score, c("Example 2", "Example 1"))

})

test_that("group_by", {

  df <- dplyr::group_by(df_org, Q1)
  dt <- dplyr::group_by(dt_org, Q1)
  tb <- dplyr::group_by(tb_org, Q1)

  test_dplyr(df)
  test_dplyr(dt)
  test_dplyr(tb)

  expect_identical(as.character(dplyr::groups(df)), "Q1")
  expect_identical(as.character(dplyr::groups(dt)), "Q1")
  expect_identical(as.character(dplyr::groups(tb)), "Q1")

})

test_that("rename", {

  df <- dplyr::rename(df_org, entity = Q1)
  dt <- dplyr::rename(dt_org, entity = Q1)
  tb <- dplyr::rename(tb_org, entity = Q1)

  test_dplyr(df)
  test_dplyr(dt)
  test_dplyr(tb)

  expect_identical(names(df)[1], "entity")
  expect_identical(names(dt)[1], "entity")
  expect_identical(names(tb)[1], "entity")

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