context("Survey/R6 accessors")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)
df_org <- survey_df(org)
dt_org <- survey_dt(org)
tb_org <- survey_tbl(org)


test_that("merge_attributes", {

  default <- c("a", "b", "c", "d")
  lst <- c("a" = 1, list(c("b" = 2), "d" = 4))
  res <- merge_attributes(default, lst)
  expect_identical(res, c("a" = 1, "b" = 2, "c" = NA, "d" = 4))

  lst[[2]] <- c(lst[[2]], "a" = "test")
  res <- merge_attributes(default, lst) # Mode ends up being character.
  expect_identical(res, c("a" = "1", "b" = "2", "c" = NA, "d" = "4"))

  expect_error(merge_attributes("a", lst = 1))
  expect_error(merge_attributes("a", lst = list(1)))

})

test_that("setting/getting label works", {

  df <- df_org$clone()$set_label(Q1 = "test")
  dt <- dt_org$clone()$set_label(Q1 = "test")
  tb <- tb_org$clone()$set_label(Q1 = "test")

  expect_identical(df$get_label("Q1"), c("Q1" = "test"))
  expect_identical(dt$get_label("Q1"), c("Q1" = "test"))
  expect_identical(tb$get_label("Q1"), c("Q1" = "test"))

})

test_that("setting/getting association works", {

  df <- df_org$clone()$set_association(mainentity = c("Q1", "Score"))
  dt <- df_org$clone()$set_association(mainentity = c("Q1", "Score"))
  tb <- df_org$clone()$set_association(mainentity = c("Q1", "Score"))

  expect_identical(df$get_association(), c("Q1" = "mainentity", "Score" = "mainentity"))
  expect_identical(dt$get_association(), c("Q1" = "mainentity", "Score" = "mainentity"))
  expect_identical(tb$get_association(), c("Q1" = "mainentity", "Score" = "mainentity"))

})

test_that("setting/getting marketshare works", {

  # Can't set markeshares without specifying mainentity first.
  expect_error(df_org$clone()$set_marketshare("Example 1" = .5, "Example 2" = .3))

  df <- df_org$clone()$set_association(mainentity = "Q1")$set_marketshare("Example 1" = .5)
  dt <- df_org$clone()$set_association(mainentity = "Q1")$set_marketshare("Example 1" = .5)
  tb <- df_org$clone()$set_association(mainentity = "Q1")$set_marketshare("Example 1" = .5)

  expect_identical(df$get_marketshare(), c("Example 1" = 0.5, "Example 2" = NA))
  expect_identical(dt$get_marketshare(), c("Example 1" = 0.5, "Example 2" = NA))
  expect_identical(tb$get_marketshare(), c("Example 1" = 0.5, "Example 2" = NA))

})

test_that("setting/getting config works", {

  # TODO

})

test_that("setting/getting translation works", {

  # TODO

})
