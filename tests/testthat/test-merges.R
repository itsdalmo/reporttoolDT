context("binds/merges/joins for Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)

df_org <- survey_df(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
dt_org <- survey_dt(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
tb_org <- survey_tbl(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))


test_that("bind_rows", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- bind_rows(x, dplyr::mutate(x, test = "test"))

    expect_is(x, "Survey")
    expect_identical(names(x), c("Q1", "Score", "test"))
    expect_identical(x$data$test, c(NA, NA, "test", "test"))

  })

})

test_that("bind_cols", {

  x <- bind_cols(dt_org, dplyr::mutate(dt_org, test = "test"))

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- bind_cols(x, dplyr::mutate(x, test = "test"))

    expect_is(x, "Survey")
    expect_identical(x$get_association("mainentity"), setNames(c("mainentity", "mainentity"), c("Q1", "Q1")))
    expect_identical(x$get_label("Q1"), setNames(c("test label", "test label"), c("Q1", "Q1")))

  })

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