context("melt/gather and dcast/spread for Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score1" = c(9, 8), "Score2" = c(80, 70), stringsAsFactors = FALSE)

df_org <- survey_df(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
dt_org <- survey_dt(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
tb_org <- survey_tbl(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))

test_that("gather", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- tidyr::gather(x, var, score, -Q1)

    expect_is(x, "Survey")
    expect_identical(names(x), c("Q1", "var", "score"))
    expect_identical(x$data$score, c(9, 8, 80, 70))

  })

})

test_that("spread", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- tidyr::gather(x, var, score, -Q1)
    x <- tidyr::spread(x, var, score)

    expect_is(x, "Survey")
    expect_identical(names(x), c("Q1", "Score1", "Score2"))
    expect_equal(x$data, df_org$data)

  })

})

test_that("complete", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    lvls <- c("Example 1", "Example 2", "Example 3")
    x$data$Q1 <- factor(x$data$Q1, levels = lvls)
    x <- tidyr::complete(x, Q1)

    expect_is(x, "Survey")
    expect_identical(names(x), c("Q1", "Score1", "Score2"))
    expect_identical(as.character(x$data$Q1), lvls)

  })

})