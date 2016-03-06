context("melt/gather and dcast/spread for Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score 1" = c(9, 8), "Score 2" = c(80, 70), stringsAsFactors = FALSE)

df_org <- survey_df(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
dt_org <- survey_dt(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
tb_org <- survey_tbl(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))


test_that("gather", {

  # TODO

})

test_that("spread", {

  # TODO

})