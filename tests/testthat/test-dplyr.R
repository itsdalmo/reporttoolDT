context("Regular dplyr methods for Survey")

org <- data.frame("Q1" = c("Example 1", "Example 2"), "Score" = c(9, 8), stringsAsFactors = FALSE)

df_org <- survey_df(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
dt_org <- survey_dt(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))
tb_org <- survey_tbl(org)$set_association(mainentity = "Q1")$set_label(list(Q1 = "test label"))

is_valid_survey <- function(x) {
  expect_true(
    # All dplyr verbs should return a Survey/data.frame.
    all(c("Survey", "R6") %in% class(x)) &&
    any(c("Survey_df", "Survey_dt", "Survey_tbl") %in% class(x)) &&
    is.data.frame(x$data) &&

    # Tests are written so that:
    # mainentity = Q1 (or whatever it is renamed to)
    # Q1 should have the label "test label"
    identical(x$get_association("mainentity"), setNames("mainentity", names(x)[1L])) &&
    identical(x$get_label(names(x)[1L]), setNames("test label", names(x)[1L]))
  )
}

# ------------------------------------------------------------------------------

test_that("mutate", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- dplyr::mutate(x, test = "val")
    is_valid_survey(x)

    expect_identical(x$data$test, rep("val", 2))
    expect_true("test" %in% names(x$get_association()))

  })

})

test_that("select", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- dplyr::select(x, Q1)
    is_valid_survey(x)

    expect_true(ncol(x) == 1L)
    expect_identical(names(x), "Q1")

  })

})

test_that("filter", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- dplyr::filter(x, Score > 8)
    is_valid_survey(x)

    expect_true(nrow(x) == 1L)

  })

})

test_that("arrange", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- dplyr::arrange(x, Score)
    is_valid_survey(x)

    expect_identical(x$data$Q1, c("Example 2", "Example 1"))

  })

})

test_that("group_by", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- dplyr::group_by(x, Q1)
    is_valid_survey(x)

    expect_identical(as.character(dplyr::groups(x)), "Q1")

  })

})

test_that("rename", {

  lapply(list(df_org, dt_org, tb_org), function(x) {

    x <- dplyr::rename(x, entity = Q1)
    is_valid_survey(x)

    expect_identical(names(x)[1], "entity")

  })

})

