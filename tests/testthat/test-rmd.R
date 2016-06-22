context("Rmarkdown/Generate report")

md <- readLines("rmd.Rmd", encoding = "UTF-8")

test_that("eval_block works for Markdown", {
  rmd <- structure(list(content = "Inline example `r 0+1`"), class = c("markdown", "character"))
  res <- eval_block(rmd, env = parent.frame())
  expect_identical(res, "Inline example 1")
})

test_that("eval_block works for code chunks", {
  # Chunks are vectors (not multiline strings)
  # Returns a list (chunks can have multiple return values. E.g., when creating plots in a loop.)
  chunk <- c("```{r}", "res <- 1+1", "cat(\"Chunk example\", res)", "```")
  rmd <- structure(list(content = chunk), class = c("chunk", "character"))
  res <- eval_block(rmd, env = parent.frame())
  expect_identical(unlist(res), "Chunk example 2")
})

test_that("split_file manages to split .Rmd correctly", {
  # NOTE: split_file is not exported, and only does a "rough" splitting of the file.
  # This means that empty lines between blocks etc are marked as "markdown", which
  # is then handled in "eval_block".
  blk <- split_file(md)
  expect_identical(length(blk), 6L)
  expect_identical(
    vapply(blk, function(x) class(x)[1L], character(1)), # Classes
    c("yaml", "markdown", "chunk", "markdown", "chunk", "markdown") # Ref
    )
})

test_that("evaluate_rmd returns the expected output.", {
  res <- evaluate_rmd(md)
  expect_identical(length(res), 7L)
  expect_identical(which(vapply(res, is.character, logical(1))), c(1:6))
  expect_identical(class(res[[7]]), "recordedplot")
  expect_identical(unlist(res[c(2, 4, 6)]), c("## R Markdown", "## Slide with Bullets", "## Slide with R Output"))
})

test_that("We can create Powerpoint output from .Rmd", {
  skip_if_not_installed("ReporteRs")

  fileDir <- tempdir()
  render_ppt("rmd.Rmd", output = fileDir)

  expect_true(file.exists(file.path(fileDir, "rmd.pptx")))
  unlink(file.path(fileDir, "rmd.pptx"), recursive = TRUE, force = TRUE)
})