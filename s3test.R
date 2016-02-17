# S3 -------------------------
rm(list = ls(all = TRUE))
x <- reporttool::read_data("./test.sav")

survey_dt <- function(x, copy = FALSE) {
  if (!requireNamespace("data.table")) {
    stop("data.table package required to use data tables", call. = FALSE)
  }
  if (data.table::is.data.table(x)) {
    if (copy)
      x <- data.table::copy(x)
  } else {
    x <- data.table::as.data.table(x)
  }

  structure(new_survey(x), class = c("survey_dt", "survey"))
}

survey_df <- function(x) {
  if (!is.data.frame(x))
    x <- as.data.frame(x)

  structure(new_survey(x), class = c("survey_df", "survey"))
}

new_survey <- function(x) {
  labs <- lapply(x, function(v) { a <- attr(v, "label"); if (is.null(v)) NA else a })
  list(
    .data = x,
    .associations = NULL,
    .labels = unlist(labs),
    .config = NULL,
    .dictionary = NULL
  )
}

survey <- function(x) UseMethod("survey")

survey.data.frame <- function(x) {
  # if (contains_labelled(x)) x <- convert_labelled(x)
  survey_df(x)
}

survey.data.table <- function(x) {
  # if (contains_labelled(x)) x <- convert_labelled(x)
  survey_dt(x)
}

`[.survey` <- function(x, ...) {
  `[`(x$.data, ...)
}

`[[.survey` <- function(x, ...) {
  args <- list(...)
  if (substr(args[[1]], 0, 1) == ".") {
    NextMethod()
  } else {
    `[[`(x$.data, ...)
  }
}

`$.survey` <- function(x, name) {
  `[[`(x, name)
}

`[<-.survey` <- function(x, i, j, value) {
  `[<-`(x$.data, i, j, value)
}

`[[<-.survey` <- function(x, i, j, value) {
  `[[<-`(x$.data, i, j, value)
}

names.survey <- function(x) names(x$.data)

print.survey <- function(x, ...) {
  print(x$.data)
}

y <- survey(x)
y <- survey(data.table::as.data.table(x))
