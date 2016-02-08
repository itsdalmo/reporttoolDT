#' @export
tbl_vars.survey <- function(x) NextMethod()

#' @export
groups.survey <- function(x) NextMethod()

#' @export
ungroup.survey <- function(x) NextMethod()

#' @export
group_by_.survey <- function(x, ..., .dots, add = FALSE) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

#' @export
summarise_.survey <- function(x, ..., .dots) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

#' @export
arrange_.survey <- function(x, ..., .dots) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

#' @export
mutate_.survey <- function(x, ..., .dots) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

#' @export
select_.survey <- function(x, ..., .dots) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}

#' @export
rename_.survey <- function(x, ..., .dots) {
  old <- names(x)
  x <- NextMethod()
  update_survey_names(x, old, names(x))
  x
}

#' @export
filter_.survey <- function(x, ..., .dots) {
  o <- get_attributes(x, which = default$attributes)
  x <- NextMethod()
  update_survey_attributes(x, old = list(o))
  x
}
