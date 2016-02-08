#' @export
survey_df <- function(x) {
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }

  # Update attributes
  o <- get_attributes(x, which = default$attributes)
  update_survey_attributes(x, old_attributes = list(o))
  class(x) <- c("survey_df", "survey", "data.frame")
  x
}

#' @export
survey.data.frame <- function(x) {
  if (is.labelled(x)) x <- from_labelled(x)
  survey_df(x)
}

#' @export
as.list.survey_df <- function(x, attributes = FALSE) {
  if (!attributes) return(NextMethod())

  if (is.null(get_association(x, "mainentity"))) {
    ents <- NULL
  } else {
    ents <- entities(x)
  }

  df <- as.data.frame(x)
  mm <- model(x)

  strip_attributes(df, which = default$attributes)
  structure(list("df" = df, "ents" = ents, "mm" = mm), class = c("survey_list", "list"))

}

#' @export
rbind.survey_df <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
  survey_df(NextMethod())
}

#' @export
cbind.survey_df <- function(...) {
  survey_df(NextMethod())
}

#' @export
merge.survey_df <- function(x, y, ...) {
  survey_df(NextMethod())
}

#' @export
melt.survey_df <- function(x, ...) {
  survey_df(NextMethod())
}

#' @export
dcast.survey_df <- function(x, ...) {
  survey_df(NextMethod())
}

#' @export
model.survey_df <- function(x) {
  NextMethod()
}

# DPLYR ------------------------------------------------------------------------
#' @export
tbl_vars.survey_df <- function(x) NextMethod()

#' @export
groups.survey_df <- function(x) NextMethod()

#' @export
ungroup.survey_df <- function(x) NextMethod()

#' @export
group_by_.survey_df <- function(x, ..., .dots, add = FALSE) {
  x <- dplyr::as.tbl(NextMethod())
  class(x) <- unique(c("survey_df", "survey", class(x)))
  x
}

#' @export
summarise_.survey_df <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  class(x) <- unique(c("survey_df", "survey", class(x)))
  x
}

#' @export
arrange_.survey_df <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  class(x) <- unique(c("survey_df", "survey", class(x)))
  x
}

#' @export
mutate_.survey_df <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  class(x) <- unique(c("survey_df", "survey", class(x)))
  x
}

#' @export
select_.survey_df <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  class(x) <- unique(c("survey_df", "survey", class(x)))
  x
}

#' @export
rename_.survey_df <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  class(x) <- unique(c("survey_df", "survey", class(x)))
  x
}

#' @export
filter_.survey_df <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  class(x) <- unique(c("survey_df", "survey", class(x)))
  x
}
