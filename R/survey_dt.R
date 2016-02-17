#' @export
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

#' @export
survey.data.table <- function(x) {
  if (is.labelled(x)) x <- from_labelled(x)
  survey_dt(x, copy = TRUE)
}

#' @export
as.list.survey_dt <- function(x, attributes = FALSE) {
  if (!attributes) return(NextMethod())

  if (is.null(get_association(x, "mainentity"))) {
    ents <- NULL
  } else {
    ents <- entities(x)
  }

  df <- data.table::as.data.table(data.table::copy(x))
  mm <- model(x)

  strip_attributes(df, which = default$attributes)
  structure(list("df" = df, "ents" = ents, "mm" = mm), class = c("survey_list", "list"))

}

#' @export
# rbind.survey_dt <- function(..., use.names = TRUE, fill = FALSE, idcol = NULL) {
#   survey_dt(NextMethod())
# }

#' @export
cbind.survey_dt <- function(...) {
  survey_dt(NextMethod())
}

#' @export
merge.survey_dt <- function(x, y, ...) {
  survey_dt(NextMethod())
}

#' @export
melt.survey_dt <- function(x, ...) {
  survey_dt(NextMethod())
}

#' @export
dcast.survey_dt <- function(x, ...) {
  survey_dt(NextMethod())
}

#' @export
model.survey_dt <- function(x) {
  x <- data.table::as.data.table(NextMethod())
  data.table::setattr(x, "class", c("survey_model", "data.table", "data.frame"))
  x
}

# DPLYR ------------------------------------------------------------------------
#' @export
tbl_vars.survey_dt <- function(x) NextMethod()

#' @export
groups.survey_dt <- function(x) NextMethod()

#' @export
ungroup.survey_dt <- function(x) NextMethod()

#' @export
group_by_.survey_dt <- function(x, ..., .dots, add = FALSE) {
  x <- dplyr::as.tbl(NextMethod())
  data.table::setattr(x, "class", unique(c("survey_dt", "survey", class(x))))
  x
}

#' @export
summarise_.survey_dt <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  data.table::setattr(x, "class", unique(c("survey_dt", "survey", class(x))))
  x
}

#' @export
arrange_.survey_dt <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  data.table::setattr(x, "class", unique(c("survey_dt", "survey", class(x))))
  x
}

#' @export
mutate_.survey_dt <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  data.table::setattr(x, "class", unique(c("survey_dt", "survey", class(x))))
  x
}

#' @export
select_.survey_dt <- function(x, ..., .dots) {
  x <- dplyr::tbl_dt(NextMethod())
  data.table::setattr(x, "class", unique(c("survey_dt", "survey", class(x))))
  x
}

#' @export
rename_.survey_dt <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  data.table::setattr(x, "class", unique(c("survey_dt", "survey", class(x))))
  x
}

#' @export
filter_.survey_dt <- function(x, ..., .dots) {
  x <- dplyr::as.tbl(NextMethod())
  data.table::setattr(x, "class", unique(c("survey_dt", "survey", class(x))))
  x
}

