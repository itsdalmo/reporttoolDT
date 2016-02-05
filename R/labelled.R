#' Flatten SPSS input
#'
#' When reading SPSS files with \code{\link{read_data}}, se this function to convert
#' \code{labelled} to \code{factor} and remove additional attributes from the data.
#' The function returns a \code{list} (mm and df) and the additional information is stored in
#' \code{mm}. The process can be returned when writing spss files with \code{\link{write_data}}
#' (if changes to the data are reflected in the mm), but it is better to not avoid using this
#' function when doing light data cleaning and writing it after.
#'
#' @param df A data.frame as returned from \code{read_data} or \code{haven::read_sav}.
#' \code{data.frame} is returned from the function.
#' @author Kristian D. Olsen
#' @note The results are error-prone. Carefully check the results.
#' @export
#' @examples
#' read_data("test.sav") %>% from_labelled()

from_labelled <- function(df) UseMethod("from_labelled")

from_labelled.data.table <- function(df) {
  from_labelled_impl(data.table::copy(df))
}

from_labelled.data.frame <- function(df) {
  df <- data.table::as.data.table(df)
  as.data.frame(from_labelled_impl(df))
}

from_labelled.tbl_df <- function(df) {
  df <- data.table::as.data.table(df)
  dplyr::tbl_df(from_labelled_impl(df))
}

from_labelled_impl <- function(dt) {
  stopifnot(data.table::is.data.table(dt))

  # Preserve labels
  label <- lapply(dt, attr, which = "label")
  label <- unlist(lapply(label, function(x) { if(is.null(x)) NA else x }))

  # Differentiate between scale and factor variables
  #dt[, vapply(.SD, inherits, what = "labelled", logical(1))]
  labelled <- vapply(dt, inherits, what = "labelled", logical(1))
  labels <- lapply(dt, function(x) if (!is.null(attr(x, "labels"))) names(attr(x, "labels")) else attr(x, "levels"))
  scales <- vapply(labels, function(x) { sum(stri_detect(x, regex = default$pattern$detect_scale)) == 10L }, logical(1))

  # Check labelled scales for consistency and convert to factor
  cols <- names(dt)
  dt[, cols[scales] := Map(fix_labelled, .SD, names(.SD)), .SDcols = cols[scales], with = FALSE]
  dt[, cols[labelled] := lapply(.SD, haven::as_factor, drop_na = FALSE, ordered = FALSE), .SDcols = cols[labelled], with = FALSE]

  # Return
  data.table::setattr(dt, "labels", label)
  dt

}

#' Convert to labelled
#'
#' Reverses the process from \code{\link{from_labelled}}, and uses a measurement
#' model to create labelled variables and add \code{label} to the attributes of
#' each variable in the data. Meant for use with survey objects from \code{\link{survey}}.
#'
#' @param survey A survey object, or a list with data (df) and a measurement model (mm).
#' @author Kristian D. Olsen
#' @note The results are error-prone. Carefully check the results.
#' @export
#' @examples
#' read_data("test.sav") %>% from_labelled() %>% to_labelled()


to_labelled <- function(survey) {

  # Convert to factors/scales
  vars <- survey$mm$manifest[survey$mm$type %in% c("scale", "factor")]

  # Make sure all factor/scale variables are factors
  survey <- factor_data(survey, vars)

  # Convert variables
  survey$df[] <- lapply(names(survey$df), function(nm, df, mm) {

    x <- df[[nm]]

    # All factors should be 'labelled'
    if (is.factor(x)) {
      v <- levels(x)
      x <- as.numeric(x); x <- haven::labelled(x, setNames(as.numeric(1:length(v)), v), is_na = NULL)
    } else if (is.character(x)) {
      # Make sure encoding is native
      x <- collect_warnings(stri_enc_tonative(x))
      if (!is.null(x$warnings)) {
        warnings <- unlist(lapply(x$warnings, "[[", "message"))
        warning("Warnings when encoding ", nm, " to native:\n",
                stri_c(unique(warnings), collapse = "\n"), call. = FALSE)
      }
      x <- x$value
    }

    # Set attributes/class and return
    attr(x, "label") <- mm$question[mm$manifest %in% nm]
    x

  }, survey$df, survey$mm)

  # Return
  survey

}

# Utilities --------------------------------------------------------------------

fix_labelled <- function(x, nm) {

  labels <- attr(x, "labels")
  differ <- setdiff(unique(x[!is.na(x)]), labels)
  nm <- stri_c("In column ", nm, ": ")

  # If it has a 'do not know', fix
  if (length(differ)) {
    if (length(labels) > 10) {
      if (!differ %in% c(11, 98)) {
        warning(nm, "Assigned ", differ, " to label:\n", labels[length(labels)], call. = FALSE)
      }
      # Set last value to 'do not know'
      labels[length(labels)] <- differ

      # Assign the fixed labels
      attr(x, "labels") <- labels

    } else {
      warning(nm, differ, " has been set to NA.", call. = FALSE)
      x[x %in% differ] <- NA
    }
  }

  # Return
  x

}
