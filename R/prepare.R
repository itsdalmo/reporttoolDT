#' Prepare data
#'
#' Prepare a \code{Survey} for PLS modelling, or get means for each latent.
#'
#' @param x A Survey.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

latents_pls <- function(x) {
  latents_impl(x, "pls")
}

#' @rdname latents_pls
#' @export
latents_mean <- function(x) {
  latents_impl(x, "mean")
}


latents_impl <- function(x, type) {
  stopifnot(is.survey(x))
  s <- class(x)[1L]
  x <- x$clone(deep = TRUE)$as_dt()

  cutoff <- get_config(x, "cutoff")
  if (is.null(cutoff) || is.na(cutoff))
    stop("'cutoff' must be set first. See help(set_config).")
  cutoff <- as.numeric(cutoff)

  vars <- x$get_association(which = default_latents())
  vars <- vars[match(names(x), vars, nomatch = 0L)] # Get vars in same order as data.
  if (!length(vars))
    stop("Latent associations must be set. See help(set_association).")

  # Check whether EM variables already exist. If so, overwrite.
  em <- c(stri_c(vars, "EM"))
  ex <- names(x)[stri_trans_tolower(names(x)) %in% stri_trans_tolower(em)]
  if (length(ex) == length(em))
    stop("All 'em' variables already exist in the data. Did you really mean to prepare data?")
  em <- c(em, ex)
  em <- em[!duplicated(stri_trans_tolower(em), fromLast = TRUE)]

  # Add "EM" variables. Calculate missing and add coderesp.
  x[, `:=`(em, lapply(.SD, clean_scale)), .SDcols = vars, with = FALSE]
  x[, `:=`(em, lapply(.SD, rescale_100)), .SDcols = em, with = FALSE]
  x[, percent_missing := rowMeans(is.na(.SD)), .SDcols = em]
  x[, coderesp := 1:.N]

  if (type == "pls") {
    x[, em := NULL, with = FALSE]
  } else if (type == "mean") {
    for (lat in default_latents()) {
      lat_var <- em[names(vars) == lat]
      if (length(lat_var)) {
        x[percent_missing <= cutoff,
          lat := rowMeans(.SD, na.rm = TRUE),
          .SDcols = lat_var, with = FALSE]
      }

    }
  }

  # Coerce back to input format.
  if (s == "Survey_df") {
    x <- x$as_df()
  } else if (s == "Survey_tbl") {
    x <- x$as_tbl()
  }

  # Set associations, update labels and return.
  x$set_association(percent_missing = "percent_missing")
  x

}