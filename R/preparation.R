#' Add contrast
#'
#' This function adds one or more contrast to a \code{Survey}.
#'
#' @param x A Survey.
#' @param ... The contrasts (\code{data.frame} or \code{Survey}) to add.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

add_contrast <- function(x, ...) {
  stopifnot(is.survey(x))

  dots <- list(...)
  out <- x$clone(deep = TRUE)

  me <- unname(get_association(x, "mainentity"))
  if (is.null(me)) {
    stop("'mainentity' must be specified in associations.", call. = FALSE)
  }

  has_me <- vapply(dots, function(df) me %in% names(df), logical(1))
  if (!all(has_me)) {
    stop("'mainentity' must be present in all data.frame's used as contrast.", call. = FALSE)
  }

  # Extract factor levels
  levs <- lapply(c(list(out), dots), function(df) {
    var <- df[[me]]; if (is.factor(var)) levels(droplevels(var)) else unique(var)
  })
  levs <- unique(unlist(levs))

  # Bind rows in turn
  for (df in dots) {
    out <- R6Frame::rbind(out, df)
  }

  out[[me]] <- factor(out[[me]], levels = levs)
  out$update()

}

#' Add weight
#'
#' Create a weight variable to a survey, based on marketshares. Values will be
#' replaced if 'weight' is set in associations. If not, a new variable 'w' is created.
#' Weights are only set for valid observations, non-valid will be set to \code{NA}.
#'
#' @param x A Survey.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

add_weight <- function(x) {
  stopifnot(is.survey(x))
  out <- x$clone(deep = TRUE)$as_dt()
  ents <- out$entities() # Throws errors if necessary information is missing.

  missing_valid <- any(is.na(ents$valid))
  if (missing_valid) {
    stop("Cannot calculate valid observations. Do set_config(cutoff = ...) and
         set_association(percent_missing = ...) first.")
  }

  missing_ms <- any(is.na(ents$marketshare))
  if (missing_ms) {
    stop("Marketshares must be set for all entities. See help(set_marketshare).")
  }

  # Calculate a weight for each entity based on valid observations and ms.
  ents <- data.table::as.data.table(ents)
  ents[, total := sum(valid)][, wt := (marketshare*total)/valid]

  # Replace existing weights with the newly calculated ones.
  pm <- out$get_association("percent_missing")
  co <- out$get_config("cutoff")
  me <- out$get_association("mainentity")

  wt <- out$get_association("weight") %||% "w"
  if (wt %in% names(out)) out[, wt := NULL, with = FALSE]
  out[, wt := NA_real_, with = FALSE]  # Set 'w' or weight variable.

  is_valid <- out[[pm]] <= as.numeric(co)
  for (i in seq_along(ents$entity)) {
    current_entity <- out[[me]] == ents$entity[i]
    rows <- which(is_valid & current_entity)
    data.table::set(out$data, rows, wt, ents$wt[i])
  }

  # Set association and label
  out$set_association(weight = wt)
  out$set_label(.list = setNames(list("weight"), wt))

  # Coerce back to input format.
  if (!data.table::is.data.table(x$data))
    out$as_df()
  if (inherits(x$data, "tbl"))
    out$as_tbl()

  out$update()

}

#' Add latent spreads
#'
#' This function adds latent-spreads to a \code{Survey}.
#'
#' @param x A Survey.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

add_latent_spread <- function(x) {
  stopifnot(is.survey(x))
  out <- x$clone(deep = TRUE)$as_dt()

  # Figure out which latents
  lats <- names(out)[stri_trans_tolower(names(out)) %in% default_latents()]
  lats_spread <- stri_c(lats, "spread", sep = "_")
  out[, lats_spread := lapply(.SD, spread_100), .SDcols = lats, with = FALSE]

  # Coerce back to input format.
  if (!data.table::is.data.table(x$data))
    out$as_df()
  if (inherits(x$data, "tbl"))
    out$as_tbl()

  out$set_label(.list = setNames(stri_c(get_label(x, lats), " (spread)"), lats_spread))
  out$update()

}

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
  out <- x$clone(deep = TRUE)$as_dt()

  cutoff <- out$get_config("cutoff")
  if (is.null(cutoff) || is.na(cutoff))
    stop("'cutoff' must be set first. See help(set_config).")
  cutoff <- as.numeric(cutoff)

  vars <- out$get_association(which = default_latents())
  vars <- vars[match(names(out), vars, nomatch = 0L)] # Get vars in same order as data.
  if (!length(vars))
    stop("Latent associations must be set. See help(set_association).")

  # Check whether EM variables already exist. If so, overwrite.
  em <- c(stri_c(vars, "EM"))
  ex <- names(out)[stri_trans_tolower(names(out)) %in% stri_trans_tolower(em)]
  if (length(ex) == length(em))
    stop("All 'em' variables already exist in the data. Did you really mean to prepare data?")
  em <- c(em, ex)
  em <- em[!duplicated(stri_trans_tolower(em), fromLast = TRUE)]

  # Add "EM" variables. Calculate missing and add coderesp.
  out[, `:=`(em, lapply(.SD, clean_scale)), .SDcols = vars, with = FALSE]
  out[, `:=`(em, lapply(.SD, rescale_100)), .SDcols = em, with = FALSE]
  out[, percent_missing := rowMeans(is.na(.SD)), .SDcols = em]
  out[, coderesp := as.numeric(1:.N)]

  if (type == "pls") {
    out[, em := NULL, with = FALSE]
  } else if (type == "mean") {
    for (lat in default_latents()) {
      lat_var <- em[names(vars) == lat]
      if (length(lat_var)) {
        out[percent_missing <= cutoff,  lat := rowMeans(.SD, na.rm = TRUE),  .SDcols = lat_var, with = FALSE]
        # out[is.nan(out[[lat]]), lat := NA, with = FALSE]
        # data.table::set(out$data, i = is.nan(out$data[[lat]]), j = lat, value = NA)
      }

    }
  }

  # Coerce back to input format.
  if (!data.table::is.data.table(x$data))
    out$as_df()
  if (inherits(x$data, "tbl"))
    out$as_tbl()

  # Set type in config, associations, update labels and return.
  out$set_association(percent_missing = "percent_missing")
  out$set_config(model = type)
  out$set_label(.auto = TRUE)

  out$update()

}