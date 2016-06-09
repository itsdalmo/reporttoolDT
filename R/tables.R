#' Latent scores
#'
#' \code{latent_table} is a convenience function similar to \code{\link{manifest_table}},
#' and uses \code{\link[tabulR]{qtable}} to generate a table of means (by groups) for
#' any PLS latents.
#'
#' @inheritParams qtable_.Survey
#' @inheritParams tabulR::qtable_
#' @seealso \code{\link{latent_plot}} to easily plot the latent scores.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' NULL

latent_table <- function(df, groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  stopifnot(is.survey(df))
  # Get variables by name of latents
  vars <- names(df)[stri_trans_tolower(names(df)) %in% default_latents()]
  if (!length(vars)) stop("Latent variables were not found in the data.")

  # Make the table and rename vars
  out <- tabulR::qtable_(df, vars, groups = groups, weight = weight, margin = margin, wide = wide)

  # Remove counts.
  if (data.table::is.data.table(out)) {
    out[, n := NULL]
  } else {
    out$n <- NULL
  }

  out
}

#' Manifest scores
#'
#' \code{manifest_table} is a convenience function similar to \code{\link{latent_table}},
#' and uses \code{\link[tabulR]{qtable}} to generate a table of means (by groups) for
#' variables associated with PLS latents. It uses EM variables to calculate the means,
#' but removes the EM suffix from variable names in the results. Variables in the results are
#' ordered by their latent association.
#'
#' @inheritParams qtable_.Survey
#' @inheritParams tabulR::qtable_
#' @author Kristian D. Olsen
#' @seealso \code{\link{manifest_plot}} to easily plot the manifest scores.
#' @export
#' @examples
#' NULL

manifest_table <- function(df, groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  stopifnot(is.survey(df))
  # Get variables from associations
  vars <- get_association(df, default_latents())
  if (!length(vars)) stop("Latent associations have not been set yet.")
  vars <- names(df)[match_all(stri_c(tolower(vars), "em"), tolower(names(df)))]
  if (!length(vars)) stop("No 'em' variables found in the data.")

  # Make the table and rename vars
  out <- tabulR::qtable_(df, vars, groups = groups, weight = weight, margin = margin, wide = wide)
  if (wide) {
    names(out) <- stri_replace(names(out), "$1", regex = "^([^-]*)em", case_insensitive = TRUE)
  } else {
    new <- levels(out$variable) %||% unique(out$variable)
    new <- setNames(new, stri_replace(new, "$1", regex = "^([^-]*)em", case_insensitive = TRUE))
    out$variable <- suppressWarnings(recode_(out$variable, dots = as.list(new), add = TRUE))
  }

  # Remove counts.
  if (data.table::is.data.table(out)) {
    out[, n := NULL]
  } else {
    out$n <- NULL
  }

  out
}

#' Using qtable_ with a Survey
#'
#' This is a S3 method for \code{\link[tabulR]{qtable}}. In addition to the standard
#' functionality in \code{qtable}, it also replaces variable names with their label
#' ("var: label") if a label exists. This is done for column names, and for the
#' \code{variable} column in a long table (\code{wide = FALSE}). When \code{margin = TRUE},
#' the table will use the "average" from \code{\link{set_translation}} for the margin.
#'
#' If you only want the regular \code{qtable} functionality, the recommended way
#' is to just call \code{qtable} on the survey data itself (\code{qtable(x$data)}
#' instead of \code{qtable(x)}).
#'
#' @inheritParams tabulR::qtable_
#' @param df A \code{Survey}.
#' @param weight Name of variable to weight by. Set this to \code{TRUE} to use the
#' 'weight' association in the Survey.
#' @author Kristian D. Olsen
#' @importFrom tabulR qtable qtable_
#' @export
#' @examples
#' NULL

qtable_.Survey <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wide = TRUE) {
  # Check dplyr::groups if groups is NULL and dplyr is installed.
  if (is.null(groups) && requireNamespace("dplyr", quietly = TRUE)) {
    groups <- dplyr::groups(df)
    if (!is.null(groups))
      groups <- as.character(groups)
  }

  # Get margin and weight variable from associations
  if (margin) margin_name <- margin_name %||% unname(get_translation(df, "average"))
  if (margin && isTRUE(weight)) weight <- unname(get_association(df, "weight"))

  out <- tabulR::qtable_(df$get_data(copy = TRUE),
                         vars = vars,
                         groups = groups,
                         weight = weight,
                         margin = margin,
                         margin_name = margin_name,
                         wide = wide)

  # If only one variable was specified, and there is no "variable" column and/or
  # the variable name is not in colnames - assume it has been spread. Use it's
  # label as title.
  if (length(vars) == 1L && !vars %in% names(out)) {
    title <- stri_c(stri_c(vars, ": "), get_label(df, vars))
    title <- stri_c(title, if (!is.null(weight)) " (Weighted)" else " (Unweighted)")
  } else {
    title <- NULL
  }

  # Include labels with variable names for wide/long tables.
  # (The "variable" column exists when creating table with mult. vars, or long tables.)
  if ("variable" %in% names(out)) {
    new <- get_label(df, unique(out$variable))
    if (!is.null(new) && !all(is.na(new))) {
      # Keep original name if it is not a latent.
      old <- levels(out$variable)
      old <- ifelse(stri_trans_tolower(old) %in% default_latents(), "", stri_c(old, " - "))
      new <- new[!is.na(new)]
      new <- setNames(names(new), stri_c(old, new))
      out$variable <- suppressWarnings(recode_(out$variable, dots = as.list(new), add = TRUE))
    }
  }

  # Columnnames should only be replaced when wide = TRUE. Else
  # we only replace variable (above).
  if (wide) {
    new <- get_label(df, names(out))
    if (!is.null(new) && !all(is.na(new))) {
      # Keep original name if it is not a latent.
      new <- new[!is.na(new)]
      old <- names(out)[names(out) %in% names(new)]
      old <- ifelse(stri_trans_tolower(old) %in% default_latents(), "", stri_c(old, " - "))
      names(out)[names(out) %in% names(new)] <- stri_c(old, new)
    }
  }

  # Return with title
  attr(out, "title") <- title
  out

}

#' Impact table
#'
#' summarise outer weights (impacts) for the EPSI model in a table.
#'
#' @param x A \code{Survey} or a \code{data.frame} with the outer weights.
#' @param entity Optional: If \code{x} is a \code{Survey}; the entity to create the flowchart for.
#' @param ... Ignored.
#' @author Kristian D. Olsen
#' @seealso \code{\link{flow_chart}}
#' @export
#' @examples
#' NULL

impact_table <- function(x, entity = NULL, ...) {
  if (is.survey(x)) {
    if (is.null(entity)) stop("When 'x' is a survey you must also specify the entity.", call. = FALSE)
    weight <- x$get_outer_weight(which = entity)
    if (is.null(weight)) stop("Could not find inner weight for ", entity, call. = FALSE)
    # Use labels from the survey (in case they have changed)
    weight$question <- unname(x$get_label(which = weight$manifest))
    x <- weight
  }

  x[order(x$effect, decreasing = TRUE), c("latent", "manifest", "question", "effect")]
}
