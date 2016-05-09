#' Tables from surveys.
#'
#' \code{Survey}'s work best with the \code{\link[tabulR]{qtable}} function. In
#' addition to it's usual functionality, \code{qtable} used with a \code{Survey}
#' will use labels as columnames if possible. It also replaces variable names with their
#' label in long tables (in the column "variable").
#'
#' @inheritParams tabulR::qtable
#' @author Kristian D. Olsen
#' @name tables
#' @examples
#' NULL

#' @importFrom tabulR qtable qtable_
#' @export
qtable_.Survey <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  out <- tabulR::qtable_(df$data, vars = vars, groups = groups, weight = weight, margin = margin, wide = wide)

  # If only one variable was specified, and there is no "variable" column and/or
  # the variable name is not in colnames - assume it has been spread. Use it's
  # label as title.
  if (length(vars) == 1L && !vars %in% names(out)) {
    title <- stri_c(stri_c(vars, ": "), get_label(df, vars))
    title <- stri_c(title, if (!is.null(weight)) " (Weighted)" else " (Unweighted)")
  } else {
    title <- " "
  }

  # Use labels in "variable" column
  if ("variable" %in% names(out)) {
    new <- get_label(df, out$variable)
    if (!is.null(new)) {
      new <- new[!is.na(new)]
      new <- new[!duplicated(names(new))]
      new <- setNames(names(new), new)
      out$variable <- suppressWarnings(recode_(out$variable, dots = as.list(new), add = TRUE))
    }
  }

  # Also use labels as names for columns
  new <- get_label(df, names(out))
  if (!is.null(new)) {
    new <- new[!is.na(new)]
    names(out)[names(out) %in% names(new)] <-  stri_c(names(new), stri_c(": ", unname(new)))
  }

  # Return with title
  attr(out, "title") <- title
  out
}

#' @rdname tables
#' @export
latent_table <- function(df, groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  # Get variables by name of latents
  vars <- names(df)[stri_trans_tolower(names(df)) %in% default_latents()]
  if (!length(vars)) stop("Latent variables were not found in the data.")

  # Get weights in the same way
  if (margin) {
    weight <- get_association(df, "weight")
    if (is.null(weight))
      warning("'weight' is not specified in associations. Margin is unweighted.")
  } else {
    weight <- NULL
  }

  # Make the table and rename vars
  out <- tabulR::qtable_(df, vars, groups = groups, weight = weight, margin = margin, wide = wide)
  title <- stri_c("Latent scores", if (!is.null(weight)) " (Weighted)" else " (Unweighted)")

  # Remove counts.
  if (data.table::is.data.table(out)) {
    out[, n := NULL]
  } else {
    out$n <- NULL
  }

  # Return with title
  attr(out, "title") <- title
  out
}

#' @rdname tables
#' @export
manifest_table <- function(df, groups = NULL, weight = NULL, margin = TRUE, wide = TRUE) {
  # Get variables from associations
  vars <- get_association(df, default_latents())
  if (!length(vars)) stop("Latent associations have not been set yet.")
  vars <- names(df)[match_all(stri_c(tolower(vars), "em"), tolower(names(df)))]
  if (!length(vars)) stop("No 'em' variables found in the data.")

  # Get weights in the same way
  if (margin) {
    weight <- get_association(df, "weight")
    if (is.null(weight))
      warning("'weight' is not specified in associations. Margin is unweighted.")
  } else {
    weight <- NULL
  }

  # Make the table and rename vars
  out <- tabulR::qtable_(df, vars, groups = groups, weight = weight, margin = margin, wide = wide)
  names(out) <- stri_replace(names(out), "", regex = "em$")
  title <- stri_c("Manifest scores", if (!is.null(weight)) " (Weighted)" else " (Unweighted)")

  # Remove counts.
  if (data.table::is.data.table(out)) {
    out[, n := NULL]
  } else {
    out$n <- NULL
  }

  # Return with title
  attr(out, "title") <- title
  out
}