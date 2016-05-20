#' Latent plot
#'
#' Create a "latent profile" for one or more entities against the average.
#'
#' @param df A \code{Survey}.
#' @inheritParams tabulR::bar_chart_
#' @author Kristian D. Olsen
#' @seealso latent_table
#' @export
#' @examples
#' NULL

latent_plot <- function(df, groups = NULL, weight = NULL, margin = TRUE) {
  vars <- names(df)[stri_trans_tolower(names(df)) %in% default_latents()]
  if (is.null(groups) && requireNamespace("dplyr", quietly = TRUE)) {
    groups <- as.character(dplyr::groups(df))
  }
  if (!length(vars)) stop("Latent variables were not found in the data.")
  if (is.null(groups)) {
    bar_chart_(df, vars = vars, groups = groups, weight = weight, margin = margin)
  } else {
    line_chart_(df, vars = vars, groups = groups, weight = weight, margin = margin)
  }

}

#' Manifest plot
#'
#' Create a "swot" with manifest results for one or more entities against the average.
#'
#' @param df A \code{Survey}.
#' @inheritParams tabulR::bar_chart_
#' @author Kristian D. Olsen
#' @seealso manifest_table
#' @export
#' @examples
#' NULL

manifest_plot <- function(df, groups = NULL, weight = NULL, margin = TRUE) {
  vars <- get_association(df, default_latents())
  vars <- names(df)[match_all(stri_trans_tolower(stri_c(vars, "em")), stri_trans_tolower(names(df)))]
  if (is.null(groups) && requireNamespace("dplyr", quietly = TRUE)) {
    groups <- as.character(dplyr::groups(df))
    if (!length(groups)) groups <- NULL
  }
  out <- manifest_table(df, groups = groups, weight = weight, margin = margin, wide = FALSE)
  out <- bar_chart_(out, vars = vars, groups = groups, weight = weight, margin = margin)
  out
}

#' Using bar_chart_ with a Survey
#'
#' This is a S3 method for \code{\link[tabulR]{bar_chart_}} which applies the
#' EPSI theme to the plot. See \code{\link{qtable_.Survey}} for additional information
#' on how they differ.
#'
#' @inheritParams tabulR::bar_chart_
#' @author Kristian D. Olsen
#' @importFrom tabulR bar_chart bar_chart_
#' @export
#' @examples
#' NULL

bar_chart_.Survey <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, wide = FALSE)
  out <- tabulR::bar_chart_(out, vars = vars, groups = groups, weight = weight, margin = margin, wrap = wrap)
  out + theme_epsi() + scale_fill_epsi()
}

#' Using line_chart_ with a Survey
#'
#' This is a S3 method for \code{\link[tabulR]{line_chart_}} which applies the
#' EPSI theme to the plot. See \code{\link{qtable_.Survey}} for additional information
#' on how they differ.
#'
#' @inheritParams tabulR::line_chart_
#' @author Kristian D. Olsen
#' @importFrom tabulR line_chart line_chart_
#' @export
#' @examples
#' NULL

line_chart_.Survey <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, wide = FALSE)
  out <- tabulR::line_chart_(out, vars = vars, groups = groups, weight = weight, margin = margin, wrap = wrap)
  out + theme_epsi() + scale_fill_epsi()
}

#' Flow chart
#'
#' Create a flowchart to visualize latent scores and inner weights for the EPSI model.
#'
#' @param x A \code{Survey}.
#' @param ... Ignored.
#' @author Kristian D. Olsen
#' @seealso impact_table
#' @export
#' @examples
#' NULL

flow_chart <- function(x, ...) {
  stopifnot(is.survey(x))
  # TODO
  stop("flow_chart has not been implemented yet.")
}
