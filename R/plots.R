#' Bar and line charts
#'
#' These functions provide an easy way to visualize numeric or categorical data.
#' Uses \code{\link[tabulR]{qtable}} to aggregate the data, but only supports one
#' grouping variable.
#'
#' @inheritParams tabulR::qtable
#' @param wrap Optional: Call \code{\link[ggplot2]{facet_wrap}} on variables in \code{bar_chart}.
#' @param ... Additional parameters. Not used.
#' @author Kristian D. Olsen
#' @importFrom ggplot2 theme_gray theme element_rect element_text element_blank element_line
#' @name plots
#' @export
#' @examples
#' NULL

bar_chart <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  # Aggregate data to a long table using qtable (always wide = FALSE)
  out <- tabulR::qtable(df$data, vars, groups, weight, margin, wide = FALSE)
  pct <- "proportion" %in% names(out)

  # Create the plot
  out <- ggplot2::ggplot(
    data = out,
    ggplot2::aes_string(
      x     = if (pct) "value" else "variable",
      y     = if (pct) "proportion" else "value",
      ymin  = if (pct) 0 else min(out$value, na.rm = TRUE) * 0.8,
      ymax  = if (pct) 1.05 else max(out$value, na.rm = TRUE) * 1.2,
      group = groups,
      fill  = groups)
  )

  # Add geom
  out <- out + ggplot2::geom_bar(
    stat = "identity",
    width = .5,
    position = ggplot2::position_dodge(width = .6)
  )

  # Set y axis to percentages
  if (pct)
    out <- out + ggplot2::scale_y_continuous(labels = scales::percent)

  # Add labels to each bar
  out <- out + ggplot2::geom_text(
    ggplot2::aes(
      label    = if (pct) sprintf("%.0f %%", proportion * 100L) else sprintf("%.1f", value)),
      position = ggplot2::position_dodge(width = 0.6),
      vjust    = -1.1,
      hjust    = .35,
      size     = 3,
      colour   = "#23373b"
  )

  # Wrap if multiple variables.
  if (wrap && length(vars) > 1L)
    out <- out + ggplot2::facet_wrap(~ variable, ncol = 2L, scales="free_x")

  out + theme_epsi() + scale_fill_epsi()

}

#' @rdname plots
#' @export
line_chart <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, ...) {
  # Aggregate data to a long table using qtable (always wide = FALSE)
  out <- tabulR::qtable(df$data, vars, groups, weight, margin, wide = FALSE)
  if ("proportion" %in% names(df)) stop("Use bar_chart() to plot proportions.")

  # Create the plot
  out <- ggplot2::ggplot(
    data = out,
    ggplot2::aes_string(
      x     = "variable",
      y     = "value",
      ymin  = min(out$value) * 0.8,
      ymax  = max(out$value) * 1.1,
      group = groups,
      colour  = groups)
  )

  # Add geom line and point (dots)
  out <- out + ggplot2::geom_line(size = 1L) + ggplot2::geom_point(size = 3L)

  # Add labels to each line
  out <- out + ggplot2::geom_text(
    ggplot2::aes(
      label    = sprintf("%.1f", value)),
      size     = 3,
      colour   = "#23373b"
  )

  out + theme_epsi() + scale_colour_epsi()

}

#' @rdname plots
#' @export
latent_plot <- function(df, groups = NULL, weight = NULL, margin = TRUE) {
  vars <- names(df)[stri_trans_tolower(names(df)) %in% default_latents()]
  if (!length(vars)) stop("Latent variables were not found in the data.")
  if (is.null(groups)) {
    bar_chart(df, vars = vars, groups = groups, weight = weight, margin = margin)
  } else {
    line_chart(df, vars = vars, groups = groups, weight = weight, margin = margin)
  }

}

#' @rdname plots
#' @export
manifest_plot <- function(df, groups = NULL, weight = NULL, margin = TRUE) {
  # TODO
}

