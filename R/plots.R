#' Bar chart
#'
#' Create a bar chart to easily visualize numeric and/or categorical data. This function
#' is meant to be used with \code{\link[tabulR]{qtable}}, but only supports one
#' grouping variable.
#'
#' @inheritParams tabulR::qtable_
#' @param wrap Optional: Call \code{\link[ggplot2]{facet_wrap}} on variables in \code{bar_chart}.
#' @param ... Unquoted variable names passed to \code{\link[dplyr]{select}}.
#' @author Kristian D. Olsen
#' @importFrom ggplot2 theme_gray theme element_rect element_text element_blank element_line
#' @seealso line_chart
#' @export
#' @examples
#' NULL

bar_chart <- function(df, ..., groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("The NSE version of bar_chart requires dplyr.")

  # Use dplyr to select vars. Also, look for dplyr groups if not specified.
  vars <- dplyr::select_vars_(names(df), lazyeval::lazy_dots(...))
  groups <- groups %||% as.character(dplyr::groups(df))
  if (!length(groups)) groups <- NULL

  # Rename vars before creating table
  if (any(names(vars) != vars)) {
    if (data.table::is.data.table(df)) {
      data.table::setnames(df, unname(vars), names(vars))
    } else {
      names(df)[match(vars, names(df))] <- names(vars)
    }
  }

  bar_chart_(df, vars = names(vars), groups = groups, weight = weight, margin = margin, wrap = wrap)

}

#' @rdname bar_chart
#' @export
bar_chart_ <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  if (length(groups) > 1L)
    stop("bar_chart can only handle 1 grouping variable.")
  UseMethod("bar_chart_")
}

bar_chart_.default <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, wide = FALSE)
  bar_chart_impl(out, vars, groups, weight, margin, wrap)
}

bar_chart_.qtable <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  bar_chart_impl(df, vars, groups, weight, margin, wrap)
}

bar_chart_impl <- function(df, vars, groups, weight, margin, wrap) {
  pct <- "proportion" %in% names(df)

  # Create the plot
  out <- ggplot2::ggplot(
    data = df,
    ggplot2::aes_string(
      x     = if (pct) "value" else "variable",
      y     = if (pct) "proportion" else "value",
      ymin  = if (pct) 0 else min(df$value, na.rm = TRUE) * 0.8,
      ymax  = if (pct) 1.05 else max(df$value, na.rm = TRUE) * 1.2,
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

#' Line chart
#'
#' Create a line chart to easily visualize numeric data. This function
#' is meant to be used with \code{\link[tabulR]{qtable}}, but only supports one
#' grouping variable.
#'
#' @inheritParams bar_chart
#' @param wrap Ignored.
#' @author Kristian D. Olsen
#' @importFrom ggplot2 theme_gray theme element_rect element_text element_blank element_line
#' @seealso bar_chart
#' @export
#' @examples
#' NULL

line_chart <- function(df, ..., groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("The NSE version of bar_chart requires dplyr.")

  # Use dplyr to select vars. Also, look for dplyr groups if not specified.
  vars <- dplyr::select_vars_(names(df), lazyeval::lazy_dots(...))
  groups <- groups %||% as.character(dplyr::groups(df))
  if (!length(groups)) groups <- NULL

  # Rename vars before creating table
  if (any(names(vars) != vars)) {
    if (data.table::is.data.table(df)) {
      data.table::setnames(df, unname(vars), names(vars))
    } else {
      names(df)[match(vars, names(df))] <- names(vars)
    }
  }

  line_chart_(df, vars = names(vars), groups = groups, weight = weight, margin = margin, wrap = wrap)

}

#' @rdname line_chart
#' @export
line_chart_ <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  if (length(groups) > 1L)
    stop("line_chart can only handle 1 grouping variable.")
  UseMethod("line_chart_")
}

line_chart_.default <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, wide = FALSE)
  line_chart_impl(out, vars, groups, weight, margin, wrap)
}

line_chart_.qtable <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap = FALSE) {
  line_chart_impl(df, vars, groups, weight, margin, wrap)
}

line_chart_impl <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, wrap) {
  if ("proportion" %in% names(df)) stop("Use bar_chart() to plot proportions.")

  # Create the plot
  out <- ggplot2::ggplot(
    data = df,
    ggplot2::aes_string(
      x     = "variable",
      y     = "value",
      ymin  = min(df$value) * 0.8,
      ymax  = max(df$value) * 1.1,
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

#' @rdname latent_table
#' @export
latent_plot <- function(df, groups = NULL, weight = NULL, margin = TRUE) {
  vars <- names(df)[stri_trans_tolower(names(df)) %in% default_latents()]
  if (!length(vars)) stop("Latent variables were not found in the data.")
  if (is.null(groups)) {
    bar_chart_(df, vars = vars, groups = groups, weight = weight, margin = margin)
  } else {
    line_chart_(df, vars = vars, groups = groups, weight = weight, margin = margin)
  }

}

#' @rdname manifest_table
#' @export
manifest_plot <- function(df, groups = NULL, weight = NULL, margin = TRUE) {
  # TODO
}

