#' EPSI ggplot theme
#'
#' A baseline theme for plots.
#'
#' @param base_size Fontsize.
#' @param base_family Font family.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{discrete_scale}}.
#' @author Kristian D. Olsen
#' @importFrom ggplot2 theme_gray theme element_rect element_text element_blank element_line
#' @name plot_themes
#' @export
#' @examples
#' NULL

theme_epsi <- function(base_size = 12, base_family = "sans") {
  out <- theme_gray(base_size = base_size, base_family = base_family)
  out + theme(
    # Use a transparent canvas/background for the plots.
    panel.background  = element_rect(fill = "transparent", colour = NA),
    plot.background   = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key        = element_rect(fill = "transparent", colour = NA),

    # Margins should also be transparent
    panel.border = element_rect(fill = "transparent", colour=NA),

    # Remove axis ticks.
    axis.ticks = element_blank(),

    # Format axis text size and color. Titles should be blank.
    axis.text.x  = element_text(size=9, colour="#23373b"),
    axis.text.y  = element_text(size=9, colour="#23373b"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),

    # Format the grid (only horizontal lines)
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour="#D0D0D0", size=.5),

    # Facet-title formatting
    strip.text       = element_text(colour="white"),
    strip.background = element_rect(fill="#23373b"),

    # Change plot margins
    panel.margin = grid::unit(1L, "cm"),
    plot.margin  = grid::unit(c(.5, .5, .5, .5), "cm"),

    # Legend position and format
    legend.position = "bottom",
    legend.title    = element_blank()
  )
}

#' @rdname plot_themes
#' @export
plot_shared_legend <- function(...) {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("'gridExtra' is required to create plots with shared legends.")
  }

  plots <- list(...)

  # Make sure only plots have been input.
  is_plot <- vapply(plots, inherits, what = "ggplot", logical(1))
  if (!all(is_plot))
    stop("Expecting all arguments to be ggplot objects.")

  # Create grob for "bottom" legend
  grobs <- ggplot2::ggplotGrob(plots[[1]] + theme(legend.position = "bottom"))
  grobs <- grobs$grobs

  # Get legends and heights
  legend <- which(lapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend]]
  height <- sum(legend$height)
  height <- grid::unit.c(grid::unit(1, "npc") - height, height)

  # Remove legends from original plots
  plots <- lapply(plots, function(x) { x + theme(legend.position = "none") })
  plots <- do.call(gridExtra::arrangeGrob, plots)

  # Return
  gridExtra::grid.arrange(plots, legend, ncol = 1, heights = height)

}

#' @rdname plot_themes
#' @export
scale_fill_epsi <- function(...) {
  ggplot2::discrete_scale("fill", "epsi", epsi_pal(), ...)
}

#' @rdname plot_themes
#' @export
scale_colour_epsi <- function(...) {
  ggplot2::discrete_scale("colour", "epsi", epsi_pal(), ...)
}

#' @rdname plot_themes
#' @export
scale_color_epsi <- scale_colour_epsi

epsi_pal <- function() {
  scales::manual_pal(unname(get_default("palette")))
}
