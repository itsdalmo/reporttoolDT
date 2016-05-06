# splot <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE) {
#   stopifnot(is.survey(df))
#   if (length(groups) > 1L) stop("splot does not support multiple groups.")
#   out <- stable(df, vars, groups, weight, margin, wide = FALSE)
#   if ("proportion" %in% names(out)) {
#     splot_factor(out, vars, groups)
#   } else {
#     splot_numeric(out)
#   }
# }
#
# splot_factor <- function(x, vars, groups) {
#   out <- ggplot2::ggplot( data = x, ggplot2::aes_string(x = "value", y = "proportion", group = "variable"))
#   out <- out + ggplot2::geom_bar( stat = "identity", width = .5, position = ggplot2::position_dodge(width = .6))
#   out <- out + ggplot2::scale_y_continuous(labels = scales::percent)
#   # out <- out + ggplot2::geom_text()
#   out <- out + theme_epsi()
#   if (length(vars) > 1L) {
#     out <- out +  ggplot2::facet_wrap(~ variable, ncol=2L, scales="free_x")
#   }
#   out
# }

#' EPSI ggplot2 theme
#'
#' A baseline theme for plots.
#'
#' @param base_size Fontsize.
#' @param base_family Font family.
#' @author Kristian D. Olsen
#' @importFrom ggplot2 theme_gray theme element_rect element_text element_blank element_line
#' @name plots
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
    strip.text       = element_text(colour="white", vjust = 1L),
    strip.background = element_rect(fill="#23373b"),

    # Change plot margins
    panel.margin = grid::unit(2, "lines"), # facet margins
    plot.margin  = grid::unit(c(1, 0, 1, 0), "cm"),

    # Legend position and format
    legend.position = "bottom",
    legend.title    = element_blank()
  )
}

#' @rdname plots
#' @export
plot_shared_legend <- function(...) {

  plots <- list(...)

  grobs <- ggplotGrob(plots[[1]] + theme(legend.position = "bottom"))$grobs
  legends <- grobs[[which(lapply(grobs, function(x) x$name) == "guide-box")]]
  heights <- sum(legends$height)

  gridExtra::grid.arrange(
    do.call(gridExtra::arrangeGrob, lapply(plots, function(x) {
      x + theme(legend.position="none")})),
    legends,
    ncol = 1,
    heights = grid::unit.c(grid::unit(1, "npc") - heights, heights))

}
