#' Latent plot
#'
#' Create a "latent profile" for one or more entities against the average.
#'
#' @param df A \code{Survey}.
#' @inheritParams tabulR::bar_chart_
#' @author Kristian D. Olsen
#' @seealso \code{\link{latent_table}} to create a table with latent scores.
#' @export
#' @examples
#' NULL

latent_plot <- function(df, groups = NULL, weight = NULL, margin = TRUE) {
  vars <- names(df)[stri_trans_tolower(names(df)) %in% default_latents()]
  if (is.null(groups) && requireNamespace("dplyr", quietly = TRUE)) {
    groups <- as.character(dplyr::groups(df))
    if (!length(groups)) groups <- NULL
  }
  if (!length(vars)) stop("Latent variables were not found in the data.")
  if (is.null(groups)) {
    out <- bar_chart_(df, vars = vars, groups = groups, weight = weight, margin = margin)
  } else {
    out <- line_chart_(df, vars = vars, groups = groups, weight = weight, margin = margin)
  }

  out + theme_epsi() + scale_fill_epsi()

}

#' Manifest plot
#'
#' Create a "swot" with manifest results for one or more entities against the average.
#'
#' @param df A \code{Survey}.
#' @inheritParams tabulR::bar_chart_
#' @author Kristian D. Olsen
#' @seealso \code{\link{manifest_table}} to create a table with manifest scores.
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
  out + theme_epsi() + scale_fill_epsi()
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

bar_chart_.Survey <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, margin_name = margin_name, wide = FALSE)
  out <- tabulR::bar_chart_(out, vars = vars, groups = groups, weight = weight, margin = margin, margin_name = margin_name, wrap = wrap)
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

line_chart_.Survey <- function(df, vars, groups = NULL, weight = NULL, margin = TRUE, margin_name = NULL, wrap = FALSE) {
  out <- tabulR::qtable_(df, vars = vars, groups = groups, weight = weight, margin = margin, margin_name = margin_name, wide = FALSE)
  out <- tabulR::line_chart_(out, vars = vars, groups = groups, weight = weight, margin = margin, margin_name = margin_name, wrap = wrap)
  out + theme_epsi() + scale_color_epsi()
}

#' Flow chart
#'
#' Create a flowchart to visualize latent scores and inner weights for the EPSI model.
#'
#' @param x A \code{Survey} or a \code{data.frame} with the inner weights.
#' @param entity Optional: If \code{x} is a \code{Survey}; the entity to create the flowchart for.
#' @param scores Optional: If \code{x} is not a \code{Survey}; the latent scores to plot.
#' @param width Width of the latent boxes. Default is \code{1.25}.
#' @param height Height of the latent boxes. Default is \code{1}.
#' @author Kristian D. Olsen
#' @seealso \code{\link{impact_table}}
#' @export
#' @examples
#' NULL

flow_chart <- function(x, entity = NULL, scores = NULL, width = 1.25, height = 1) {
  boxes <- flow_chart_boxes()
  palette <- default_palette()
  labels <- c(default_latents(), "complaints")

  if (is.survey(x)) {
    if (is.null(entity)) stop("When 'x' is a survey you must also specify the entity.", call. = FALSE)
    weight <- x$get_inner_weight(which = entity)
    if (is.null(weight)) stop("Could not find inner weight for ", entity, call. = FALSE)
    if (is.null(scores)) {
      me <- unname(x$get_association("mainentity"))
      scores <- latent_table(x, groups = me, wide = FALSE)
      scores <- scores[scores[[me]] == entity,]
      scores$variable <- stri_trans_tolower(scores$variable)
    }
    translation <- get_translation(x, which = labels)
    if (!is.null(translation)) labels <- unname(translation)
    x <- weight
  }

  # Convert coordinates to boxes using height and width.
  min_edges <- lapply(boxes, function(xy) { xy$x <- xy$x - width/2L; xy$y <- xy$y - height/2; xy })
  max_edges <- lapply(boxes, function(xy) { xy$x <- xy$x + width/2L; xy$y <- xy$y + height/2; xy })

  # Create the canvas with the correct theme -----------------------------------
  p <- ggplot2::ggplot(xmin = 0, xmax = 10, ymin = 0, ymax = 10) + ggplot2::xlim(0, 10) + ggplot2::ylim(0, 10)
  p <- p + theme_epsi() + scale_fill_epsi() + scale_color_epsi()
  p <- p + ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text = ggplot2::element_blank())

  # Draw arrows/lines ----------------------------------------------------------
  paths <- flow_chart_paths(boxes, width, height)
  for (origin in paths) {
    for (dest in origin) {
      # Add regular lines ------------------------------------------------------
      if (length(dest$x) > 2L && length(dest$y) > 2L) {
        p <- p + ggplot2::geom_path(
          ggplot2::aes_q(x = head(dest$x, -2L), y = head(dest$y, -2L)),
          size = .45, colour = "#22373b", linetype = dest$type %||% "solid")
        dest <- lapply(dest, tail, 2L)
      }
      # Add arrow --------------------------------------------------------------
      p <- p + ggplot2::geom_path(
        ggplot2::aes_q(x = dest$x, y = dest$y),
        arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed"),
        size = .45, colour = "#22373b", linetype = dest$type %||% "solid")
    }

  }

  # Add inner weights ----------------------------------------------------------
  wt <- flow_chart_weights()
  for (origin in names(wt)) {
    dest <- wt[[origin]]
    path <- list(x = unlist(lapply(dest, "[[", "x")), y = unlist(lapply(dest, "[[", "y")))
    weight <- x[x$origin == origin, names(x) %in% names(dest)]
    weight <- data.frame(w = t(weight)[, 1], x = path$x, y = path$y)
    weight$w <- stri_replace(sprintf("%.2f", weight$w), ",", fixed = ".")

    p <- p + ggplot2::geom_text(
      data = weight, ggplot2::aes_string(x = "x", y = "y", label = "w"),
      size = 3, colour = "#23373b"
      )

  }

  # Add boxes ------------------------------------------------------------------
  p <- p + ggplot2::geom_rect(
    ggplot2::aes_q(xmin = unlist(lapply(min_edges, "[[", "x")),
                   xmax = unlist(lapply(max_edges, "[[", "x")),
                   ymin = unlist(lapply(min_edges, "[[", "y")),
                   ymax = unlist(lapply(max_edges, "[[", "y"))),
    fill = head(palette, 1L))

  # Add labels -----------------------------------------------------------------
  p <- p + ggplot2::geom_text(
    ggplot2::aes_q(label = labels,
                   x     = unlist(lapply(boxes, "[[", "x")),
                   y     = unlist(lapply(boxes, "[[", "y"))),
    colour = "white", size = 3, vjust = -.7, fontface = "bold")

  # Add latent scores ----------------------------------------------------------
  p <- p + ggplot2::geom_text(
    ggplot2::aes_q(label = c(sprintf("%.1f", scores$value), "-"),
                   x     = unlist(lapply(boxes, "[[", "x")),
                   y     = unlist(lapply(boxes, "[[", "y"))),
    colour = "white", size = 3, vjust = 1.1)


  # Return
  p

}

