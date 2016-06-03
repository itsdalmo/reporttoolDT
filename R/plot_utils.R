# Return default center-points for flow_chart latent boxes ---------------------
flow_chart_boxes <- function() {
  # Coordinates for the boxes of the boxes (Both axis span from 0 to 10)
  list(image  = list(x = 1.50, y = 9.00),
       expect = list(x = 1.50, y = 6.60),
       prodq  = list(x = 1.50, y = 4.10),
       servq  = list(x = 1.50, y = 1.60),
       value  = list(x = 4.00, y = 6.10),
       epsi   = list(x = 6.50, y = 6.10),
       loyal  = list(x = 9.00, y = 6.10),
       compl  = list(x = 7.75, y = 2.00))
}

# Paths drawn in flow_chart ----------------------------------------------------
flow_chart_paths <- function() {
  list(
    image  = c("expect", "prodq", "servq", "epsi"),
    expect = c("prodq", "servq"),
    prodq  = c("servq", "value", "epsi"),
    servq  = c("value", "epsi"),
    value  = "epsi",
    epsi   = "loyal"
  )
}

# Find path between two latent boxes in flow_chart -----------------------------
# (By specifying e.g. detour_x, the line will travel vertically at this point on the x-axis.)
find_path <- function(from, to, boxes, width, height, detour_x = NULL, detour_y = NULL) {
  if (!is.null(detour_x) && !is.null(detour_y))
    stop("Cannot detour paths on both the x and y axis at the same time.")

  from_x <- boxes[[from]]$x
  from_y <- boxes[[from]]$y
  to_x <- boxes[[to]]$x
  to_y <- boxes[[to]]$y

  # Subtract/add width of the box on x-axis where appropriate.
  if (from_x != to_x || !is.null(detour_x)) {
    if (!is.null(detour_x)) {
      from_x <- if (detour_x < from_x) from_x - width/2L else from_x + width/2L
      to_x   <- if (detour_x < to_x) to_x - width/2L else to_x + width/2L

      from_x <- c(from_x, detour_x)
      to_x   <- c(detour_x, to_x)
    } else if (is.null(detour_y)) {
      from_x <- if (from_x < to_x) from_x + width/2L else from_x + width/2L
      to_x   <- if (from_x < to_x) to_x - width/2L else to_x + width/2L
    }
  }
  # Subtract/add height of the box on y-axis.
  if (from_y != to_y || !is.null(detour_y)) {
    if (!is.null(detour_y)) {
      from_y <- if (detour_y < from_y) from_y - height/2L else from_y + height/2L
      to_y   <- if (detour_y < to_y) to_y - height/2L else to_y + height/2L
      from_y <- c(from_y, detour_y)
      to_y   <- c(detour_y, to_y)
    } else if (is.null(detour_x)) {
      from_y <- if (from_y < to_y) from_y + height/2L else from_y + height/2L
      to_y   <- if (from_y < to_y) to_y - height/2L else to_y + height/2L
    }
  }

  if (!is.null(detour_x)) {
    from_y <- rep(from_y, 2L)
    to_y <- rep(to_y, 2L)
  } else if (!is.null(detour_y)) {
    from_x <- rep(from_x, 2L)
    to_x <- rep(to_x, 2L)
  }

  list(x = c(from_x, to_x), y = c(from_y, to_y))

}

# x/y-Positions where each weight will be plotted in flow_chart ----------------
flow_chart_weights <- function() {
  list(
    "x" = c("image_expect"   = 1.75,
            "image_prodq"    = 0.50,
            "image_servq"    = 0.50,
            "image_epsi"     = 4.90,
            "expect_prodq"   = 1.75,
            "expect_servq"   = 0.50,
            "prodq_servq"    = 1.75,
            "prodq_value"    = 2.65,
            "prodq_epsi"     = 4.90,
            "servq_value"    = 3.60,
            "servq_epsi"     = 4.90,
            "value_epsi"     = 5.25,
            "epsi_loyal"     = 7.75
    ),
    "y" = c("image_expect"   = 7.80,
            "image_prodq"    = 4.35,
            "image_servq"    = 1.30,
            "image_epsi"     = 7.80,
            "expect_prodq"   = 5.35,
            "expect_servq"   = 1.91,
            "prodq_servq"    = 2.85,
            "prodq_value"    = 5.40,
            "prodq_epsi"     = 5.20,
            "servq_value"    = 4.00,
            "servq_epsi"     = 3.75,
            "value_epsi"     = 6.30,
            "epsi_loyal"     = 6.30))
}

