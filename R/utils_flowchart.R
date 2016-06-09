# Return default center-points for flow_chart latent boxes ---------------------
flow_chart_boxes <- function() {
  # Coordinates for the boxes of the boxes (Both axis span from 0 to 10)
  list(image  = list(x = 1.50, y = 9.00),
       expect = list(x = 1.50, y = 6.60),
       prodq  = list(x = 1.50, y = 4.10),
       servq  = list(x = 1.50, y = 1.60),
       value  = list(x = 4.00, y = 6.35),
       epsi   = list(x = 6.50, y = 6.35),
       loyal  = list(x = 9.00, y = 6.35),
       compl  = list(x = 7.75, y = 2.70))
}

# x/y-Positions where inner weights will be plotted in flow_chart --------------
flow_chart_weights <- function() {
  list(
    image  = list(
      expect = c(x = 1.75, y = 7.80),
      prodq  = c(x = 0.50, y = 4.35),
      servq  = c(x = 0.50, y = 1.30),
      epsi   = c(x = 4.90, y = 7.80)
    ),
    expect = list(
      prodq  = c(x = 1.75, y = 5.35),
      servq  = c(x = 0.50, y = 1.91)
    ),
    prodq  = list(
      servq  = c(x = 1.75, y = 2.85),
      value  = c(x = 2.65, y = 5.40),
      epsi   = c(x = 4.90, y = 5.35)
    ),
    servq  = list(
      value  = c(x = 2.75, y = 3.80),
      epsi   = c(x = 4.90, y = 3.90)
    ),
    value = list(
      epsi   = c(x = 5.25, y = 6.60)
    ),
    epsi  = list(
      loyal  = c(x = 7.75, y = 6.60)
    )
  )
}


# Paths drawn in flow_chart ----------------------------------------------------

flow_chart_paths <- function(boxes, width, height) {
  w <- width/2L; h <- height/2L
  list(
    image  = list(
      expect = list(x = c(boxes$image$x, boxes$expect$x),
                    y = c(boxes$image$y - h, boxes$expect$y + h)),
      prodq  = list(x = c(boxes$image$x - w, rep(.25, 4L), boxes$prodq$x - w),
                    y = c(rep(boxes$image$y, 3L), rep(boxes$prodq$y, 3L))),
      servq  = list(x = c(boxes$image$x - w, rep(.25, 4L), boxes$servq$x - w),
                    y = c(rep(boxes$image$y, 3L), rep(boxes$servq$y - .15, 3L))),
      epsi   = list(x = c(boxes$image$x + w, boxes$epsi$x),
                    y = c(boxes$image$y, boxes$epsi$y + h))),
    expect = list(
      prodq  = list(x = c(boxes$expect$x, boxes$prodq$x),
                    y = c(boxes$expect$y - h, boxes$prodq$y + h)),
      servq  = list(x = c(boxes$expect$x - w, rep(.1, 4L), boxes$servq$x - w),
                    y = c(rep(boxes$expect$y, 3L), rep(boxes$servq$y + .15, 3L)))),
    prodq  = list(
      servq  = list(x = c(boxes$prodq$x, boxes$servq$x),
                    y = c(boxes$prodq$y - h, boxes$servq$y + h)),
      value  = list(x = c(boxes$prodq$x + w, boxes$value$x - w),
                    y = c(boxes$prodq$y, boxes$value$y)),
      epsi   = list(x = c(boxes$prodq$x + w, boxes$epsi$x - w),
                    y = c(boxes$prodq$y, boxes$epsi$y))),
    servq  = list(
      value  = list(x = c(boxes$servq$x + w, boxes$value$x),
                    y = c(boxes$servq$y, boxes$value$y - h)),
      epsi   = list(x = c(boxes$servq$x + w, boxes$epsi$x),
                    y = c(boxes$servq$y, boxes$epsi$y - h))),
    value  = list(
      epsi   = list(x = c(boxes$value$x + w, boxes$epsi$x - w),
                    y = c(boxes$value$y, boxes$epsi$y))),
    epsi   = list(
      loyal  = list(x = c(boxes$epsi$x + w, boxes$loyal$x - w),
                    y = c(boxes$epsi$y, boxes$loyal$y)),
      compl  = list(x = c(boxes$epsi$x, boxes$compl$x),
                    y = c(boxes$epsi$y - h, boxes$compl$y + h),
                    type = "dashed")),
    compl  = list(
      loyal  = list(x = c(boxes$compl$x, boxes$loyal$x),
                    y = c(boxes$compl$y + h, boxes$loyal$y - h),
                    type = "dashed"))
  )
}

