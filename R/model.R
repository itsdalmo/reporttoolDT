#' @export
model <- function(x) {
  stopifnot(is.survey(x))
  x <- list("latent" = get_association(x),
            "manifest" = names(x),
            "question" = get_labels(x),
            "type" = vapply(x, function(x) class(x)[1], character(1)),
            "levels" = vapply(x, function(x) {
              l <- levels(x); if (is.null(l)) NA_character_ else stri_c(l, collapse = "\n")
              }, character(1)))

  structure(as.data.table(x), class = c("survey_mm", "data.table", "data.frame"))
}

#' @export
print.survey_mm <- function(mm, width = getOption("width")) {

  cat("Measurement model\n")

  # Print the number of observations
  n <- nrow(mm); cat("Observations: ", n, "\n\n", sep = ""); if (!n) return()
  mm <- data.table::copy(mm)

  # Get string width limits
  w <- mm[, list(n = stri_length(.N), manifest = max(stri_length(manifest), na.rm = TRUE) + 1)]
  w[, reserved := 8 + manifest + 3] # $ and three spaces as seperation
  w[, available := width - n - reserved - 5]

  # Shorten name of 'type'
  mm[is.na(type), type := "miss"]
  mm[, type := vapply(type, function(x) {
    switch(x, character = "(char)", factor = "(fctr)", numeric = "(num)", Date = "(date)", scale = "(scale)", integer = "(int)", "(????)")
  }, character(1)) ]
  mm[!is.na(latent), type := stri_c(type, "*")]

  # Pad strings to correct width
  mm[, manifest := stri_pad_right(manifest, width = w$manifest)]
  mm[, type := stri_pad_right(type, width = 8)]
  mm[is.na(question), question := ""]
  mm[, question := stri_sub(question, to = w$available)]

  # Print
  for (i in 1:nrow(mm)) {
    cat(stri_pad_right(i, w$n), ": ", mm$manifest[i], mm$type[i], " ", mm$question[i], sep = "", collapse = "\n")
  }

  cat("Note: Associations (including latents) are marked with *\n")

}

# Get/set for measurement model ------------------------------------------------

get_association <- function(srv, associations = NULL, arrange = TRUE) {
  x <- get_attr(srv, which = "associations", matches = associations, arrange = arrange, match_names = FALSE)
  names(x)
}

#' @export
set_association <- function(srv, ..., list = NULL) {
  srv <- data.table::copy(srv)
  set_attr(srv, "associations", c(base::list(...), list), match_names = FALSE)
  srv
}

#' @export
get_labels <- function(srv, labels = NULL, arrange = TRUE) {
  x <- get_attr(srv, which = "labels", matches = labels, arrange, match_names = TRUE)
  unname(x)
}

#' @export
set_labels <- function(srv, ..., list = NULL) {
  srv <- data.table::copy(srv)
  set_attr(srv, "labels", c(base::list(...), list), match_names = TRUE)
  srv
}
