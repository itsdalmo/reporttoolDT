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

  structure(as.data.table(x), class = c("survey_mm", "data.table"))
}

#' @export
print.survey_mm <- function(mm, width = getOption("width")) {

  cat("Measurement model\n")

  # Print the number of observations
  n <- nrow(mm); cat("Observations: ", n, "\n\n", sep = ""); if (!n) return()

  # Limit string width
  w_n <- stri_length(nrow(mm))
  w_name <- max(stri_length(mm$manifest), na.rm = TRUE) + 1
  w_reserved <- 8 + w_name + 3 # $ and three spaces as separation
  w_available <- width - w_n - w_reserved - 5

  # Type
  mm$type <- vapply(mm$type, function(x) {
    x <- ifelse(is.na(x), "miss", x)
    switch(x, character = "(char)", factor = "(fctr)", numeric = "(num)", Date = "(date)", scale = "(scale)", integer = "(int)", "(????)")
    }, character(1))

  mm$type <- ifelse(!is.na(mm$latent), stri_c(mm$type, "*"), mm$type)

  # Clean manifest/type
  mm$manifest <- vapply(mm$manifest, stri_pad_right, width = w_name, character(1))
  mm$type <- vapply(mm$type, stri_pad_right, width = 8, character(1))

  # Shorten question-text to the remaining width
  mm$question[is.na(mm$question)] <- ""
  mm$question <- vapply(mm$question, stri_sub, to = w_available, character(1))

  # Print
  for (i in 1:nrow(mm)) {
    cat(stri_pad_right(i, w_n), ": ", mm$manifest[i], mm$type[i], " ", mm$question[i], sep = "", collapse = "\n")
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
