#' @importFrom R6 R6Class
#' @export
Survey <- R6::R6Class("Survey",
  private = list(
    .associations = NULL,
    .labels = NULL,
    .config = NULL,
    .dictionary = NULL,
    .marketshares = NULL,

    update_labels = function() {
      "Update labels."
      cols <- self$names()
      new <- setNames(rep(NA_character_, length(cols)), cols)
      old <- private$.labels

      if (!is.null(old)) {
        old <- old[!is.na(old)]
        new <- merge_vectors(if (length(old)) old else NULL, new)
        new <- new[cols]
      }
      private$.labels <- new
    },

    update_associations = function() {
      "Update associations."
      cols <- self$names()
      new <- setNames(rep(NA_character_, length(cols)), cols)
      old <- private$.associations

      if (!is.null(old)) {
        old <- old[!is.na(old)]
        new <- merge_vectors(if (length(old)) old else NULL, new)
        new <- new[cols]
      }
      private$.associations <- new
    }
  ),

  public = list(

    data = NULL,

    initialize = function(x) {
      if (missing(x) || !is.data.frame(x))
        stop("Expecting a data.frame or data.table.", call = FALSE)
      if (is.labelled(x)) {
        x <- from_labelled(x, copy = FALSE)
        private$.labels <- attr(x, "labels")
      }
      self$data <- x
      self$update()
    },

    update = function() {
      "Update the survey. (Associations, labels, etc.)"
      private$update_associations()
      private$update_labels()
    },

    model = function() {
      "Return the measurement model"
      na <- rep(NA, ncol(self$data))

      x <- data.frame(
        "latent" = if (is.null(private$.assocations)) na else private$.associations,
        "manifest" = names(self$data),
        "question" = if (is.null(private$.labels)) na else private$.labels,
        "type" = vapply(self$data, function(x) class(x)[1], character(1)),
        "levels" = vapply(self$data, function(x) {
          l <- levels(x); if (is.null(l)) NA_character_ else stri_c(l, collapse = "\n")
        }, character(1)),
        stringsAsFactors = FALSE
      )

      structure(x, class = c("survey_model", "data.frame"))
    },

    names = function() {
      names(self$data)
    },

    print = function(...) {
      print(self$data)
    }
  )
)

# as/is ------------------------------------------------------------------------
#' @export
survey <- function(x) UseMethod("survey")

#' @export
is.survey <- function(x) inherits(x, "Survey")

#' @export
as.survey <- function(x) UseMethod("as.survey")

#' @export
as.survey.Survey <- function(x) x

#' @export
as.survey.default <- function(x) survey(x)

# Names ------------------------------------------------------------------------
#' @export
names.Survey <- function(x) names(x$data)

#' @export
dimnames.Survey <- function(x) {
  dimnames(x$data)
}

# Subset/alter -----------------------------------------------------------------
#' @export
`[.Survey` <- function(x, ...) {
  x$do("[", capture_dots(...))
}

#' @export
`[[.Survey` <- function(x, ...) {
  x$do("[[", capture_dots(...))
}

#' @export
`[<-.Survey` <- function(x, ...) {
  x$do("[<-", capture_dots(...), assign = TRUE)
}

#' @export
`[[<-.Survey` <- function(x, ...) {
  x$do("[[<-", capture_dots(...), assign = TRUE)
}

# Print ------------------------------------------------------------------------
#' @export
print.survey_model <- function(mm, width = getOption("width")) {

  cat("Measurement model\n")

  if (data.table::is.data.table(mm)) {
    mm <- data.table::copy(mm)
  } else {
    mm <- data.table::as.data.table(mm)
  }

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