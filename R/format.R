#' Entities (summary)
#'
#' This method produces a summary of the entities (total/valid observations and
#' marketshare) if they have been specified.
#' @param x A \code{Survey}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' df <- survey_df(data.frame("A" = "test", "B" = 2))
#'
#' # Regular
#' x <- set_association(df, mainentity = "A")
#' entities(x)
#'
#' # R6 (mutates)
#' df$set_association(mainentity = "A")
#' df$entities()

entities <- function(x) UseMethod("entities")

#' @rdname entities
#' @export
entities.Survey <- function(x) x$entities()

#' @rdname entities
#' @param ents The \code{survey_entities}, as returned by \code{entities()}.
#' @param ... Further arguments passed to \code{print}.
#' @export
print.survey_entities <- function(ents, ...) {
  cat("Entities\n")
  n <- nrow(ents); if (!n || is.null(n)) return()
  cat("Observations: ", n, "\n\n", sep = "")

  ents$entity <- as.character(ents$entity)
  res <- Map("c", ents, c("Total", colSums(ents[-1], na.rm = FALSE)))
  res$marketshare <- ifelse(is.na(res$marketshare), NA, stri_c(as.numeric(res$marketshare) * 100L, "%", sep = " "))

  # Add titles and format before print
  res <- Map("c", c("Entity", "Obs", "Valid", "Marketshare/Weight"), res)
  res <- Map("format", res, na.encode = TRUE, width = 8L, justify = "right")
  res <- do.call("stri_c", c(res, sep = " "))

  cat(res[1], "\n")
  cat(stri_pad("", width = stri_length(res[1]), pad = "-"), "\n")
  for (i in 2L:length(res)) { cat(res[i], "\n") }

}

#' Measurement model
#'
#' Return a summary of the data for the \code{Survey}. This includes labels and
#' associations, and the object (\code{survey_model}) prints nicely.
#' @param x A \code{Survey}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' df <- survey_df(data.frame("A" = 1, "B" = 2))
#'
#' # Regular
#' model(df)
#'
#' # R6
#' df$model()

model <- function(x) UseMethod("model")

#' @rdname model
#' @export
model.Survey <- function(x) x$model()

#' @rdname model
#' @param mm The \code{survey_model}, as returned by \code{model()}.
#' @param ... Further arguments passed to \code{print}.
#' @param width Restrict the width of the output (by truncating labels).
#' @export
print.survey_model <- function(mm, ..., width = getOption("width")) {
  cat("Measurement model\n")
  n <- nrow(mm); if (!n || is.null(n)) return()
  cat("Observations: ", n, "\n\n", sep = "")

  res <- stri_c(stri_c(format(1:n), ":"), format(mm$manifest), sep = " ")
  res <- stri_c(res, format(trunc_class(mm$type)), sep = " ")
  res <- stri_c(res, ifelse(is.na(mm$latent), " ", "*"))

  w <- width - stri_length(res[1L]) - 2L

  txt <- mm$question
  txt <- ifelse(stri_length(txt) > w, stri_c(stri_sub(txt, to = w - 3), "..."), txt)
  res <- stri_c(res, ifelse(is.na(txt), " ", txt), sep = " ")

  for (i in 1:n) {
    cat(res[i], "\n")
  }

  cat("Note: Associations (including latents) are marked with *\n")

}

# Truncate class names (similar to dplyr, used when printing survey_model.)
trunc_class <- function(x) {
  vapply(x, function(x) {
    switch(x,
           character = "(char)",
           factor = "(fctr)",
           numeric = "(num)",
           Date = "(date)",
           scale = "(scale)",
           integer = "(int)",
           "(????)")
  }, character(1))
}
