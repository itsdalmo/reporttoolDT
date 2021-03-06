#' Entities (summary)
#'
#' This method produces a summary of the entities (total/valid observations and
#' marketshare) if they have been specified.
#' @param srv A \code{Survey}.
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

entities <- function(srv) UseMethod("entities")

#' @rdname entities
#' @export
entities.Survey <- function(srv) srv$entities()

#' @rdname entities
#' @param x The \code{survey_entities}, as returned by \code{entities()}.
#' @param ... Further arguments passed to \code{print}.
#' @export
print.survey_entities <- function(x, ...) {
  cat("Entities\n")
  n <- nrow(x); if (!n || is.null(n)) return()
  cat("Observations: ", n, "\n\n", sep = "")

  x$entity <- as.character(x$entity)
  res <- Map("c", x, c("Total", colSums(x[-1], na.rm = FALSE)))
  res$marketshare <- ifelse(is.na(res$marketshare), NA, sprintf("%.2f %%", as.numeric(res$marketshare) * 100L))

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
#' @param srv A \code{Survey}.
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

model <- function(srv) UseMethod("model")

#' @rdname model
#' @export
model.Survey <- function(srv) srv$model()

#' @rdname model
#' @param x The \code{survey_model}, as returned by \code{model()}.
#' @param ... Further arguments passed to \code{print}.
#' @param width Restrict the width of the output (by truncating labels).
#' @export
print.survey_model <- function(x, ..., width = getOption("width")) {
  cat("Measurement model\n")
  n <- nrow(x); if (!n || is.null(n)) return()
  cat("Observations: ", n, "\n\n", sep = "")

  res <- stri_c(stri_c(format(1:n), ":"), format(x$manifest), sep = " ")
  res <- stri_c(res, format(trunc_class(x$type)), sep = " ")
  res <- stri_c(res, ifelse(is.na(x$latent), " ", "*"))

  w <- width - stri_length(res[1L]) - 2L

  txt <- x$question
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
           POSIXct = "(date)",
           POSIXt = "(date)",
           scale = "(scale)",
           integer = "(int)",
           "(????)")
  }, character(1))
}
