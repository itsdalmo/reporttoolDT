entities <- function(x) UseMethod("model")
entities.Survey <- function(x) x$entities()

#' @export
print.survey_entities <- function(mm, width = getOption("width")) {
  cat("Measurement model\n")
  n <- nrow(mm); if (!n || is.null(n)) return()
  cat("Observations: ", n, "\n\n", sep = "")

  res <- stri_c(stri_c(format(1:n), ":"), format(mm$manifest), sep = " ")
  res <- stri_c(res, format(trunc_class(mm$type)), sep = " ")
  w <- width - stri_length(res[1L]) - 2L

  txt <- mm$question
  txt <- ifelse(stri_length(txt) > w, stri_c(stri_sub(txt, to = w - 3), "..."), txt)
  res <- stri_c(res, ifelse(is.na(txt), " ", txt), sep = " ")

  for (i in 1:n) {
    cat(res[i], "\n")
  }

  cat("Note: Associations (including latents) are marked with *\n")

}

model <- function(x) UseMethod("model")
model.Survey <- function(x) x$model()

#' @export
print.survey_model <- function(mm, width = getOption("width")) {
  cat("Measurement model\n")
  n <- nrow(mm); if (!n || is.null(n)) return()
  cat("Observations: ", n, "\n\n", sep = "")

  res <- stri_c(stri_c(format(1:n), ":"), format(mm$manifest), sep = " ")
  res <- stri_c(res, format(trunc_class(mm$type)), sep = " ")
  w <- width - stri_length(res[1L]) - 2L

  txt <- mm$question
  txt <- ifelse(stri_length(txt) > w, stri_c(stri_sub(txt, to = w - 3), "..."), txt)
  res <- stri_c(res, ifelse(is.na(txt), " ", txt), sep = " ")

  for (i in 1:n) {
    cat(res[i], "\n")
  }

  cat("Note: Associations (including latents) are marked with *\n")

}

trunc_class <- function(x) {
  vapply(x, function(x) {
    switch(x, character = "(char)", factor = "(fctr)", numeric = "(num)", Date = "(date)", scale = "(scale)", integer = "(int)", "(????)")
  }, character(1))
}
