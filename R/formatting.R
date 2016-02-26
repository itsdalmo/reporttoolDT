entities <- function(x) UseMethod("model")
entities.Survey <- function(x) x$entities()

#' @export
print.survey_entities <- function(mm, width = getOption("width")) {
#   cat("Entities\n")
#
#   # Return early if it is empty
#   if (is.null(ents)) {
#     cat("Not specified (NULL). See help(add_entities)\n"); return()
#   }
#
#   # Print the number of observations
#   n <- nrow(ents); cat("Observations: ", n, "\n\n", sep = ""); if (!n) return()
#
#   # Return early if it contains no columnnames (obs = 0)
#   if (!ncol(ents)) {
#     cat("No columns\n"); return()
#   }
#
#   # Get the entities summary
#   ents <- data.table::copy(ents)
#   entt <- ents[, setNames(c("Total*", lapply(.SD, mean, na.rm = FALSE)), names(ents)), .SD = names(ents)[-1]]
#   ents <- rbind(ents, entt)
#
#   # Pad to match max length
#   w <- ents[, lapply(.SD, function(x) max(stri_length(x), na.rm = TRUE) + 4), .SDcols = c("entity", "n")]
#   ents[, marketshare := ifelse(!is.na(marketshare), sprintf("%.2f%%", marketshare*100), "")]
#   ents[, valid := stri_pad_right(sprintf("%.0f%%", (valid/n)*100), width = 9)]
#   ents[, entity := stri_pad_right(entity, width = w$entity)][, n := stri_pad_right(n, width = w$n)]
#
#   # Print headers for the table
#   cat(stri_pad_right("Entity", width = w$entity),
#       stri_pad_right("Obs", width = w$n),
#       stri_pad_right("Valid", width = 9),
#       "Marketshare/Weight\n", sep = "")
#
#   # Print results per entity
#   for (i in 1:nrow(ents)) {
#     cat(ents$entity[i], ents$n[i], ents$valid[i], ents$marketshare[i], sep = "", collapse = "\n")
#   }

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

trunc_class <- function(x) {
  vapply(x, function(x) {
    switch(x, character = "(char)", factor = "(fctr)", numeric = "(num)", Date = "(date)", scale = "(scale)", integer = "(int)", "(????)")
  }, character(1))
}
