entities <- function(x) {
  stopifnot(is.survey(x))

  me <- get_association(x, "mainentity")
  if (is.null(me)) stop("'mainentity' has not been specified yet. See help(set_association).")

  x <- data.table::copy(x); setkeyv(x, me)
  co <- as.numeric(get_config(x, "cutoff"))
  ms <- get_marketshare(x)

  # Aggregate
  val <- !is.null(co) && "percent_missing" %in% names(x)
  x <- x[, list("n" = .N, "valid" = if (val) sum(percent_missing <= co) else NA), by = me]

  if (!is.null(ms)) {
    ms <- setNames(list(names(ms), unname(ms)), c(me, "marketshare"))
    ms <- as.data.table(ms)
    x[ms[, marketshare := as.numeric(marketshare)]]
  } else {
    x[, marketshare := NA]
  }

  structure(x, class = c("survey_ents", "data.table", "data.frame"))

}

print.survey_ents <- function(ents, width = getOption("width")) {

  cat("Entities\n")

  # Return early if it is empty
  if (is.null(ents)) {
    cat("Not specified (NULL). See help(add_entities)\n"); return()
  }

  # Print the number of observations
  n <- nrow(ents); cat("Observations: ", n, "\n\n", sep = ""); if (!n) return()

  # Return early if it contains no columnnames (obs = 0)
  if (!ncol(ents)) {
    cat("No columns\n"); return()
  }

  # Get the entities summary
  ents <- data.table::copy(ents)
  entt <- ents[, c(setNames("Total*", names(ents)[1]), lapply(.SD, sum, na.rm = FALSE)), .SD = c("n", "valid", "marketshare")]
  ents <- rbind(ents, entt)

  # Format the strings
  w_name <- ents[, max(stri_length(2), na.rm = TRUE), with = FALSE]
  w_name <- max(stri_length(ents[, 1]), na.rm = TRUE) + 4
  w_n <- max(stri_length(ents[, 2]), na.rm = TRUE) + 4

  # Pad
  ents[, c("valid", "marketshare") := list(sprintf("%.0f%%", (valid/n)*100),
                                           ifelse(!is.na(marketshare), sprintf("%.2f%%", marketshare*100), ""))]

  ents <- mutate(ents, valid = sprintf("%.0f%%", (valid/n)*100))
  ents <- mutate(ents, marketshare = sprintf("%.2f%%", marketshare*100))
  ents <- mutate(ents, entity = stri_pad_right(entity, width = w_name), n = stri_pad_right(n, width = w_n))
  ents <- mutate(ents, valid = stri_pad_right(valid, width = 9))

  # Print headers for the table
  cat(stri_pad_right("Entity", width = w_name),
      stri_pad_right("Obs", width = w_n),
      stri_pad_right("Valid", width = 9),
      "Marketshare/Weight\n", sep = "")

  # Print results per entity
  for (i in 1:nrow(ents)) {
    cat(ents$entity[i], ents$n[i], ents$valid[i], ents$marketshare[i], sep = "", collapse = "\n")
  }

}