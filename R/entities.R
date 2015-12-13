entities <- function(x) {
  stopifnot(is.survey(x))

  me <- get_association(x, "mainentity")
  if (is.null(me)) stop("'mainentity' has not been specified yet. See help(set_association).")

  x <- data.table::copy(x); setkeyv(x, me)
  co <- as.numeric(get_config(x, "cutoff"))

  # Aggregate
  val <- !is.null(co) && "percent_missing" %in% names(x)
  x <- x[, list("n" = .N, "valid" = if (val) sum(percent_missing <= co) else NA), by = me]

  ms <- get_marketshare(x)
  if (!is.null(ms)) {
    ms <- setNames(list(names(ms), unname(ms)), c(me, "marketshare"))
    ms <- as.data.table(ms)
    x <- x[ms[, marketshare := as.numeric(marketshare)]]
  } else {
    x[, marketshare := NA]
  }

  setkeyv(x, NULL)
  setnames(x, me, "entity")
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
  entt <- ents[, setNames(c("Total*", lapply(.SD, mean, na.rm = FALSE)), names(ents)), .SD = names(ents)[-1]]
  ents <- rbind(ents, entt)

  # Pad to match max length
  w <- ents[, lapply(.SD, function(x) max(stri_length(x), na.rm = TRUE) + 4), .SDcols = c("entity", "n")]
  ents[, marketshare := ifelse(!is.na(marketshare), sprintf("%.2f%%", marketshare*100), "")]
  ents[, valid := stri_pad_right(sprintf("%.0f%%", (valid/n)*100), width = 9)]
  ents[, entity := stri_pad_right(entity, width = w$entity)][, n := stri_pad_right(n, width = w$n)]

  # Print headers for the table
  cat(stri_pad_right("Entity", width = w$entity),
      stri_pad_right("Obs", width = w$n),
      stri_pad_right("Valid", width = 9),
      "Marketshare/Weight\n", sep = "")

  # Print results per entity
  for (i in 1:nrow(ents)) {
    cat(ents$entity[i], ents$n[i], ents$valid[i], ents$marketshare[i], sep = "", collapse = "\n")
  }

}

# Get/set for entities ---------------------------------------------------------

#' @export
get_marketshare <- function(srv, entities = NULL, arrange = TRUE) {
  x <- get_attr(srv, which = "marketshares", matches = entities, arrange = arrange, match_names = TRUE)
  x
}


#' @export
set_marketshare <- function(srv, ..., list = NULL) {
  srv <- data.table::copy(srv)
  # Update survey in case mainentity has just been set
  ms <- attr(srv, "marketshares")
  if (is.null(ms)) {
    setattr(srv, "marketshares", update_marketshares(srv, ms))
  }

  set_attr(srv, "marketshares", c(base::list(...), list), match_names = TRUE)
  srv
}