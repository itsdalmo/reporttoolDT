#' Export to netigate
#'
#' This function loops through the specified questionnaire and passes each question
#' to the clipboard, with the correct formatting for Netigate's magic import.
#'
#' @inheritParams write_questionnaire
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   q <- read_data("master questionnaire.xlsx", sheet = "questionnaires")
#'   export_netigate(q, study = "Banking", segment = "B2C", year = 2016)
#' }

export_netigate <- function(x, study = NULL, segment = NULL, year = NULL, entity = NULL) {
  blocks <- split_questionnaire(x, study, segment, year, entity, short_scale = FALSE)
  for (i in seq_along(blocks)) {
    x <- blocks[[i]]
    if (nrow(x) == 1L) {
      if (x$type[1] == "character") {
        res <- x$question
      } else {
        res <- stri_c(x$question, "\n", x$values, collapse = "\n")
      }
    } else {
      vals <- stringi::stri_replace_all(x$values[1], "\t", fixed = "\n")
      res <- stri_c(stri_c(x$primer[1], "\n", vals, "\n"), stri_c(x$question, collapse = "\n"), collapse = "\n")
    }

    cat("Current questions", stri_c("(", str_list(x$manifest), ")"))
    rdy <- FALSE
    to_clipboard(res)
    while (!rdy) {
      n <- readline("Press N when ready for next question:")
      if (n == "N")
        rdy <- TRUE
    }
  }
  invisible()
}

#' Print questionnaire
#'
#' This function creates a .xlsx file with a "pretty print" of a questionnaire
#' for a specific study. It expects the internal master questionnaire as input (
#' or a data.frame structured like the master questionnaire), as an R object. The
#' master questionnaire itself can be read by \code{\link[seamless]{read_data}}).
#'
#' @param x The questionnaire (\code{data.frame}).
#' @param file A file path for the output.
#' @param study Name of the study.
#' @param segment Filter by segment (B2B/B2C).
#' @param year Optional: Filter the annual edition of the questionnaire. Defaults
#' to current year.
#' @param entity Optional: Replace \{XX\} with something else.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   q <- read_data("master questionnaire.xlsx", sheet = "questionnaires")
#'   write_questionnaire(q, "Banking B2C.xlsx", study = "Banking", segment = "B2C", year = 2016)
#' }

write_questionnaire <- function(x, file, study = NULL, segment = NULL, year = NULL, entity = NULL) {
  blocks <- split_questionnaire(x, study, segment, year, entity, short_scale = TRUE)
  wb <- seamless::excel_workbook()

  # Write the data and retain rows that have been written to
  block_index <- lapply(blocks, function(q) {
    df <- q[, c("latent", "manifest", "question", "values")]
    # Set either the primer or question as title
    if (nrow(q) == 1L && is.na(q$primer)) {
      title <- q$question
    } else {
      title <- stri_c(unique(q$primer), collapse = "\n")
    }
    # Write the question
    seamless::to_excel(df, wb, title = title, sheet = study)
  })

  lapply(block_index, function(x) {
    rows <- x$rows["start"]:x$rows["end"]
    # Merge values for question matrix
    if (length(rows) > 3L) {
      openxlsx::mergeCells(wb, sheet = study, cols = 4, rows = tail(rows, -2L))
    }
    # Always wrap text on values
    openxlsx::addStyle(
      wb, sheet = study, style = openxlsx::createStyle(wrapText = TRUE),
      rows = tail(rows, -2L), cols = 4, gridExpand = TRUE, stack = TRUE)

  })

  # Widen the columns containing the question text and values
  openxlsx::setColWidths(wb, sheet = study, cols = 3, widths = 100)
  openxlsx::setColWidths(wb, sheet = study, cols = 4, widths = 50)

  # Save and make sure nothing is printed
  seamless::write_data(wb, file, overwrite = TRUE)
  invisible()

}

split_questionnaire <- function(x, study = NULL, segment = NULL, year = NULL, entity = NULL, short_scale = TRUE) {
  # Check input ----------------------------------------------------------------
  if (!is.data.frame(x)) {
    stop("'x' (the questionnaire) should be a data.frame.", call. = FALSE)
  }

  if (is.null(year)) {
    year <- as.numeric(format(Sys.Date(), "%Y"))
  } else if (!is.numeric(year)) {
    stop("'year' should be numeric.", call. = FALSE)
  }

  args <- list(study = study, segment = segment, entity = entity)
  not_string <- !vapply(args, is_string, logical(1)) & !vapply(args, is.null, logical(1))
  if (any(not_string)) {
    stop("The following arguments should be a string:\n", str_list(names(args)[not_string]), call. = FALSE)
  }

  # Convert to regular data.frame and lowercase names
  if (data.table::is.data.table(x))
    x <- as.data.frame(x)
  names(x) <- stri_trans_tolower(names(x))

  # Check for expected columns
  exp <- c("year", "segment", "study", "latent", "order", "manifest", "primer", "question", "type", "values", "conditional_on")
  missing <- setdiff(exp, names(x))
  if (length(missing)) {
    stop("Could not find the following (required) columns:\n", str_list(missing), call. = FALSE)
  }

  # Filter ---------------------------------------------------------------------
  # Lowercase filter-columns
  x$year <- as.numeric(x$year)
  x[, c("segment", "study")] <- lapply(x[, c("segment", "study")], stri_trans_tolower)

  # Filter study
  if (is.null(study) && length(unique(x$study)) > 1L) {
    stop("'study' cannot be NULL when 'x' contains more than one study.", call. = FALSE)
  } else if (!is.null(study) && !stri_trans_tolower(study) %in% x$study) {
    stop("The following 'study' was not found in the data:\n", str_list(study), call. = FALSE)
  } else if (!is.null(study)) {
    x <- x[x$study == stri_trans_tolower(study), ]
  }

  # Segment
  if (is.null(segment) && length(unique(x$segment)) > 1L) {
    stop("'segment' cannot be NULL when the study contains more than one segment.", call. = FALSE)
  } else if (!is.null(segment) && !stri_trans_tolower(segment) %in% x$segment) {
    stop("The following 'segment' was not found for the current study:\n", str_list(segment), call. = FALSE)
  } else if (!is.null(segment)) {
    x <- x[x$segment == stri_trans_tolower(segment), ]
  }

  # Year
  if (!year %in% x$year) {
    stop("The following 'year' was not found for the current study/segment:\n", str_list(year), call. = FALSE)
  }
  x <- x[x$year == year, ]

  # Fix question order according to "order".
  x <- x[order(x$order), ]

  # Remove all \r newlines before writing and add an index.
  x[] <- lapply(x, stringi::stri_replace_all, replacement = "", fixed = "\r")

  # Expand scale variable levels
  is_scale <- stri_trans_tolower(x$type) == "scale"
  x$values[is_scale] <-  vapply(x$values[is_scale], function(v) {
    v <- unlist(stringi::stri_split(v, fixed = "\n"))
    if (length(v) == 1L) {
      v
    } else {
      dot <- if (short_scale) "..." else stri_c(2:9, collapse = "\n")
      stri_c(v[1], dot, v[2], if (length(v) > 2L) v[3] else "", sep = "\n")
    }
  }, character(1))

  # Replace {XX} with whatever value is specified
  if (!is.null(entity)) {
    x[] <- lapply(x, stri_replace_all, replacement = entity, fixed = "{XX}")
  }

  # Loop through the rows and update the index
  x$index <- NA_real_
  for (i in 1:nrow(x)) {
    if (all(is.na(x$index))) {
      index_value <- 1L
    } else {
      index_value <- max(x$index, na.rm = TRUE) + 1
    }
    # If index and primer are NA - use the rownumber as index
    if (is.na(x$primer[i]) && is.na(x$index[i])) {
      x$index[i] <- index_value
      # If only the index is NA - give the same index to all identical primers
    } else if (is.na(x$index[i])) {
      x$index[x$primer %in% x$primer[i]] <- index_value
    }
  }

  split(x, x$index)
}
