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
#' @param entity Optional: Replace {XX} with something else.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   q <- read_data("master questionnaire.xlsx", sheet = "questionnaires")
#'   write_questionnaire(q, "Banking B2C.xlsx", study = "Banking", segment = "B2C", year = 2016)
#' }

write_questionnaire <- function(x, file, study = NULL, segment = NULL, year = NULL, entity = NULL) {
  # Check input ----------------------------------------------------------------
  if (!is.data.frame(x)) {
    stop("'x' (the questionnaire) should be a data.frame.", call. = FALSE)
  }
  # Convert to regular data.frame and lowercase names
  if (data.table::is.data.table(x))
    x <- as.data.frame(x)
  names(x) <- stri_trans_tolower(names(x))

  # Check for expected columns
  exp <- c("year", "segment", "study", "latent", "order", "manifest", "primer", "question", "type", "values", "conditional_on")
  missing <- setdiff(exp, names(x))
  if (length(missing)) {
    stop("Could not find the following expected columns:\n", str_list(missing), call. = FALSE)
  }

  # Lowercase filter-columns
  x[, c("year", "segment", "study")] <- lapply(x[, c("year", "segment", "study")], stri_trans_tolower)

  # Filter ---------------------------------------------------------------------
  # Study
  if (is.null(study) && length(unique(x$study)) > 1L) {
    stop("'study' cannot be NULL when 'x' contains more than one study.", call. = FALSE)
  } else if (!study %in% x$study) {
    stop("The following 'study' was not found in the data:\n", str_list(study), call. = FALSE)
  }
  x <- x[x$study == study, ]

  # Segment
  if (is.null(segment) && length(unique(x$segment)) > 1L) {
    stop("'segment' cannot be NULL when the study contains more than one segment.", call. = FALSE)
  } else if (!segment %in% x$segment) {
    stop("The following 'segment' was not found for the current study:\n", str_list(segment), call. = FALSE)
  }
  x <- x[x$segment == segment, ]

  # Year
  if (!year %in% x$year) {
    stop("The following 'year' was not found for the current study/segment:\n", str_list(year), call. = FALSE)
  }
  x <- x[x$year == year, ]

  # Fix question order according to "order".
  x <- x[order(x$order), ]

  # Write ----------------------------------------------------------------------

#
#   # Subset and create index
#   quest <- quest[stri_trans_tolower(quest$study) == study & stri_trans_tolower(quest$segment) == segment, ]
#   quest[] <- lapply(quest, stri_replace_all, replacement = "", regex = "\\r")
#   quest$index <- NA
#
#   # Expand scale variable levels
#   is_scale <- stri_trans_tolower(quest$type) == "scale"
#   quest$values[is_scale] <-  vapply(quest$values[is_scale], function(x) {
#     x <- split_scale(x); stri_c(x[1], "...", x[10], if (length(x) > 10) x[11] else "", sep = "\n")
#   }, character(1))
#
#   # Replace {XX} with whatever value is specified
#   if (!is.null(entity)) {
#     quest[] <- lapply(quest, stri_replace_all, replacement = entity, regex = "\\{XX\\}")
#   }
#
#   # Loop through the rows and update the index
#   for (i in 1:nrow(quest)) {
#
#     # Get the next index value
#     index_value <- if (all(is.na(quest$index))) 1 else max(quest$index, na.rm = TRUE) + 1
#
#     # If index and primer are NA - use the rownumber as index
#     if (is.na(quest$primer[i]) && is.na(quest$index[i])) {
#       quest$index[i] <- index_value
#
#       # If only the index is NA - give the same index to all identical primers
#     } else if (is.na(quest$index[i])) {
#       quest$index[quest$primer %in% quest$primer[i]] <- index_value
#     }
#
#   }
#
#   # Split the questionnaire by index
#   split_quest <- split(quest, quest$index)
#
#   # Use the split list to write to a xlsx_file
#   wb <- openxlsx::createWorkbook()
#
#   # Write the data and retain rows that have been written to
#   merge_rows <- lapply(split_quest, function(x) {
#
#     # Get columns
#     df <- x[, c("latent", "manifest", "question", "values")]
#
#     # Set either the primer or question as title
#     if (nrow(x) == 1L && is.na(x$primer)) {
#       title <- x$question
#     } else {
#       title <- stri_c(unique(x$primer), collapse = "\n")
#     }
#
#     # Write the question
#     to_sheet(df, wb, title = title, sheet = study)
#
#   })
#
#   lapply(merge_rows, function(x) {
#
#     rows <- x["first"]:x["last"]
#
#     # Merge values for question matrix
#     if (length(rows) > 2L) {
#       openxlsx::mergeCells(wb, sheet = study, cols = 4, rows = rows[-1])
#     }
#
#     # Set values to wrap text
#     openxlsx::addStyle(wb, sheet = study, style = openxlsx::createStyle(wrapText = TRUE),
#                        rows = rows[-1], cols = 4, gridExpand = TRUE, stack = TRUE)
#
#   })
#
#   # Widen the columns containing the question text and values
#   openxlsx::setColWidths(wb, sheet = study, cols = 3, widths = 100)
#   openxlsx::setColWidths(wb, sheet = study, cols = 4, widths = 50)
#
#   # Save and make sure nothing is printed
#   openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
#   invisible()

}
