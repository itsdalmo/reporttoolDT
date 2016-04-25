# S3 methods for officeR -------------------------------------------------------

#' @importFrom officeR write_data
#' @export
write_data.Survey <- function(x, file, ...) {
  ext <- stri_trans_tolower(tools::file_ext(file))
  # Convert Survey to a format that can be written using officeR.
  if (ext == "xlsx") {
    x <- list(data = x$get_data(), model = model(x), entities = entities(x))
  } else if (ext == "sav") {
    # S3 method for Survey below.
    x <- officeR::to_labelled(x)
  } else if (ext %in% c("rda", "rdata")) {
    x <- setNames(list(x), deparse(substitute(x)))
  } else {
    stop("Unrecognized output format (for Survey). See help(write_survey).")
  }

  officeR::write_data(x, file, ...)

}

#' @importFrom officeR to_labelled
#' @export
to_labelled.Survey <- function(x) {
  out <- x$get_data(copy = TRUE)
  if (data.table::is.data.table(out)) {
    data.table::setattr(out, "labels", x$get_label())
  } else {
    attr(out, "labels") <- x$get_label()
  }

  officeR::to_labelled(out)

}

#' @importFrom officeR to_excel
#' @export
to_excel.Survey <- function(x, ...) {
  # TODO: Automatic labels etc.
  officeR::to_excel(x$data, ...)
}

#' @importFrom officeR to_ppt
#' @export
to_ppt.Survey <- function(x, ...) {
  # TODO: Automatic labels etc.
  officeR::to_ppt(x$data, ...)
}

#' Write a Survey
#'
#' In contrast to \code{\link[officeR]{write_data}}, \code{write_survey} will store
#' hidden fields (such as config, translations etc.) which can be retrieved by using
#' \code{\link{read_survey}}. This means that you don't have to repeatedly set
#' these options when reading/writing a survey you are working on. Information that
#' cannot be stored in SPSS will be stored in an associated .Rdata file instead.
#'
#' You can also use \code{\link[officeR]{write_data}} to write the \code{Survey}.
#' In this case the information which is persisted depends on the format used:
#'
#' \itemize{
#'  \item \code{.sav}: Data including labels, and levels for factor variables.
#'  \item \code{.xlsx}: Data, measurement model (labels and levels) and entities.
#'  \item \code{.Rdata}: Everything is stored.
#' }
#'
#' @param x A \code{Survey}.
#' @param file Output file or directory. (If a directory is specified, PLS-input
#' will be created. The resulting EM.sav file-name is based on the Survey's config.)
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#' df <- survey_df(data.frame("A" = "test", "B" = 2))
#'
#' # Store data and labels
#' officeR::write_data(df, "test.sav")
#'
#' # Store everything
#' write_survey(df, "test.Rdata")
#' }

write_survey <- function(x, file) {
  if (!is.survey(x)) stop("Use officeR::write_data to write data that is not a survey.")
  file <- clean_path(file)

  # Use extension to determine output
  ext <- stri_trans_tolower(tools::file_ext(file))
  if (ext == "") {
    # Return early. Writing model input also writes the data.
    write_model_input(x$clone(deep = TRUE), file)
  } else if (ext != "sav") {
    stop("Expecting a directory or .sav file. Other formats can be written with officeR.")
  } else {
    # Write .sav file (Long strings backup is created by officeR.)
    officeR::write_data(x, file)

    # Include hidden fields.
    file <- stri_c(tools::file_path_sans_ext(file), " (survey).Rdata")
    officeR::write_data(x$get_field(), file)
  }

  # Make sure nothing is printed
  invisible()

}

# Function to write input-files for the PLS-wizard.
# (recursively calls write_survey to write the .sav file.)
write_model_input <- function(x, file) {
  x <- survey_dt(x)

  # 1 - Use config to creat a filename -----------------------------------------
  cfg <- x$get_config(c("name", "segment", "year"))
  if (is.null(cfg)) stop("Config must be set before writing input.")
  fname <- stri_c(stri_c(cfg[!is.na(cfg)], collapse = " "), ".sav")

  # 2 - Check data -------------------------------------------------------------
  # Make sure mainentity is set and that it is a factor variable
  me <- unname(x$get_association("mainentity")); lvl <- unique(x[[me]])
  if (is.null(me)) {
    stop("'mainentity' must be set before writing input.")
  } else if (!is.factor(lvl)) {
    lvl <- lvl[stri_order(lvl)]
    x[, (me) := lapply(.SD, function(x) factor(x, levels = me_levels)), .SDcols = me]
  }

  # Check whether required variables can be found. (order by variable names)
  mm <- x$get_association(which = default_latents())
  mm <- mm[match_all(names(x), mm)]

  cr <- names(x)[stri_trans_tolower(names(x)) %in% "coderesp"]
  if (!length(cr)) stop("'coderesp' variable was not found.")
  vars <- c(unname(mm), me, cr)

  # 3 - Create required folders ------------------------------------------------
  folders <- c("Data", "Input")
  folders <- require_folders(file, folders, create = TRUE)

  # 4 - Write data -------------------------------------------------------------
  fname <- file.path(folders$data, fname)
  write_survey(x, fname)

  # 5 - Write measurement model ------------------------------------------------
  model <- split(unname(mm), names(mm))
  model <- lapply(model[default_latents()], function(vars) as.integer(mm %in% vars) * -1L)
  model <- as.data.frame(c(list(manifest = unname(mm), model)), stringsAsFactors = FALSE)

  fname <- file.path(folders$input, "measurement model.txt")
  write.table(model, fname, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # 5 - Write PLS data ---------------------------------------------------------
  mm <- unname(mm) # Drop latent association.
  co <- as.numeric(x$get_config("cutoff"))
  if (is.na(co) || !length(co)) {
    warning("Cutoff had not been set. Defaulting to 30%.", call. = FALSE)
    co <- .3; x$set_config(cutoff = .3)
  }

  data <- x$get_data()
  data <- data[percent_missing <= co, vars, with = FALSE]

  # Convert mainentity to integer, clean scales, recode NA and order by mainentity.
  data[, (me) := lapply(.SD, as.integer), .SDcols = unname(me)]
  data[, (mm) := lapply(.SD, clean_scale), .SDcols = mm]
  data[, (mm) := lapply(.SD, function(x) { x[is.na(x)] <- 98; x }), .SDcols = mm]
  data.table::setkeyv(data, me)

  fname <- file.path(folders$input, "modelldatafil.txt")
  write.table(data, fname, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # 6 - Write config (entities, count and marketshares) ------------------------
  en <- x$entities() # Returns a data.frame.
  en <- en[, c("entity", "valid", "marketshare")]

  fname <- file.path(folders$input, "config.txt")
  write.table(en, fname, sep = "\t", dec = ",", col.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # 7 - Question text ----------------------------------------------------------
  qt <- unname(x$get_label(which = mm))

  fname <- file.path(folders$input, "qtext.txt")
  write.table(qt, fname, row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # 8 - Entity (Q1) names ------------------------------------------------------
  q1 <- as.character(en$entity)

  fname <- file.path(folders$input, "q1names.txt")
  write.table(q1, fname, row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # Make sure nothing is printed
  invisible()

}

#' Read a Survey
#'
#' Read a \code{Survey} that has been written by \code{\link{write_survey}} (or
#' created manually using the PLS-wizard). This function also reads output from
#' the PLS-wizard, such as inner and outer weights. Hidden fields will be updated
#' to reflect changes in input-files when reading a survey directory.
#'
#' @param file Output file or directory.
#' @param mainentity Name of the mainentity variable in data.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#' # TODO
#' }

read_survey <- function(file, mainentity = "q1") {
  file <- clean_path(file)
  if (!file.exists(file)) stop("File does not exist:\n", file)

  # Use extension to determine output
  ext <- stri_trans_tolower(tools::file_ext(file))
  if (ext == "") {
    # Return early. Reading model output also reads in the data.
    return(read_model_output(file, mainentity))
  } else if (ext != "sav") {
    stop("Expecting a directory or .sav file. Other formats can be read with officeR.")
  }

  # Read .sav file (Long strings are read by officeR.)
  out <- officeR::read_data(file)

  # Include hidden fields if they exist.
  file <- stri_c(tools::file_path_sans_ext(file), " (survey).Rdata")
  if (file.exists(file)) {
    fields <- officeR::read_data(file)
  } else {
    fields <- NULL
  }

  out <- Survey_tbl$new(out, fields = fields) # Always return tbl?

  # Rename "andel_missing" to "percent_missing".
  if (!"percent_missing" %in% stri_trans_tolower(names(out))) {
    names(out)[stri_trans_tolower(names(out)) == "andel_missing"] <-  "percent_missing"
  }

  # Return
  out

}

# Function to read input to/output from the PLS-wizard.
# (recursively calls read_survey to read the .sav file.)
read_model_output <- function(file, mainentity) {
  # Match required folders (case_insensitive.)
  folders <- c("Data", "Input", "Output")
  folders <- require_folders(file, folders, create = FALSE)

  # 1 - Read the .sav file -----------------------------------------------------
  files <- file.path(folders$data, list.files(folders$data))
  fname <- files[stri_detect(files, regex = ".*em\\.sav$", case_insensitive = TRUE)]
  if (length(fname) != 1L) {
    msg <- if (length(fname) > 1L) "Found more than one" else "Could not find any"
    stop(stri_c(msg, "'...EM.sav' files.", sep = " "))
  }

  # Call read_survey again, using the path to the .sav.
  out <- read_survey(fname, mainentity = mainentity)

  # 2 - Read model -------------------------------------------------------------
  files <- file.path(folders$input, list.files(folders$input))
  fname <- files[stri_detect(files, regex = "measurement model.*\\.txt", case_insensitive = TRUE)]
  if (length(fname) != 1L) {
    msg <- if (length(fname) > 1L) "Found more than one" else "Could not find any"
    stop(stri_c(msg, "file matching 'measurement model.txt'", sep = " "))
  }

  mm <- officeR::read_data(fname, encoding = "latin1", col_types = list("Manifest" = readr::col_character()))
  mm <- lapply(mm[-1], function(x, vars) {vars[x == -1]}, mm[[1]])

  # Case insensitive matching for mainentity.
  mainentity <- names(out)[tolower(names(out)) %in% tolower(mainentity)]

  # Set associations specified in measurement model (and mainentity).
  out$set_association(list = mm, mainentity = mainentity)

  # 3 - Read config ------------------------------------------------------------
  fname <- files[stri_detect(files, regex = "config.*\\.txt", case_insensitive = TRUE)]
  if (length(fname) != 1L) {
    msg <- if (length(fname) > 1L) "Found more than one" else "Could not find any"
    stop(stri_c(msg, "file matching 'config.txt'", sep = " "))
  }

  cf <- officeR::read_data(fname, encoding = "latin1", col_names = FALSE, decimal = ",")
  names(cf) <- c("row", "entity", "valid", "marketshare")

  # Set marketshare based on config
  out$set_marketshare(list = setNames(as.list(cf$marketshare), cf$entity))

  # 4 - Read inner/outer weights -----------------------------------------------

  # TODO

  # Return
  out

}

# Match contents from a path to a list of required folders (case insensitive)
# (Returns a named list with paths to the folders. Optionally creates missing folders.)
require_folders <- function(path, folders, create = FALSE) {
  if (missing(folders) || !is.character(folders))
    stop("Argument 'folders' should be a character vector.")

  # Check/create required folders (error if create = FALSE)
  existing <- list.files(path)
  missing <- folders[!tolower(folders) %in% tolower(existing)]
  if (length(missing)) {
    if (!create) stop("Required folders do not exist:\n", str_list(missing), call. = FALSE)
    for (folder in missing) {
      dir.create(file.path(path, folder), showWarnings = FALSE)
    }
    warning("Created missing folders:\n", str_list(missing), call. = FALSE)
  }

  # Reuse existing (case insensitive)
  paths <- c(missing, existing[tolower(existing) %in% tolower(folders)])
  paths <- setNames(file.path(path, paths), tolower(paths))

  # Return as list (avoid brackets)
  as.list(paths)

}
