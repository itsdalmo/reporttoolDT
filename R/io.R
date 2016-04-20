# read_survey <- function(file) {
#   if (!tools::file_ext(file) == "") {
#     stop("The specified path is not a directory:\n", file, call. = FALSE)
#   } else {
#     file <- officeR::clean_path(file)
#     if (!file.exists(file))
#       stop("The specified directory does not exist:\n", file, call. = FALSE)
#   }
#
#   # Make sure the required directories are present.
#   dir <- list.files(file)
#   missing <- setdiff(c("data", "input"), stri_trans_tolower(dir))
#   if (length(missing))
#     stop("The required folders were not found in the directory:\n", str_list(missing))
#
#   # Read in the required data
#   dir <- file.path(file, dir[stri_trans_tolower(dir) %in% c("data", "input")])
#   dir <- setNames(dir, c("data", "input"))
#
#
#
# }

#' Write a Survey
#'
#' The preferred way to store a \code{Survey} is by using \code{write_survey},
#' which persists all the information in the \code{Survey}. Information that
#' cannot be stored in SPSS for instance, will be stored in an associated .Rdata
#' file instead. This also means that you have to use \code{\link{read_survey}}
#' to get everything back.
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
#' @param file Output file or directory.
#' @param write_input Set to \code{TRUE} to create input-files for the PLS-wizard.
#' @param ... Arguments passed to \code{\link[officeR]{write_data}}.
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

write_survey <- function(x, file, write_input = FALSE) {
  stopifnot(is.survey(x))

  file <- clean_path(file)
  ext <- stri_trans_tolower(tools::file_ext(file))

  if (ext == "") {
    folders <- setNames("Data", "data")
    # Also create "input" folder if writing input.
    if (write_input) folders <- c(folders, setNames("Input", "input"))
    current <- list.files(file)

    # Reuse folders if they exist
    if (length(current)) {
      existing <- names(folders) %in% stri_trans_tolower(current)
      if (any(existing)) {
        folders[existing] <- current[match_all(tolower(folders), tolower(current))]
        folders[existing] <- file.path(file, folders[existing])
      }
    } else {
      existing <- rep(FALSE, length(folders))
    }

    # Make sure the necessary configs have been set.
    cfg <- x$get_config(c("name", "segment", "year"))
    if (is.null(cfg) || is.na(cfg[["name"]]))
      stop("Config must be set and name cannot be empty.")

    # Construct a file name from config.
    data <- stri_c(stri_c(cfg[!is.na(cfg)], collapse = " "), ".sav")
    folders[["data"]] <- file.path(folders[["data"]], data)

    # Create missing folders that are required
    if (any(!existing)) {
      folders[!existing] <- file.path(file, folders[!existing])
      lapply(folders[!existing], dir.create, showWarnings = FALSE)
      warning("Created the following (required) folders:\n", str_list(folders[!existing]))
    }
  } else {
    if (write_input)
      stop("Cannot write input when 'file' is not a directory.")
    if (ext != "sav")
      stop("Use write_data to write Surveys to formats other than .sav.")
    folders <- setNames(file, "data")
  }

  # Write input
  if (write_input)
    write_model_input(x$clone(deep = TRUE), folders[["input"]])

  # Write hidden fields.
  field_data <- stri_c(basename_sans_ext(folders[["data"]]), " (survey).Rdata")
  write_data(x$get_field(), file = file.path(dirname(file), field_data))

  # Write data
  write_data(x, file = folders[["data"]])

  # Make sure nothing is printed
  invisible()

}

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

# This function creates "input"-files for the PLS-wizard -----------------------
write_model_input <- function(x, dir) {
  # Convert Survey to Survey_dt
  x <- survey_dt(x)

  # Mainentity must be a factor variable
  me <- unname(x$get_association("mainentity"))
  if (!is.factor(x[[me]])) {
    lvls <- unique(x[[me]])
    lvls <- lvls[stri_order(lvls)]
    # Use unique/ordered values as levels for mainentity.
    x[, (me) := lapply(.SD, function(x) factor(x, levels = me_levels)), .SDcols = me]
  }

  # Make sure data has been prepared beforehand
  reqs <- c(unname(mm), me, "coderesp")
  vars <- match_all(stri_trans_tolower(reqs), stri_trans_tolower(x$names()))
  if (length(vars) != length(reqs)) stop("Could not find required variables in data. See help(prepare_data).")

  # 1 - Write measurement model ------------------------------------------------
  mm <- x$get_association(which = get_default("latents"))
  model <- split(unname(mm), names(mm))
  model <- lapply(model[unique(names(mm))], function(vars) as.integer(mm %in% vars) * -1L)
  model <- as.data.frame(c(list(manifest = unname(mm), model)), stringsAsFactors = FALSE)

  fname <- file.path(dir, "measurement model.txt")
  write.table(model, fname, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # 2 - Write model data -------------------------------------------------------
  mm <- unname(mm) # Drop latent association.
  co <- as.numeric(x$get_config("cutoff"))
  if (is.na(co)) {
    co <- .3; x$set_config(cutoff = .3)
  }

  data <- x$get_data()
  data <- data[percent_missing <= co, vars, with = FALSE]

  # Convert mainentity to integer, clean scales, recode NA and order by mainentity.
  data[, (me) := lapply(.SD, as.integer), .SDcols = me]
  data[, (mm) := lapply(.SD, clean_scale), .SDcols = mm]
  data[, (mm) := lapply(.SD, function(x) { x[is.na(x)] <- 98; x }), .SDcols = mm]
  data.table::setkeyv(data, me)

  fname <- file.path(dir, "modelldatafil.txt")
  write.table(data, fname, sep = "\t", row.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # 3 - Config (entities, count and marketshares) ------------------------------
  en <- x$entities() # Returns a data.frame.
  en <- en[, c("entity", "valid", "marketshare")]

  fname <- file.path(dir, "config.txt")
  write.table(en, fname, sep = "\t", dec = ",", col.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # 4 - Question text ----------------------------------------------------------
  qt <- unname(x$get_label(which = mm))

  fname <- file.path(dir, "qtext.txt")
  write.table(qt, fname, row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # 5 - Entity (Q1) names ------------------------------------------------------
  q1 <- levels(x[[me]])

  fname <- file.path(dir, "q1names.txt")
  write.table(q1, fname, row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = "latin1")

  # Make sure nothing is printed
  invisible()

}
