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
#' @param srv A \code{Survey}.
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
