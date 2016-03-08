#' reporttool: Work with Surveys to create reports.
#'
#' This is a package created to make it easier to work with Survey data in R,
#' to generate (batch) reports. As such, it provides convenience functions that
#' makes it easier to:
#'
#' \itemize{
#'
#' \item Get data in and out of R. (See \code{\link{read_data}} and \code{\link{write_data}}).
#' \item Manipulate the data using \code{data.table} or \code{dplyr}.
#' \item set/get additional information regarding the Survey, such as labels, and
#'       keep track of changes in the underlying data. (See \code{\link{set_label}}
#'       and \code{\link{get_label}}).
#' \item Produce standardized plots and tables.
#' \item Generate reports in batch.
#'
#' }
#' @author Kristian D. Olsen
#' @importFrom stringi stri_c stri_detect stri_replace stri_sub stri_length stri_pad stri_enc_tonative stri_trans_totitle stri_trans_tolower
#' @docType package
#' @name reporttool

NULL
