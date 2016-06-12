#' reporttoolDT: Work with Surveys to create reports.
#'
#' This is a package created to make it easier to work with Survey data in R,
#' to generate (batch) reports. Together with \code{\link[seamless]{seamless}} and
#' \code{\link[tabulR]{tabulR}} it provides the following functionality:
#'
#' @section Read and write data:
#' \itemize{
#' \item Get data in and out of R with \code{\link[seamless]{read_data}} and \code{\link[seamless]{write_data}}.
#' \item Access sharepoint using \code{\link[seamless]{sharepoint_link}} and/or \code{\link[seamless]{sharepoint_mount}}.
#' \item Read/write to the clipboard on Windows and Os X using \code{\link[seamless]{from_clipboard}} and \code{\link[seamless]{to_clipboard}}.
#' \item Send data or plots to Excel and Powerpoint with \code{\link[seamless]{to_excel}} and \code{\link[seamless]{to_ppt}}.
#' }
#'
#' @section Create a Survey:
#' \itemize{
#' \item Create a new survey using \code{\link{survey}}.
#' \item Read or write an existing survey using \code{\link{read_survey}} and \code{\link{write_survey}}.
#' \item Adjust and create fleixbile but consistent output from a Survey, using
#' \code{\link{set_label}}, \code{\link{set_association}} and \code{\link{set_translation}}.
#' \item Generate reports in PDF or Powerpoint formats using your own, or the included template,
#' with \code{\link{generate_pdf}} and \code{\link{generate_ppt}}.
#' }
#'
#' @section Create standardized tables and plots:
#' \itemize{
#' \item Create tables using \code{\link[tabulR]{qtable}} and \code{\link[tabulR]{qtable_}}.
#' \item Plots using \code{\link[tabulR]{bar_chart}} and \code{\link[tabulR]{line_chart}}.
#' \item For Survey's, you can also use \code{\link{manifest_table}} and \code{\link{manifest_plot}},
#' as well as \code{\link{latent_table}} and \code{\link{latent_plot}}.
#' \item \code{\link{impact_table}} and \code{\link{flow_chart}} can be used
#' when the Survey includes PLS-weights.
#' }
#'
#' @author Kristian D. Olsen
#' @importFrom stats setNames
#' @importFrom stringi stri_c stri_detect stri_replace stri_sub stri_length stri_pad stri_enc_tonative stri_trans_totitle stri_trans_tolower
#' @importFrom utils head tail write.table
#' @docType package
#' @name reporttoolDT

NULL
