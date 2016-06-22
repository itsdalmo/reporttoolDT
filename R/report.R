#' Render .Rmd to HTML
#'
#' This function is a wrapper for \code{\link[rmarkdown]{render}} with HTML output.
#'
#' @param x An object to pass to the knitr environment when processing.
#' @param input Path to a \code{.Rmd} report template.
#' @param group A variable to "group" the reports by. Each entity will get their
#' own report.
#' @param type What format should the output be in. PDF, PPT or HTML.
#' @param output Optional directory to use for output files.
#' @param ... Further arguments passed to the render functions.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   # TODO
#'   render_html("example.Rmd")
#' }

generate_report <- function(x, input = NULL, group = NULL, output = NULL, type = c("pdf", "ppt", "html"), ...) {
  if (!requireNamespace("knitr", quietly = TRUE))
    stop("'knitr' is required for generating PDF's.", quietly = TRUE)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("'rmarkdown' is required for generating PDF's.", quietly = TRUE)

  type <- stri_trans_tolower(type)
  type <- match.arg(type, several.ok = FALSE)

  # If input is not specified, use default template. If dir is not specified,
  # use the same directory as the input file, or working directory if input is NULL.
  if (is.null(input)) {
    if (!is.survey(x)) stop("'x' must be a Survey when input is NULL (default template).")
    input <- system.file("rmarkdown/templates/report_template/skeleton/skeleton.Rmd", package = "reporttoolDT")
    output <- output %||% getwd()
  } else {
    input <- clean_output(input)
    ext <- stri_trans_tolower(tools::file_ext(input))
    if (ext != "rmd") stop("Argument input should be a path to a .Rmd file:\n", input)
    output <- output %||% dirname(input)
  }

  # Make sure the Markdown and output directory exists, and the theme files.
  output_dir <- switch(type, pdf = "PDF", html = "HTML", ppt = "Powerpoint")
  dir.create(file.path(output, output_dir), showWarnings = FALSE)
  dir.create(file.path(output, "Markdown"), showWarnings = FALSE)

  # Create a new environment which contains 'x'.
  if (is.survey(x)) {
    input <- list(srv = x$clone(deep = TRUE))
  } else if (is_list(x)) {
    if (!is_named(x)) stop("All list items in 'x' must be named.")
    input <- x
  } else {
    name <- stri_replace(deparse(substitute(x)), "$1", regex = "([a-zA-Z]+).*")
    input <- setNames(list(x), name)
  }

  # TODO: How should entities be handled if specified? Why doesn't x[[ent]] work?
  # Create one report per group for Surveys.
  if (is.survey(x)) {
    # Only create reports for entities which have observations.
    ent <- get_association(x, "mainentity")
    if (is.null(ent)) stop("'mainentity' is not specified for the Survey.")
    ent <- unique(as.character(x[[ent]]))
    ent <- ent[stringi::stri_order(ent)]
  } else {
    # If 'x' is not a survey, create one report based on the Rmd template.
    ent <- basename_sans_ext(input)
  }

  input <- c(input, list(entity = ent))
  envir <- list2env(input, parent = environment())


}
