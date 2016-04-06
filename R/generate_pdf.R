#' PDF report
#'
#' Generate a pdf report from \code{Rmarkdown} using the included beamer template.
#'
#' @param x A \code{Survey} or \code{list} of objects to pass to the \code{knitr}
#' environment when generating the report. Set to \code{NULL} to disable.
#' @param rmd Path to an existing \code{.Rmd} document. Set to \code{NULL} to use
#' the default template.
#' @param entity Optional: Specify which entities to generate the report for.
#' Only used when \code{x} is a \code{Survey}.
#' @param dir Optional: Root directory for the resulting folder structure.
#' @param ... Further arguments passed to \code{beamer_template}.
#' @param toc Include a table of contents.
#' @param keep_tex Keep intermediary .tex files. (For debugging).
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   # Data is the first argument, so it works with pipes.
#'   x %>% generate_pdf(rmd = "example.Rmd", entity = NULL)
#' }

generate_pdf <- function(x, rmd = NULL, entity = NULL, dir = NULL, ...) {
  if (!requireNamespace("knitr", quietly = TRUE))
    stop("'knitr' is required for generating PDF's.", quietly = TRUE)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("'rmarkdown' is required for generating PDF's.", quietly = TRUE)

  # If rmd is not specified, use default template. If dir is not specified,
  # use the same directory as the rmd file, or working directory if rmd is NULL.
  if (is.null(rmd)) {
    if (!is.survey(x)) stop("'x' must be a Survey when rmd is NULL (default template).")
    rmd <- system.file("rmarkdown/templates/tmpdf/skeleton/skeleton.Rmd", package = "reporttoolDT")
    dir <- dir %||% getwd()
  } else {
    rmd <- clean_path(rmd)
    ext <- stri_trans_tolower(tools::file_ext(rmd))
    if (ext != "rmd") stop("Argument rmd should be a path to a .Rmd file:\n", rmd)
    dir <- dir %||% dirname(rmd)
  }

  # Read in the .Rmd file (UTF-8 encoding)
  if (!file.exists(rmd)) stop("File not found:\n", rmd)
  md <- readLines(rmd, encoding = "UTF-8")

  # Make sure the Markdown and PDF directory exists, and the theme files.
  dir.create(file.path(dir, "PDF"), showWarnings = FALSE)
  dir.create(file.path(dir, "Markdown"), showWarnings = FALSE)

  # Copy theme files to the markdown directory
  theme <- get_default("theme")$beamer
  theme <- system.file(file.path(theme$dir, theme$files), package = "reporttoolDT")
  for (thm in theme) {
    file.copy(from = thm, to = file.path(dir, "Markdown"), overwrite = FALSE)
  }

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

  envir <- list2env(input, parent = environment())

  # TODO: How should entities be handled if specified? Why doesn't x[[ent]] work?
  # Create one report per entity for Surveys.
  if (is.survey(x)) {
    # Only create reports for entities which have observations.
    ent <- names(get_association(x, "mainentity"))
    if (!length(ent)) stop("'mainentity' is not specified for the Survey.")
    ent <- unique(as.character(x[[ent]]))
    ent <- ent[stringi::stri_order(ent)]
  } else {
    # If 'x' is not a survey, create one report based on the Rmd template.
    ent <- basename_sans_ext(rmd)
  }

  is_survey <- is.survey(x)
  message("Generating PDF from:\n", stri_c("'", rmd, "'"), if (is_survey) "\n\nCurrent entity:")

  for (entity in ent) {
    # Generate a new Rmd for each entity in Markdown folder
    path <- file(file.path(dir, "Markdown", stri_c(entity, ".Rmd")), encoding = "UTF-8")
    writeLines(md, path); close(path)

    # Assign entity to 'entity' in the environment. Print current.
    if (is_survey) {
      envir$entity <- entity
      message(stri_c("'", entity, "'"))
    }

   # Render each Rmd and output to PDF folder
    rmarkdown::render(file.path(dir, "Markdown", stri_c(entity, ".Rmd")),
                      output_format = beamer_template(...),
                      intermediates_dir = file.path(dir, "Markdown"),
                      output_dir = file.path(dir, "PDF"),
                      quiet = FALSE,
                      envir = envir,
                      encoding = "UTF-8")

  }

}

#' @rdname generate_pdf
#' @export
beamer_template <- function(toc = TRUE, keep_tex = FALSE) {

  # Update beamer_presentation
  format <- rmarkdown::beamer_presentation(
    template = "default",
    latex_engine = "xelatex",
    toc = toc,
    keep_tex = keep_tex,
    slide_level = 2L,
    theme = "metropolis",
    fonttheme = "metropolis",
    colortheme = "metropolis"
    )

  # Set knitr options for PDF output
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  format$knitr$opts_chunk$warning <- FALSE
  format$knitr$opts_chunk$dev <- "cairo_pdf"
  format$knitr$opts_chunk$dev.args <- list(bg = "transparent")
  format$knitr$opts_chunk$results <- "asis"

  format

}
