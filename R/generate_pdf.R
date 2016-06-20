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

generate_pdf <- function(input, output, encoding, env = parent.frame()) {
  rmarkdown::render(
    input = input,
    output_format = beamer_template(...),
    output_file = output,
    envir = env,
    quiet = FALSE,
    quiet = FALSE,
    encoding = encoding
  )
    rmarkdown::render(file.path(dir, "Markdown", stri_c(entity, ".Rmd")),
                      output_format = beamer_template(...),
                      intermediates_dir = file.path(dir, "Markdown"),
                      output_dir = file.path(dir, "PDF"),
                      quiet = FALSE,
                      envir = envir,
                      encoding = "UTF-8")
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
