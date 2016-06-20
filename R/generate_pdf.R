#' Render .Rmd to PDF
#'
#' Render a \code{rmarkdown} file using the included theme (\code{mtheme}). Read
#' more in the documentation for \code{\link[rmarkdown]{render}}.
#'
#' @param input Path to a \code{.Rmd} file.
#' @param output Output directory. Default uses the directory of the input file.
#' @param encoding The encoding of the input file. Defaults to \code{UTF-8}.
#' @param env Optional environment to use when evaluating the \code{.Rmd}-file.
#' @param keep_sty Optional: Keep \code{.sty} files for the beamer theme.
#' @param ... Further arguments passed to \code{beamer_template}.
#' @param toc Include a table of contents.
#' @param keep_tex Keep intermediary .tex files. (For debugging).
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   # Render PDF from current working directory to root folder.
#'   render_pdf("./Example.Rmd", output = "../")
#' }

render_pdf <- function(input, output = NULL, encoding = "UTF-8", env = parent.frame(), keep_sty = FALSE, ...) {
  input <- clean_path(input)
  path <- dirname(input)

  # Make sure .sty files exist in the current folder.
  theme <- get_default("theme")$beamer
  theme <- system.file(file.path(theme$dir, theme$files), package = "reporttoolDT")
  theme <- theme[stri_detect(theme, regex = "(theme|logo)")]

  theme_files <- file.path(path, basename(theme))
  is_missing <- !file.exists(theme_files)

  # Copy all missing files and delete them on exit. (Unless keep_sty = TRUE)
  file.copy(from = theme[is_missing], to = path, overwrite = FALSE)
  if (!keep_sty) {
    on.exit(unlink(theme_files[is_missing]), add = TRUE)
  }

  # Render using the beamer template
  rmarkdown::render(
    input = input,
    output_format = beamer_template(...),
    output_dir = output,
    clean = TRUE,
    envir = env,
    quiet = TRUE,
    encoding = encoding
  )
}

#' @rdname render_pdf
#' @export
beamer_template <- function(toc = TRUE, keep_tex = FALSE) {

  dir <- "rmd/beamer/"
  template <- system.file(file.path(dir, "beamer_template.tex"), package = "reporttoolDT")
  preamble <- system.file(file.path(dir, "beamer_preamble.tex"), package = "reporttoolDT")

  # Update beamer_presentation
  format <- rmarkdown::beamer_presentation(
    template = template,
    latex_engine = "xelatex",
    toc = toc,
    keep_tex = keep_tex,
    slide_level = 2L,
    theme = "m",
    includes = rmarkdown::includes(in_header = preamble)
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
