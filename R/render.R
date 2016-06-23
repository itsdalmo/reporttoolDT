#' Render .Rmd to HTML
#'
#' This function is a wrapper for \code{\link[rmarkdown]{render}} with HTML output.
#'
#' @inheritParams render_pdf
#' @param keep_md Keep intermediary \code{.md} files when generating HTML.
#' @param ... Further arguments passed to \code{\link{html_template}}.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   # TODO
#'   render_html("example.Rmd")
#' }

render_html <- function(input, output = NULL, env = parent.frame(), encoding = "UTF-8", ...) {
  if (!requireNamespace("knitr", quietly = TRUE))
    stop("'knitr' is required for generating PDF's.", quietly = TRUE)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("'rmarkdown' is required for generating PDF's.", quietly = TRUE)
  rmarkdown::render(
    input = clean_path(input),
    output_format = html_template(...),
    output_dir = output,
    clean = TRUE,
    envir = env,
    quiet = TRUE,
    encoding = encoding
  )
}

#' Render .Rmd to PPT
#'
#' This function takes a rmarkdown file as input and generates a powerpoint as
#' ouput, using \pkg{ReporteRs}. It is a dumbed down version of \pkg{knitr}, similar
#' to \code{\link{render_pdf}} (where \code{#} is a section and \code{##} is a
#' new slide).
#'
#' @inheritParams render_pdf
#' @param template Optional powerpoint template to use for when rendering.
#' @param ... Unused.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   # TODO
#'   render_ppt("example.Rmd")
#' }

render_ppt <- function(input, output = NULL, env = parent.frame(), encoding = "UTF-8", template = NULL, ...) {
  if (!requireNamespace("ReporteRs", quietly = TRUE))
    stop("'ReporteRs' is required for generating PPT's.", quietly = TRUE)
  input <- clean_path(input)
  rmd <- readLines(input, encoding = encoding)

  ppt <- seamless::ppt_workbook(template = template) # Default template
  pat <- get_default("pat_rmd")

  # First entry is always the YAML. Rest is results.
  res <- evaluate_rmd(rmd, env = env)
  yaml <- res[[1L]]; res <- res[-1L]

  # Use YAML information in a title slide
  ts <- split_yaml(yaml)
  ppt <- seamless::add_ts(ppt, ts$title, ts$subtitle, ts$author, ts$date)

  # Loop through results and add to ppt. (Excluding yaml)
  # (Note that subtitles will be used as titles if there are no sections.)
  title <- NULL; subtitle <- NULL

  for (blk in res) {
    if (is.character(blk)) {
      # Check if it contains a title or subtitle
      # ('#' = Section/title, '##' = Slide/subtitle)
      is_title <- stri_detect(blk, regex = pat$section)
      is_subtitle <- stri_detect(blk, regex = pat$slide)

      if (length(blk) == 1L && (is_title | is_subtitle)) {
        if (is_title) {
          title <- stri_replace(blk, "$1", regex = pat$section)
        } else {
          subtitle <- stri_replace(blk, "$1", regex = pat$slide)
        }
        next # (sub)title updated. Skip to next block.
      } else {
        blk <- stri_c(blk, collapse = "\n")

      }
    }
    # Add blk to doc with to_ppt.
    seamless::to_ppt(blk, ppt, title = title %||% subtitle, subtitle = if (!is.null(title)) subtitle)
  }

  # Write the finished document.
  path <- output %||% dirname(input)
  output <- file.path(path, stri_c(basename_sans_ext(input), ".pptx"))
  seamless::write_data(ppt, file = output)

}

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
#'   # TODO
#'   render_pdf("example.Rmd")
#' }

render_pdf <- function(input, output = NULL, env = parent.frame(), encoding = "UTF-8", keep_sty = FALSE, ...) {
  if (!requireNamespace("knitr", quietly = TRUE))
    stop("'knitr' is required for generating PDF's.", quietly = TRUE)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("'rmarkdown' is required for generating PDF's.", quietly = TRUE)
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

#' @rdname render_html
#' @export
html_template <- function(toc = TRUE, keep_md = FALSE) {
  format <- rmarkdown::html_document(
    theme = "spacelab",
    toc = toc,
    toc_depth = 1L,
    keep_md = keep_md
  )

  # Set knitr options for HTML output
  format$knitr$opts_chunk$echo <- FALSE
  format$knitr$opts_chunk$message <- FALSE
  format$knitr$opts_chunk$warning <- FALSE
  format$knitr$opts_chunk$dev <- "svg"
  format$knitr$opts_chunk$dev.args <- list(bg = "transparent")
  format$knitr$opts_chunk$results <- "asis"

  format

}