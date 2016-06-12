#' Generate PPT from rmarkdown.
#'
#' This function takes a rmarkdown file as input and generates a powerpoint as
#' ouput, using \pkg{ReporteRs}. It is a dumbed down version of \pkg{knitr}, similar
#' to \code{\link{generate_pdf}} (where \code{#} is a section and \code{##} is a
#' new slide).
#'
#' @param rmd The \code{Rmarkdown} code to convert.
#' @param file Output destination. (\code{.pptx}).
#' @param env Optional environment in which rmarkdown code is evaluated.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   # TODO
#'   md <- readLines("example.Rmd", encoding = "UTF-8")
#'   generate_ppt(md, file = "example.pptx")
#' }

generate_ppt <- function(rmd, file, env = parent.frame()) {
  ppt <- seamless::ppt_workbook(template = NULL) # Default template
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
  seamless::write_data(ppt, file = file)

}