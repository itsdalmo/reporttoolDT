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
#' @param encoding Encoding of the input file.
#' @param ... Further arguments passed to the render functions.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' \dontrun{
#'   # TODO
#'   generate_report(input = "example.Rmd")
#' }

generate_report <- function(x, input = NULL, group = NULL, output = NULL, type = c("pdf", "ppt", "html"), encoding = "UTF-8", ...) {
  if (missing(x)) stop("'x' cannot be missing. Please use render_", type, " instead.", call. = FALSE)
  type <- match.arg(type, several.ok = FALSE)

  # If input is not specified, use default template. If dir is not specified,
  # use the same directory as the input file, or working directory if input is NULL.
  if (!is.null(output))
    output <- clean_path(output)
  if (is.null(input)) {
    if (!is.survey(x)) stop("'x' must be a Survey when input is NULL (default template).")
    input <- system.file("rmarkdown/templates/report_template/skeleton/skeleton.Rmd", package = "reporttoolDT")
    output <- output %||% getwd()
  } else {
    input <- clean_path(input)
    ext <- stri_trans_tolower(tools::file_ext(input))
    if (ext != "rmd") stop("Argument input should be a path to a .Rmd file:\n", input)
    output <- output %||% dirname(input)
  }

  # Check whether the data has been grouped in a dplyr chain.
  if (is.null(group) && requireNamespace("dplyr", quietly = TRUE)) {
    group <- dplyr::groups(x)
  }
  # If a group is specified - generate_report should create one report
  # per entity, and make the current 'entity' available in the environment.
  if (!is.null(group)) {
    group <- as.character(group)
    if (!group %in% names(x))
      stop("'group' (", group, ") was not found in 'x'")
    entities <- unique(x[[group]])
  } else {
    entities <- basename_sans_ext(input)
  }

  # Create a new environment which contains 'x'.
  if (is.survey(x)) {
    envir <- list(srv = x$clone(deep = TRUE))
  } else if (is_list(x)) {
    if (!is_named(x)) stop("All list items in 'x' must be named.")
    envir <- x
  } else {
    name <- stri_replace(deparse(substitute(x)), "$1", regex = "([a-zA-Z]+).*")
    envir <- setNames(list(x), name)
  }

  envir <- list2env(envir, parent = environment())
  assign("mainentity", group, envir = envir)
  md <- readLines(input, encoding = encoding)

  # Make sure the Markdown and output directory exists, and the theme files.
  output_dir <- switch(type, pdf = "PDF", html = "HTML", ppt = "Powerpoint")
  dir.create(file.path(output, output_dir), showWarnings = FALSE)
  dir.create(file.path(output, "Markdown"), showWarnings = FALSE)

  for (entity in entities) {
    # Create path to the new .Rmd file and add entity to the environment.
    inp <- file.path(output, "Markdown", stri_c(entity, ".Rmd"))
    out <- file.path(output, output_dir)
    assign("entity", entity, envir = envir)

    # Create new .Rmd file after replacing "REPLACE_ENTITY".
    writeLines(stringi::stri_replace_all(md, entity, regex = "REPLACE_ENTITY"), inp)

    # Render the output
    switch(type,
      pdf = render_pdf(input = inp, output = out, env = envir, keep_sty = TRUE),
      ppt = render_ppt(input = inp, output = out, env = envir),
      html = render_html(input = inp, output = out, env = envir)
    )
  }

  invisible()

}

#' @rdname generate_report
#' @export
report_pdf <- function(x, input = NULL, group = NULL, output = NULL, encoding = "UTF-8", ...) {
  generate_report(x, input = input, group = group, type = "pdf", output = output, encoding = encoding, ...)
}

#' @rdname generate_report
#' @export
report_ppt <- function(x, input = NULL, group = NULL, output = NULL, encoding = "UTF-8", ...) {
  generate_report(x, input = input, group = group, type = "ppt", output = output, encoding = encoding, ...)
}

#' @rdname generate_report
#' @export
report_html <- function(x, input = NULL, group = NULL, output = NULL, encoding = "UTF-8", ...) {
  generate_report(x, input = input, group = group, type = "html", output = output, encoding = encoding, ...)
}
