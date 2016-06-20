generate_report <- function(x, rmd = NULL, entity = NULL, type = c("pdf", "ppt", "html"), path = NULL, ...) {
  if (!requireNamespace("knitr", quietly = TRUE))
    stop("'knitr' is required for generating PDF's.", quietly = TRUE)
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("'rmarkdown' is required for generating PDF's.", quietly = TRUE)

  type <- stri_trans_tolower(type)
  type <- match.arg(type, several.ok = FALSE)

  # If rmd is not specified, use default template. If dir is not specified,
  # use the same directory as the rmd file, or working directory if rmd is NULL.
  if (is.null(rmd)) {
    if (!is.survey(x)) stop("'x' must be a Survey when rmd is NULL (default template).")
    rmd <- system.file("rmarkdown/templates/report_template/skeleton/skeleton.Rmd", package = "reporttoolDT")
    path <- path %||% getwd()
  } else {
    rmd <- clean_path(rmd)
    ext <- stri_trans_tolower(tools::file_ext(rmd))
    if (ext != "rmd") stop("Argument rmd should be a path to a .Rmd file:\n", rmd)
    path <- path %||% dirname(rmd)
  }

  # Read in the .Rmd file (UTF-8 encoding)
  if (!file.exists(rmd)) stop("File not found:\n", rmd)
  md <- readLines(rmd, encoding = "UTF-8")

  # Make sure the Markdown and output directory exists, and the theme files.
  output_dir <- switch(type, pdf = "PDF", html = "HTML", ppt = "Powerpoint")
  dir.create(file.path(path, output_dir), showWarnings = FALSE)
  dir.create(file.path(path, "Markdown"), showWarnings = FALSE)

  # Copy theme files to the markdown directory
  theme <- get_default("theme")$beamer
  theme <- system.file(file.path(theme$dir, theme$files), package = "reporttoolDT")
  for (thm in theme) {
    file.copy(from = thm, to = file.path(path, "Markdown"), overwrite = FALSE)
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

  # TODO: How should entities be handled if specified? Why doesn't x[[ent]] work?
  # Create one report per entity for Surveys.
  if (is.survey(x)) {
    # Only create reports for entities which have observations.
    ent <- get_association(x, "mainentity")
    if (is.null(ent)) stop("'mainentity' is not specified for the Survey.")
    ent <- unique(as.character(x[[ent]]))
    ent <- ent[stringi::stri_order(ent)]
  } else {
    # If 'x' is not a survey, create one report based on the Rmd template.
    ent <- basename_sans_ext(rmd)
  }

  input <- c(input, list(entity = ent))
  envir <- list2env(input, parent = environment())

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
                      output_dir = file.path(path, output_dir),
                      quiet = TRUE,
                      envir = envir,
                      encoding = "UTF-8")

  }

}
