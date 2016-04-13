evaluate_rmd <- function(rmd, env = new.env()) {
  blk <- split_blocks(rmd)
  out <- vector("list", length(blk))

  # Loop through blocks and evaluate them
  for (i in seq_along(out)) {
    out[[i]] <- eval_block(blk[[i]], env)
  }

  # Drop missing results
  out <- out[!vapply(out, is.null, logical(1)) & vapply(out, length, numeric(1)) > 0L]

  out
}

eval_block <- function(blk, env) UseMethod("eval_block")

# Markdown/YAML ----------------------------------------------------------------
eval_block.markdown <- function(blk, env) {
  lines <- blk$content
  pat <- get_default("pat_rmd")

  # Using for-loops so that code is evaluated in turn (in case of assignment).
  inline <- which(stri_detect(lines, regex = pat$inline))
  if (length(inline)) {
    for (i in inline) {
      # Extract all inlines expressions in the given lines w/inlines code
      code <- unlist(stringi::stri_extract_all(lines[i], regex = pat$inline))
      expr <- stringi::stri_replace_all(code, "", regex = "`r\\s?|\\s?`")

      # Exaluate and drop source code
      expr <- evaluate::evaluate(expr, envir = env, output_handler = rmd_handler)
      expr <- expr[!vapply(expr, inherits, what = "source", logical(1))]

      # Replace inlines code with return values
      for (ii in seq_along(expr)) {
        lines[i] <- stri_replace(lines[i], expr[[ii]] %||% "", fixed = code[ii])
      }
    }
  }

  if (all(lines == ""))
    lines <- NULL

  lines

}

eval_block.yaml <- eval_block.markdown

# Code chunks ------------------------------------------------------------------
eval_block.chunk <- function(blk, env) {
  lines <- blk$content
  code <- lines[-c(1L, length(lines))]
  pat <- get_default("pat_rmd")

  # Check options
  opts <- stri_replace(lines[1], "$1", regex = pat$chunk_start)
  opts <- stri_replace(opts, "", regex = "^([a-zA-Z]+)")
  opts <- knitr:::parse_params(opts)

  if (opts$eval %||% TRUE) {
    res <- evaluate::evaluate(code, envir = env)
    res <- res[!vapply(res, inherits, what = "source", logical(1))]
  } else {
    res <- NULL
  }

  res

}

# Handlers for evaluate --------------------------------------------------------
rmd_handler <- evaluate::new_output_handler(
  value = function(x, visible = TRUE) {
    if (!visible) return()
    # if (evaluate::is.recordedplot(x)) {
    #   evaluate:::render(x)
    # } else {
    #   x
    # }
    x
  }
)

# Split rmd into groups --------------------------------------------------------
split_blocks <- function(rmd, pattern = get_default("pat_rmd")) {
  # Check input (either a string or a character vector)
  if (!is.character(rmd)) stop("Expecting character vector as input.")
  if (is_string(rmd)) {
    if (!any(stri_detect(rmd, fixed = "\n")))
      stop("The string does not contain any new lines.")
    rmd <- stri_split(rmd, fixed = "\n")
  }

  n <- length(rmd)

  # Detect patterns associated with chunks and their indicies
  chunk_start <- stri_detect(rmd, regex = pattern$chunk_start)
  chunk_end <- stri_detect(rmd, regex = pattern$chunk_end)

  # Get indicies for chunks
  # (Using sum() stri_detect returns logical vectors)
  if (sum(chunk_start) != sum(chunk_end))
    stop("Detected unused delimiters (```) for code chunks.")
  chunk <- Map(":", which(chunk_start), which(chunk_end))

  # Get YAML delimiters that are not in a chunk.
  yaml <- stri_detect(rmd, regex = pattern$yaml_start) & !(1:n %in% unlist(chunk))
  if (sum(yaml) %% 2 == 1L) stop("Unused delimiters (---) for YAML fronmatter.")
  yaml <- which(yaml)[1]:which(yaml)[2]

  # Rest of the document should be text and/or inline code
  # (yaml_end:1st_chunk_start, 1st_chunk_end:2nd_chunk_start ... last_c_e:end)
  text_start <- c(max(yaml) + 1L, which(chunk_end) + 1L)
  text_end <- c(which(chunk_start) - 1L, n)
  if (length(text_start) != length(text_end))
    stop("Start and end indicies for text are not same length. (Should not happen).")
  text <- Map(":", text_start, text_end)

  # Extract content based on index and indicate type
  yaml <- lapply(list(yaml), function(i) extract_block(i, rmd, type = "yaml"))
  chunk <- lapply(chunk, function(i) extract_block(i, rmd, type = "chunk"))
  text <- lapply(text, function(i) extract_block(i, rmd, type = "markdown"))

  # Join, order by start-index and return
  out <- c(yaml, text, chunk)
  out <- out[order(vapply(out, function(x) x$start, numeric(1)))]
  out
}

extract_block <- function(i, rmd, type) {
  structure(list(content = rmd[i], start = min(i), end = max(i)), class = c(type, "list"))
}
