# Uses split_file + eval_block to return a list of evaluated blocks ------------
# (Should not contain nested)
evaluate_rmd <- function(rmd, env = new.env()) {
  blk <- split_file(rmd)
  out <- list()

  # Loop through blocks and evaluate them (in turn).
  for (i in seq_along(blk)) {
    b_i <- blk[[i]]
    out <- c(out, eval_block(b_i, env = env, split = !inherits(b_i, "yaml")))
  }

  # Drop missing and return
  is_missing <- vapply(out, is.null, logical(1)) | lengths(out) == 0L
  out[!is_missing]

}

# Handler for evaluate ---------------------------------------------------------
# We simply want to return visible objects without mimicking console etc.
rmd_handler <- evaluate::new_output_handler(
  value = function(x, visible = TRUE) {
    if (!visible) return()
    x
  }
)

# Evaluate blocks by type ------------------------------------------------------
eval_block <- function(blk, env, split) UseMethod("eval_block")

# CHUNK (split is unused.)
eval_block.chunk <- function(blk, env, split = FALSE) {
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

  # Return
  res

}

# MARKDOWN
eval_block.markdown <- function(blk, env, split = TRUE) {
  lines <- blk$content
  pat <- get_default("pat_rmd")

  # Handle inline code (in order using a for loop.)
  inline <- which(stri_detect(lines, regex = pat$inline))
  if (length(inline)) {
    for (i in inline) {
      # Extract all inline code
      code <- unlist(stringi::stri_extract_all(lines[i], regex = pat$inline))
      expr <- stringi::stri_replace_all(code, "", regex = "`r\\s?|\\s?`")

      # Exaluate (and drop source code)
      expr <- evaluate::evaluate(expr, envir = env, output_handler = rmd_handler)
      expr <- expr[!vapply(expr, inherits, what = "source", logical(1))]

      # Replace code with result
      for (ii in seq_along(expr)) {
        lines[i] <- stri_replace(lines[i], expr[[ii]] %||% "", fixed = code[ii])
      }
    }
  }

  # Return NULL if all lines are empty.
  if (all(lines == "")) return()

  if (split) {
    split_markdown(lines)
  } else {
    list(lines)
  }

}

# Split markdown blocks into sections, titles and content ----------------------
split_markdown <- function(x) {
  n <- length(x)
  pat <- get_default("pat_rmd")

  is_section <- stri_detect(x, regex = pat$section)
  is_slide <- stri_detect(x, regex = pat$slide)

  # Return early if x is a string, or there are no section/slide markers.
  if (n == 1L || !any(is_section) && !any(is_slide)) return(x)

  # Get indicies for markers and actual content (non empty lines)
  markers <- which(is_section | is_slide)
  contents <- setdiff(which(x != ""), markers)

  start <- c(markers + 1L)
  end <- c(markers[-1L] - 1L, n)

  # Content starts before the first marker in the current block.
  if (min(contents) < min(markers)) {
    start <- c(1L, start[-1L])
    end <- c(markers[1L], end)
  }

  # No content after last marker (Marker belongs to the next block.)
  if (max(contents) < max(markers)) {
    start <- start[-length(start)]
    end <- end[-length(end)]
  }

  # List indicies for each piece of markdown code.
  markdown <- Map(":", start, end)

  # Order and extract. Drop if all lines == "".
  out <- c(as.list(markers), markdown)
  out <- out[order(vapply(out, min, numeric(1)))]
  out <- lapply(out, function(i) { md <- x[i]; if (all(md == "")) NULL else md})

  # Return
  out

}

# YAML
eval_block.yaml <- eval_block.markdown

# Split RMD file into blocks ---------------------------------------------------
split_file <- function(rmd, pattern = get_default("pat_rmd")) {
  # Check input (either a string or a character vector)
  if (!is.character(rmd)) stop("Expecting character vector as input.")
  if (is_string(rmd)) {
    if (!any(stri_detect(rmd, fixed = "\n")))
      stop("The string does not contain any new lines.")
    rmd <- stringi::stri_split(rmd, fixed = "\n")
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

