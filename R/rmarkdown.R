evaluate_rmd <- function(rmd, env = new.env()) {
  blk <- split_blocks(rmd)

  # Loop through blocks and evaluate them
  for (i in seq_along(blk)) {
    blk[[i]] <- eval_block(blk[[i]], env)
  }

  blk

}


eval_block <- function(x, env) UseMethod("eval_block")

eval_block.markdown <- function(x, env) {
  blk <- x$content
  pat <- get_default("pat_rmd")

  inline <- which(stri_detect(blk, regex = pat$inline))
  if (length(inline)) {
    for (i in inline) {

      # Extract all inline expressions in the given line w/inline code
      code <- unlist(stringi::stri_extract_all(blk[i], regex = pat$inline))
      expr <- stringi::stri_replace_all(code, "", regex = "`r\\s?|\\s?`")

      # Loop through the expressions and replace
      for (ii in seq_along(expr)) {
        res <- evaluate::evaluate(expr[[ii]], envir = env, output_handler = rmd_handler)
            print(str(res))
        res <- res[!vapply(res, inherits, what = "source", logical(1))]
        if (length(res)) {
          is_atomic <- vapply(res, is.atomic, logical(1))
          if (!all(is_atomic) || is.null(res)) {
            res <- " "
          } else {
            res <- stri_replace(unlist(res), "", fixed = "\n")
          }
          blk[i] <- stri_replace(blk[i], as.character(res), fixed = code[ii])
        }
      }
    }
  } else if (all(blk == "")) {
    blk <- NULL
  }

  # Return
  x$result <- blk
  x

}

eval_block.yaml <- eval_block.markdown

eval_block.chunk <- function(x, env) {
  x
}

rmd_handler <- evaluate::new_output_handler(
  value = function(x, visible = TRUE) {
    if (!visible) return()
    if (inherits(x, "FlexTable") || is.data.frame(x)) {
      x
    } else {
      print(x)
    }
  }
)


# Split rmd into groups --------------------------------------------------------
split_blocks <- function(rmd, pattern = get_default("pat_rmd")) {
  # Check input (either a string or a character vector)
  if (!is.character(rmd)) stop("Expecting a character vector as input.")
  if (is_string(rmd)) {
    if (!any(stri_detect(rmd, fixed = "\n")))
      stop("The string input does not contain any new lines.")
    rmd <- stri_split(rmd, fixed = "\n")
  }

  n <- length(rmd)

  # Detect patterns associated with chunks and their indicies
  chunk_start <- stri_detect(rmd, regex = pattern$chunk_start)
  chunk_end <- stri_detect(rmd, regex = pattern$chunk_end)

  # Get indicies for chunks
  # (Using sum() stri_detect returns logical vectors)
  if (sum(chunk_start) != sum(chunk_end))
    stop("Detected unushed delimiters (```) for code chunks.")
  chunk <- Map(":", which(chunk_start), which(chunk_end))

  # Get YAML delimiters that are not in a chunk.
  yaml <- stri_detect(rmd, regex = pattern$yaml_start) & !(1:n %in% unlist(chunk))
  if (sum(yaml) %% 2 == 1L) stop("Detected unushed delimiters (---) for yaml fronmatter.")
  yaml <- which(yaml)[1]:which(yaml)[2]

  # Rest of the document should be text and/or inline code
  # (yaml_end:1st_chunk_start, 1st_chunk_end:2nd_chunk_start ... last_c_e:end)
  text_start <- c(max(yaml) + 1L, which(chunk_end) + 1L)
  text_end <- c(which(chunk_start) - 1L, n)
  if (length(text_start) != length(text_end))
    stop("Indicies for text start and end are not same length. (Should not happen).")
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
