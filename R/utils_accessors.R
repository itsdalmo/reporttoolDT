# Utility function that merges named vectors for private fields in Survey's.
# Duplicates are dropped from the end of the named vector (after unlisting).
merge_vectors <- function(..., .current = NULL, .default = NULL, .check = TRUE) {
  vals <- unlist(list(...))
  if (!length(vals) && is.null(.default)) {
    stop("No vectors supplied.")
  }

  if (!is.null(.default)) {
    # Use unnamed default's as names for a vector of NA's.
    # (So we can pass colummnames as a default.)
    if (is.null(names(.default))) {
      .default <- setNames(rep(NA, length(.default)), .default)
    }
    # Make sure specified variables exist in defaults.
    missing <- names(vals)[!names(vals) %in% names(.default)]
    if (.check && length(missing)) {
      stop("The following arguments do not match default values:\n", str_list(missing), call. = FALSE)
    }
  }

  out <- c(vals, .current, .default)
  if (!is_named(out)) stop("All elements must be named.")
  out <- out[!duplicated(names(out), fromLast = FALSE)]

  if (!is.null(.default)) {
    out <- out[names(.default)]
  }

  out

}

# Reuse variable labels for EM variables, and translations for latents.
# (Only replaces NA's, not existing labels.)
auto_label <- function(fields) {
  labs <- fields$labels
  asso  <- fields$associations

  em <- NULL # EM variables
  lv <- NULL # Latent variables

  # Get a list of variables with EM prefix and a missing label
  is_em <- stri_detect(names(labs), regex = "em$", case_insensitive = TRUE)
  is_na <- is.na(labs)

  # Reuse labels from non-EM variables
  if (any(is_em) && any(is_na)) {
    em <- names(labs)[is_em & is_na]
    la <- stri_replace(em, "$1", regex = "(.*)em$", case_insensitive = TRUE)
    la <- match_all(stri_trans_tolower(la), stri_trans_tolower(names(labs)))
    em <- setNames(labs[la], em)
  }

  # Also set translation as label for latents if possible
  trans <- fields$translations
  is_lat <- stri_trans_tolower(names(labs)) %in% default_latents()

  if (!is.null(trans) && any(is_lat)) {
    lv <- names(labs)[is_lat & is_na]
    lb <- trans[match_all(stri_trans_tolower(lv), names(trans))]
    lv <- setNames(lb, lv)
  }

  # Return
  c(em, lv)

}

# Function to detect common associations in variable names, based
# on list in default values.
common_latents <- function(var) {
  stopifnot(is.character(var))

  pattern <- get_default("associations")
  out <- vector("character", length(var))
  for (latent in names(pattern)) {
    match <- pattern[[latent]]

    if (length(match) == 1L) {
      match <- suppressWarnings(stri_c("^", match, "[[:alpha:]]*$"))
    } else {
      match <- suppressWarnings(stri_c("^", match, "$", collapse = "|"))
    }

    # Always exclude variable names that end with "em".
    include <- stri_detect(var, regex = match, case_insensitive = TRUE)
    exclude <- stri_detect(var, regex = "em$", case_insensitive = TRUE)

    out[include & !exclude] <- latent

  }

  # Suggest q1 as mainentity if it exists
  is_me <- stri_detect(var, regex = "^q1$", case_insensitive = TRUE)
  if (any(is_me)) out[is_me] <- "mainentity"

  # Set remaining values to NA and return
  out[out == ""] <- NA
  out

}

# We specify c(value = name) to avoid repetition when specifying model-related
# associations. This function reverses the process and returns c(name = value).
names_as_values <- function(x) {
  name <- names(x); value <- unname(x)

  if (is_list(value)) {
    is_null <- vapply(value, is.null, logical(1L))
    is_atomic <- vapply(value, is.atomic, logical(1L))

    # Make sure we have not recieved a list of e.g. data.frames.
    if (any(is_null) || !all(is_atomic))
      stop("Cannot reverse lists that contain non-atomic vectors.")

    # Rep names to match length of value
    name <- lapply(seq_along(value), function(i) rep(name[[i]], length(value[[i]])))

    # Unlist
    name <- unlist(name)
    value <- unlist(value)
  }

  # Return
  setNames(name, value)

}