#' Get default values used internally.
#'
#' This function retrieves internally defined values where the names match the
#' \code{string} argument (not case sensitive). \code{default_latents} and
#' \code{default_palette} are identical to \code{get_default("latents")}.
#'
#' @param string A string which matches the default values you would like to return.
#' @param exact If the \code{string} matches more than one default value and exact
#' is set to \code{TRUE} an error occurs. The error lists the full name of all
#' matching internal defaults. Set this to \code{FALSE} to instead return all
#' matches instead.
#' @author Kristian D. Olsen
#' @export
#' @examples
#' lats <- get_default("latents")
#' pal <- get_default("palette")
#'
#' identical(lats, default_latents())
#' identical(pal, default_palette())

get_default <- function(string, exact = TRUE) {
  if (!is_string(string)) {
    stop("Expecting a string (character(1)) input for argument 'string'.")
  }

  id <- stri_detect(names(internal_defaults), fixed = string, ignore_case = TRUE)
  id <- names(internal_defaults)[id]
  if (length(id) > 1L && exact) {
    stop("The string matched more than one element:\n", str_list(id))
  }

  res <- internal_defaults[id]
  if (length(res) == 1L)
    res <- res[[1]]

  res

}

#' @rdname get_default
#' @export
default_palette <- function() get_default("palette", exact = TRUE)

#' @rdname get_default
#' @export
default_latents <- function() get_default("latents", exact = TRUE)

# Default values ---------------------------------------------------------------

internal_defaults <- list(

  # Default palette
  palette = c("#2FABB7", "#F04E36", "#747678", "#4C72B0", "#55A868", "#C44E52",
              "#8172B2", "#CCB974", "#FFC000", "#004A52", "#0091A1", "#BFBFBF"),

  # CSI latent names
  latents =   c("image", "expect", "prodq", "servq", "value", "epsi", "loyal"),

  # The inner model used in PLS-PM. The 'plspm' package expects a matrix.
  model = rbind(image  = c(0,0,0,0,0,0,0),
                expect = c(1,0,0,0,0,0,0),
                prodq  = c(1,1,0,0,0,0,0),
                servq  = c(1,1,1,0,0,0,0),
                value  = c(0,0,1,1,0,0,0),
                epsi   = c(1,0,1,1,1,0,0),
                loyal  = c(0,0,0,0,0,1,0)),

  # Common names for variables associated with a given latent.
  associations = list(image  = "q4",
                      expect = "q5",
                      prodq  = "q7p",
                      servq  = "q7s",
                      value  = "q8",
                      epsi   = c("q3", "q6", "q16"),
                      loyal  = c("q10", "q15", "q15b")),

  # Translations used for generating reports.
  translation = list(
    required = c("image",
                 "expect",
                 "prodq",
                 "servq",
                 "value",
                 "epsi",
                 "loyal",
                 "complaints",
                 "mainentity",
                 "subentity",
                 "manifest",
                 "difference",
                 "question",
                 "average",
                 "study",
                 "contrast",
                 "spring",
                 "fall"),

    english = stringi::stri_unescape_unicode(
      c("Image",
        "Expectations",
        "Product quality",
        "Service quality",
        "Value",
        "Customer satisfaction",
        "Loyalty",
        "Complaints",
        "Bank",
        "Branch",
        "Code",
        "Diff",
        "Question",
        "Average",
        "Web study",
        "National",
        "Spring",
        "Fall")
    ),

    norwegian = stringi::stri_unescape_unicode(
      c("Image/inntrykk",
        "Forventninger",
        "Produktkvalitet",
        "Servicekvalitet",
        "Verdi for pengene",
        "Kundetilfredshet",
        "Lojalitet",
        "Klager",
        "Bank",
        "Avdeling",
        "Kode",
        "Diff",
        "Sp\\u00f8rsm\\u00e5l",
        "Snitt",
        "Webstudien",
        "Nasjonal",
        "V\\u00e5r",
        "H\\u00f8st")
    ),

    swedish = stringi::stri_unescape_unicode(
      c("Image",
        "F\\u00f6rv\\u00e4ntningar",
        "Produktkvalitet",
        "Service",
        "Prisv\\u00e4rdhet",
        "Kundn\\u00f6jdhet",
        "Lojalitet",
        "Klagom\\u00e5l",
        "Bank",
        "Avdelning",
        "Kod",
        "Diff",
        "Fr\\u00e5ga",
        "Snitt",
        "Webstudien",
        "Nationell",
        "V\\u00e5r",
        "H\\u00f6st")
    ),

    danish = stringi::stri_unescape_unicode(
      c("Image",
        "Forventninger",
        "Produktkvalitet",
        "Servicekvalitet",
        "V\\u00e6rdi for pengene",
        "Kundetilfredshed",
        "Loyalitet",
        "Klage",
        "Bank",
        "Afdeling",
        "Kode",
        "Diff",
        "Sp\\u00f8rgsm\\u00e5l",
        "Snit",
        "Webstudien",
        "National",
        "For\\u00e5r",
        "Efter\\u00e5r")
    ),

    finnish = stringi::stri_unescape_unicode(
      c("Imago",
        "Odotukset",
        "Tuotelaatu",
        "Palvelulaatu",
        "Koettu Lisv\u00e4arvo",
        "Asiakastyytyv\u00e4isyys",
        "Uskollisuus",
        "Valitukset",
        "Pankki",
        "Toimiala",
        "Koodi",
        "Ero",
        "Kysymys",
        "Keskiarvo",
        "Web-tutkimus",
        "Kansallinen",
        "Kev\u00e4t",
        "Syksy")
    )),

  # Default config values for the .config private field in Survey's.
  config = list(
    required = c("name", "segment", "timestamp", "method", "cutoff", "model"),
    value = c("Example", NA, format(Sys.time(), "%Y-%m-%d"), "web", NA, NA)),

  # Locations for package-internal files.
  template = list(
    beamer = list(
      dir = "rmd/beamer",
      files = "beamer_template.tex")),

  theme = list(
    beamer = list(
      dir = "rmd/beamer",
      files = c("beamer_preamble.tex",
                "beamer_template.tex",
                "beamercolorthememetropolis.sty",
                "beamerfontthememetropolis.sty",
                "beamerthemem.sty",
                "logo.eps"))),

  # List (nested) of regex patterns used internally.
  pat_rmd = list(
      chunk_start = "^[\t >]*```+\\s*\\{[.]?([a-zA-Z]+.*)\\}\\s*$",
      chunk_end = "^[\t >]*```+\\s*$",
      yaml_start = "^---\\s*$",
      yaml_end = "^---\\s*$",
      yaml = "^\\s*([[:alnum:]]+):\\s*(.*)$",
      inline = "`r +([^`]+)\\s*`",
      section = "^#\\s+(.*)$",
      slide = "^##\\s+(.*)$")
)