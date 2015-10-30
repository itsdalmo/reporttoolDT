#' @export
get_default <- function(string) {

  if (!is.string(string)) stop("Input was not a string (character(1)).")

  y <- default[stri_detect(names(default), regex = string, ignore_case = TRUE)]

  # Drop list if only one entry is returned
  if (length(y) == 1L) y[[1]] else y

}

# Default values ---------------------------------------------------------------

default <- list(

  "palette" =  c("#F8766D", "#00BFC4", "#808080", "#00BF7D", "#9590FF", "#A3A500", "#EA8331"),
  "latents" = c("image", "expect", "prodq", "servq", "value", "epsi", "loyal"),
  "na_strings" = c("NA", " ", "", "#DIV/0!", "#NULL!", "#NAVN?", "#NAME?"),

  "structure" = list(
    "survey" = c("df", "cd", "hd", "ents", "mm", "tr", "cfg"),
    "sheet" = c("data", "contrast data", "historic data", "entities", "measurement model", "translations", "config"),
    "ents" = c("entity", "n", "valid", "marketshare"),
    "mm" = c("latent", "manifest", "question", "type", "values"),
    "tr" = c("original", "replacement"),
    "cfg" = c("config", "value")),

  "model" = rbind("image" = c(0,0,0,0,0,0,0),
                  "expect" = c(1,0,0,0,0,0,0),
                  "prodq" = c(1,1,0,0,0,0,0),
                  "servq" = c(1,1,1,0,0,0,0),
                  "value" = c(0,0,1,1,0,0,0),
                  "epsi" = c(1,0,1,1,1,0,0),
                  "loyal" = c(0,0,0,0,0,1,0)),

  "associations" = list("image" = "q4",
                        "expect" = "q5",
                        "prodq" = "q7p",
                        "servq" = "q7s",
                        "value" = "q8",
                        "epsi" = c("q3", "q6", "q16"),
                        "loyal" = c("q10", "q15", "q15b")),

  "translation" = list(
    "required" = c("image", "expect", "prodq", "servq", "value", "epsi", "loyal",
                   "mainentity", "subentity", "manifest", "difference", "question",
                   "contrast_average", "average", "study_average", "spring", "fall"),
    "norwegian" = c("Image/inntrykk", "Forventninger", "Produktkvalitet", "Servicekvalitet",
                    "Verdi for pengene", "Kundetilfredshet", "Lojalitet", "Bank",
                    "Avdeling", "Kode", "Diff", "Spørsmål", "Snitt nasjonal", "Snitt",
                    "Snitt webstudien", "Vår", "Høst"),
    "danish" = c("Image", "Forventninger", "Produktkvalitet", "Servicekvalitet",
                 "Værdi for pengene", "Kundetilfredshed", "Loyalitet", "Bank",
                 "Afdeling", "Kode", "Diff", "Spørgsmål", "Snit national", "Snit",
                 "Snit webstudien", "Forår", "Efterår")),

  "config" = list(
    "setting" = c("reporttool", "study", "segment", "year", "period", "method",
                  "language", "cutoff", "latents", "marketshares"),
    "value" = c("1.4", "Barnehage", "", "2015", "fall", "web", "norwegian", .3, "mean", "no")
  ),

  "template" = list(
    "beamer" = list(
      "dir" = "rmd/beamer",
      "files" = "beamer_template.tex")),

  "theme" = list(
    "beamer" = list(
      "dir" = "rmd/beamer",
      "files" = c("beamercolorthememetropolis.sty",
                  "beamerfontthememetropolis.sty",
                  "beamerthemem.sty", "logo.eps",
                  "beamer_preamble.tex"))),

  "pattern" = list(

    "detect_scale" = "^[0-9]{1,2}[[:alpha:][:punct:] ]*",
    "extract_scale" = "^[0-9]{1,2}\\s*=?\\s*([[:alpha:]]*)",

    "rmd" = list(
      "chunk_start" = "^```\\{r",
      "chunk_end" = "```$",
      "chunk_eval" = ".*eval\\s*=\\s*((.[^},]+|.[^}]+\\))),?.*",
      "inline" = "`r[ [:alnum:][:punct:]][^`]+`",
      "section" = "^#[^#]",
      "slide" = "^##[^#]"),

    "code" = list(
      "yaml" = "^##\\+ ---",
      "inline" = "`r[ [:alnum:][:punct:]][^`]+`",
      "title" = "^##\\+\\s*#{1,2}[^#]",
      "text" = "^##\\+\\s.*"))

)