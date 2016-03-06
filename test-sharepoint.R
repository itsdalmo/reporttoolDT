# WORKING
library(httr)
test <- GET(lnk, authenticate("", "", type = "any"), write_disk(""))
test <- GET(lnk, authenticate("", "", type = "ntlm"), write_disk(""))

read_sp <- function(lnk, destination = NULL) {
  if (is.null(destination)) {
    destination <- tempfile(fileext = ".xlsx")
    on.exit(unlink(destination, recursive = TRUE, force = TRUE), add = TRUE)
  }

  # Todo - persistent "cookies" or something?
  usr <- readline("Enter username: ")
  psw <- readline("Enter password: ")

  # Todo - clean URL? Necessary or does httr handle it internally?
  resp <- httr::GET(utils::URLencode(lnk), authenticate(usr, psw, type = "ntlm"), httr::write_disk(destination))
  res <- readxl::read_excel(destination)
  res

}


# github_pat <- function(force = FALSE) {
#   env <- Sys.getenv('GITHUB_PAT')
#   if (!identical(env, "") && !force) return(env)
#
#   if (!interactive()) {
#     stop("Please set env var GITHUB_PAT to your github personal access token",
#          call. = FALSE)
#   }
#
#   message("Couldn't find env var GITHUB_PAT. See ?github_pat for more details.")
#   message("Please enter your PAT and press enter:")
#   pat <- readline(": ")
#
#   if (identical(pat, "")) {
#     stop("Github personal access token entry failed", call. = FALSE)
#   }
#
#   message("Updating GITHUB_PAT env var to PAT")
#   Sys.setenv(GITHUB_PAT = pat)
#
#   pat
# }