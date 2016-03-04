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