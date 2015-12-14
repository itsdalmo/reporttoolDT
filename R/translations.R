#' @export
get_translation <- function(srv, translations = NULL, arrange = TRUE) {
  srv <- get_attr(srv, which = "translations", matches = translations, arrange = arrange, match_names = TRUE)
  srv
}

#' @export
set_translation <- function(srv, ..., list = NULL) {
  srv <- data.table::copy(srv)
  set_attr(srv, "translations", c(base::list(...), list), match_names = TRUE)
  srv
}