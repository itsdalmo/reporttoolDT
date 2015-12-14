#' @export
get_config <- function(srv, config = NULL, arrange = TRUE) {
  srv <- get_attr(srv, which = "config", matches = config, arrange = arrange, match_names = TRUE)
  srv
}

#' @export
set_config <- function(srv, ..., list = NULL) {
  srv <- data.table::copy(srv)
  set_attr(srv, "config", c(base::list(...), list), match_names = TRUE)
  srv
}