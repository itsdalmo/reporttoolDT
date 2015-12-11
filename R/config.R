#' @export
get_config <- function(srv, config = NULL, arrange = TRUE) {
  x <- attr(srv, which = "config")
  if (!is.null(x) && !is.null(config)) {
    if (arrange) {
      id <- names(x) %ordin% config
    } else {
      id <- names(x) %in% config
    }
    x <- x[id]
  }
  unname(x)
}

#' @export
set_config <- function(srv, ..., list = NULL) {
  srv <- data.table::copy(srv)
  set_attr(srv, "config", c(base::list(...), list), match_names = TRUE)
  srv
}