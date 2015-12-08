#' @export
get_association <- function(srv, associations, arrange = TRUE) {
  x <- get_attr(srv, which = "associations", matches = associations, arrange = arrange, match_names = FALSE)
  names(x)
}

#' @export
set_association <- function(srv, ...) {
  set_attr(srv, "associations", list(...), match_names = FALSE)
}

#' @export
get_labels <- function(srv, associations, arrange = TRUE) {
  x <- get_attr(srv, which = "labels", matches = associations, arrange)
  unname(x)
}

#' @export
set_labels <- function(srv, ...) {
  set_attr(srv, "labels", list(...))
}