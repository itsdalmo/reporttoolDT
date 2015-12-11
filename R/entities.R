entities <- function(x) {
  stopifnot(is.survey(x))

  me <- get_association(x, "mainentity")
  co <- get_association(x, "cutoff")
  ms <- get_association(x, "marketshare")
  if (is.null(me)) stop("'mainentity' has not been specified yet. See help(set_association).")

  x <- data.table::copy(x)
  setkeyv(x, me)
  x <- x[, list("n" = .N, "valid" = if (!is.null(co) && "percent_missing" %in% names(x)) sum(percent_missing <= co) else NA), by = me]

  if (!is.null(ms)) {
    ms <- setNames(list(names(ms), unname(ms)), c(me, "marketshare"))
    ms <- as.data.table(ms)
    x[ms]
  } else {
    x[, marketshare := NA]
  }

  x

}


#' @export
get_marketshare <- function(srv, entities = NULL, arrange = TRUE) {
  x <- get_attr(srv, which = "marketshares", matches = entities, arrange = arrange, match_names = TRUE)
  x
}

set_marketshare <- function(srv, ..., list = NULL) {
  srv <- data.table::copy(srv)
  entities <- get_association(srv, "mainentity")
  if (is.null(entities)) stop("Attribute 'mainentity' has not been specified.")
  ms <- get_attributes(srv, "marketshares")
  if (is.null(ms$marketshares)) {
    ms <- update_attribute(unique(srv[[entities]]), old = ms$marketshares)
    setattr(srv, "marketshares", ms)
  }
  set_attr(srv, "marketshares", c(base::list(...), list), match_names = TRUE)
}
