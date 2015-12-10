entities <- function(x) {
  stopifnot(is.survey(x))

  me <- get_association(x, "mainentity")
  co <- get_association(x, "cutoff")
  ms <- get_association(x, "marketshare")
  if (is.null(m)) stop("'mainentity' has not been specified yet. See help(set_association).")

  x <- data.table::copy(x)
  setkeyv(x, me)
  x <- x[, list("n" = .N, "valid" = if (!is.null(co) && "percent_missing" %in% names(x)) sum(percent_missing <= co) else NA), by = m]

  if (!is.null(ms)) {
    ms <- setNames(list(names(ms), unname(ms)), c(me, "marketshare"))
    ms <- as.data.table(ms)
    x[ms]
  } else {
    x[, marketshare := NA]
  }

  x

}