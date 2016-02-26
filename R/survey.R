#' @importFrom R6 R6Class
#' @export
Survey <- R6::R6Class("Survey",
  private = list(
    .associations = NULL,
    .labels = NULL,
    .config = NULL,
    .dictionary = NULL,
    .marketshares = NULL
  ),

  public = list(

    data = NULL,

    initialize = function(x) {
      if (missing(x) || !is.data.frame(x))
        stop("Expecting a data.frame or data.table.", call = FALSE)
      if (is.labelled(x)) {
        x <- from_labelled(x, copy = FALSE)
        private$.labels <- attr(x, "labels")
        attr(x, "labels") <- NULL
      }
      self$data <- x
      self$update()
    },

    initialize_subset = function(x) {
      "Return a sliced or subset survey."
      slice <- self$copy()
      slice$data <- x
      slice$update()
      slice
    },

    copy = function() {
      "Return a copy of the Survey."
      new <- self$clone(deep = FALSE)
      if (data.table::is.data.table(self$data))
        # Invalid .internal.selfref when using copy. setDT shallow copies instead.
        new$data <- data.table::setDT(self$data)
      new
    },

    update = function() {
      "Update the survey. (Associations, labels, etc.)"
      self$set_associations()
      self$set_labels()
    },

    set_labels = function() {
      "Set labels."
      cols <- self$names()
      new <- setNames(rep(NA_character_, length(cols)), cols)
      old <- private$.labels

      if (!is.null(old)) {
        old <- old[!is.na(old)]
        new <- merge_vectors(if (length(old)) old else NULL, new)
        new <- new[cols]
      }
      private$.labels <- new
    },

    set_associations = function() {
      "Set associations."
      cols <- self$names()
      new <- setNames(rep(NA_character_, length(cols)), cols)
      old <- private$.associations

      if (!is.null(old)) {
        old <- old[!is.na(old)]
        new <- merge_vectors(if (length(old)) old else NULL, new)
        new <- new[cols]
      }
      private$.associations <- new
    },

    model = function() {
      "Return the measurement model"
      mm <- list(
        latent = private$.associations,
        manifest = self$names(),
        question = private$.labels,
        type = vapply(self$data, function(x) class(x)[1], character(1)),
        levels = vapply(self$data, function(x) {
          l <- levels(x); if (is.null(l)) NA_character_ else stri_c(l, collapse = "\n")
        }, character(1))
      )

      na <- rep(NA, ncol(self$data))
      mm <- lapply(mm, function(x) { if (is.null(x)) na else x })
      mm <- as.data.frame(mm, stringsAsFactors = FALSE)
      structure(mm, class = c("survey_model", "data.frame"))
    },

#     entities = function() {
#
#       me <- self$get_association("mainentity")
#       if (is.null(me)) stop("'mainentity' has not been specified yet. See help(set_association).")
#
#       x <- data.table::copy(x); setkeyv(x, me)
#       co <- as.numeric(get_config(x, "cutoff"))
#
#       # Aggregate
#       val <- !is.null(co) && "percent_missing" %in% names(x)
#       x <- x[, list("n" = .N, "valid" = if (val) sum(percent_missing <= co) else NA), by = me]
#
#       ms <- get_marketshare(x)
#       if (!is.null(ms)) {
#         ms <- setNames(list(names(ms), unname(ms)), c(me, "marketshare"))
#         ms <- as.data.table(ms)
#         x <- x[ms[, marketshare := as.numeric(marketshare)]]
#       } else {
#         x[, marketshare := NA]
#       }
#
#       setkeyv(x, NULL)
#       setnames(x, me, "entity")
#       structure(x, class = c("survey_ents", "data.table", "data.frame"))
#
#     },

    names = function() {
      names(self$data)
    },

    print = function(...) {
      print(self$data)
    }
  )
)

# as/is ------------------------------------------------------------------------
#' @export
survey <- function(x) UseMethod("survey")

#' @export
is.survey <- function(x) inherits(x, "Survey")

#' @export
as.survey <- function(x) UseMethod("as.survey")

#' @export
as.survey.Survey <- function(x) x

#' @export
as.survey.default <- function(x) survey(x)

# Names ------------------------------------------------------------------------
#' @export
names.Survey <- function(x) x$names()

#' @export
dimnames.Survey <- function(x) {
  dimnames(x$data)
}

# Subset/alter -----------------------------------------------------------------
#' @export
`[.Survey` <- function(x, ...) {
  x$do("[", capture_dots(...))
}

#' @export
`[[.Survey` <- function(x, ...) {
  x$do("[[", capture_dots(...))
}

#' @export
`[<-.Survey` <- function(x, ...) {
  x$do("[<-", capture_dots(...), assign = TRUE)
}

#' @export
`[[<-.Survey` <- function(x, ...) {
  x$do("[[<-", capture_dots(...), assign = TRUE)
}
