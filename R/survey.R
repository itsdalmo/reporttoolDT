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
      }
      self$data <- x
      self$update()
    },

    get_data = function(copy = TRUE) {
      if (copy && data.table::is.data.table(self$data)) {
        data.table::copy(self$data)
      } else {
        self$data
      }
    },

    initialize_subset = function(x) {
      "Return a sliced or subset survey."
      slice <- self$clone(deep = FALSE)
      slice$data <- x
      slice$update()
      slice
    },

    update = function() {
      "Update the survey. (Associations, labels, etc.)"
      self$set_association()
      self$set_label()
    },

    set_label = function(new = NULL) {
      "Set labels."
      new <- merge_attributes(self$names(), new, private$.labels)
      private$.labels <- new
      invisible(self)
    },

    set_association = function(new = NULL) {
      "Set associations."
      new <- merge_attributes(self$names(), new, private$.associations)
      private$.associations <- new
      invisible(self)
    },

    set_marketshare = function(new = NULL) {
      "Set marketshares."
      me <- self$get_associations("mainentity")
      if (is.null(me)) {
        stop("'mainentity' is not specified. See help(set_association).")
      } else {
        me <- self$data[[names(me)]]
        me <- if (is.factor(me)) levels(me) else unique(me)
      }

      new <- merge_attributes(me, private$.marketshares, new)
      private$.marketshares <- new
      invisible(self)
    },

    set_config = function(new = NULL) {
      "Set associations."
      new <- merge_attributes(default$config$setting, new, private$.config)
      private$.config <- new
      invisible(self)
    },

    get_labels = function(which = NULL) {
      "Get labels."
      res <- private$.labels
      if (!is.null(which))
        res <- res[match_all(which, names(res))]
      res
    },

    get_associations = function(which = NULL) {
      "Get associations."
      res <- private$.associations
      if (!is.null(which))
        res <- res[match_all(which, res)]
      res
    },

    get_marketshares = function(which = NULL) {
      "Get marketshares."
      res <- private$.marketshares
      if (!is.null(which))
        res <- res[match_all(which, res)]
      res
    },

    get_config = function(which = NULL) {
      "Get marketshares."
      res <- private$.marketshares
      if (!is.null(which))
        res <- res[match_all(which, res)]
      res
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

    entities = function() {
      me <- names(self$get_associations("mainentity"))
      if (!length(me) || is.null(me)) stop("'mainentity' has not been specified yet. See help(set_association).")

      cutoff <- as.numeric(self$get_config("cutoff"))
      valid <- !is.null(cutoff) && "percent_missing" %in% names(x)

      df <- data.table::as.data.table(self$get_data())
      df <- df[, list("n" = .N, "valid" = if (valid) sum(percent_missing <= cutoff) else NA_integer_), keyby = me]

      ms <- self$get_marketshares()
      if (!is.null(ms)) {
        ms <- setNames(list(names(ms), unname(ms)), c(me, "marketshare"))
        ms <- data.table::as.data.table(ms)
        df <- df[ms[, marketshare := as.numeric(marketshare)]]
      } else {
        df[, marketshare := NA_real_]
      }

      data.table::setnames(df, me, "entity")
      structure(as.data.frame(df), class = c("survey_entities", "data.frame"))
    },

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
