#' Create a new Survey
#'
#' Create a new Survey from a \code{data.frame}, \code{data.table} or \code{tbl}.
#' The class \code{Survey} provides additional private (hidden) fields pertaining
#' to the Survey/data, but should otherwise behave like a regular \code{data.frame}.
#'
#' @section Privat fields (R6):
#' \describe{
#'
#'    \item{\code{labels}}{A label (question text) associated with a
#'    given variable in the data.}
#'
#'    \item{\code{association}}{Specification of what the variable is associated
#'    with. This field is used to specify the structural model for the \code{PLS-PM}
#'    modelling, in addition to other variables of interest for an analysis.}
#'
#'    \item{\code{config}}{This field keeps track of configurations made by
#'    the user when working on the \code{Survey}.}
#'
#'    \item{\code{translations}}{A dictionary containing translations to be
#'    used when generating output based on Survey.}
#'
#'    \item{\code{marketshares}}{The marketshares used for individual entities
#'    when producing weighted averages/counts from the Survey.}
#'
#' }
#'
#' @section Public methods (R6):
#' \describe{
#'
#'    \item{\code{new}}{Initialize a new \code{Survey}. Expects a \code{data.frame},
#'    \code{data.table} or \code{tbl} as input.}
#'
#'    \item{\code{do}}{Do arbitrary operations on the \code{Survey}. First argument
#'    should be the function, second a \code{list} of arguments for \code{do.call}.}
#'
#'    \item{\code{get_data}}{Return a copy of the data.}
#'
#'    \item{\code{model}}{Return a summary of the data for the \code{Survey}.
#'    This includes labels and associations, and the object (\code{survey_model})
#'    prints nicely.}
#'
#'    \item{\code{entities}}{This method produces a summary of the entities
#'    (total/valid observations and marketshare) if they have been specified.}
#'
#' }
#'
#' @param x A \code{data.frame}, \code{data.table} or \code{tbl} (requires dplyr).
#' @author Kristian D. Olsen
#' @note Under the hood, the \code{Survey} is a R6 class - this means that \code{$} is
#' reserved for accessing the public methods of the class, and not columns in the
#' data directly. S3 methods are provided (and prefered) to make the \code{Survey}
#' behave more like regular R objects.
#' @export
#' @examples
#' # Create a new survey (regular)
#' df <- survey_df(data.frame("A" = 1, "B" = 2))
#' dt <- survey_dt(data.frame("A" = 1, "B" = 2))
#'
#' if (require(dplyr)) {
#'   tbl <- survey_tbl(data.frame("A" = 1, "B" = 2))
#' }

survey <- function(x) UseMethod("survey")

#' @export
survey.data.frame <- function(x) survey_df(x)

#' @export
survey.data.table <- function(x) survey_dt(x)

#' @export
survey.tbl <- function(x) survey_tbl(x)

#' @export
survey.Survey <- function(x) x

#' @rdname survey
#' @export
as.survey <- function(x) UseMethod("as.survey")

#' @export
as.survey.Survey <- function(x) x

#' @export
as.survey.default <- function(x) survey(x)

#' @rdname survey
#' @export
is.survey <- function(x) inherits(x, "Survey")

#' @rdname survey
#' @export
survey_dt <- function(x) {
  if (!is.survey(x)) {
    out <- Survey$new(x)
  } else {
    out <- x$clone(deep = TRUE)
  }
  out$as_dt()
  out
}

#' @rdname survey
#' @export
survey_tbl <- function(x) {
  if (!is.survey(x)) {
    out <- Survey$new(x)
  } else {
    out <- x$clone(deep = TRUE)
  }
  out$as_tbldf()
  out
}

#' @rdname survey
#' @export
survey_df <- function(x) {
  if (!is.survey(x)) {
    out <- Survey$new(x)
  } else {
    out <- x$clone(deep = TRUE)
  }
  out$as_df()
  out
}

# Survey (R6 Class) ------------------------------------------------------------
#' @importFrom R6 R6Class
#' @importFrom R6Frame R6Frame
Survey <- R6::R6Class("Survey",
  inherit = R6Frame::R6Frame,


  # Private methods ------------------------------------------------------------
  private = list(
    .labels = NULL,
    .config = NULL,
    .associations = NULL,
    .translations = NULL,
    .marketshares = NULL,
    .inner_weight = NULL,
    .outer_weight = NULL,

    # TODO: Check consistency with data?
    set_fields = function(fields) {
        private$.labels = fields$labels
        private$.config = fields$config
        private$.associations = fields$associations
        private$.translations = fields$translations
        private$.marketshares = fields$marketshares
    }

  ),

  # Public methods -------------------------------------------------------------
  public = list(
    data = NULL,
    initialize = function(x, fields = NULL) {
      if (missing(x) || !is.data.frame(x))
        stop("Expecting a data.frame or data.table.", call. = FALSE)
      if (any_labelled(x)) {
        x <- seamless::from_labelled(x)
      }
      # Copy labels from attr (returns null if they do not exist.)
      private$.labels <- attr(x, "labels")
      attr(x, "labels") <- NULL

      # Set fields if provided
      if (!is.null(fields))
        private$set_fields(fields)

      # R6 Frame init
      super$initialize(x)
    },

    # Operations ---------------------------------------------------------------
    update = function(renamed = NULL) {
      "Update the survey. (Associations, labels, etc.)"
      if (!is.null(renamed)) {
        self$update_field_names(renamed)
      }
      self$set_association()
      self$set_label()
      self
    },

    do_merge = function(f, dots, env) {
      "Do merging operations on a Survey."
      # Get labels and associations
      lbl <- lapply(dots, function(x) { if (is.survey(x)) x$get_label() })
      aso <- lapply(dots, function(x) { if (is.survey(x)) x$get_association(invert = FALSE) })

      # Unlist and assign to private fields (self$do will remove duplicates)
      private$.labels <- merge_vectors(private$.labels, lbl)
      private$.associations <- merge_vectors(private$.associations, aso)

      # Extract data and apply functions
      super$do_merge(f, dots, env = env)
    },

    set_names = function(new_names) {
      "Set colnames/named vectors with new names."
      super$set_names(new_names)$update_field_names(new_names)
      invisible(self)
    },

    update_field_names = function(new_names) {
      "Update fields with new names."
      private$.labels <- setNames(private$.labels, new_names)
      private$.associations <- setNames(private$.associations, new_names)
      invisible(self)
    },

    # Accessors ----------------------------------------------------------------
    get_field = function() {
      "Get all hidden fields."
      fields <- list(
        labels = private$.labels,
        config = private$.config,
        associations = private$.associations,
        translations = private$.translations,
        marketshares = private$.marketshares
      )
      fields
    },

    set_label = function(..., list = NULL, auto = FALSE) {
      "Set labels."
      em <- if (auto) auto_label(self$get_field()) else NULL
      new <- merge_vectors(..., list, em, private$.labels, default = self$names())
      private$.labels <- new
      invisible(self)
    },

    get_label = function(which = NULL) {
      "Get labels."
      res <- private$.labels
      if (!is.null(res) && !is.null(which)) {
        res <- res[match_all(which, names(res))]
        if (!length(res)) res <- NULL
      }
      res
    },

    set_association = function(..., list = NULL, common = FALSE) {
      "Set associations."
      new <- c(list(...), list)
      old <- private$.associations
      def <- self$names()

      # Associations are specified as value = c(vars) and have to be reversed.
      # (Associations should also be lower-case, and none == no association.)
      new <- names_as_values(new)
      new <- setNames(stri_trans_tolower(new), names(new))
      new[new == "none"] <- NA

      # Optionally: Set common latents in defaults.
      if (common) {
        com <- setNames(common_latents(def), def)
      } else {
        com <- NULL
      }

      private$.associations <- merge_vectors(new, com, old, default = def)
      invisible(self)
    },

    get_association = function(which = NULL, invert = TRUE) {
      "Get associations."
      res <- private$.associations
      if (!is.null(res) && !is.null(which)) {
        res <- res[match_all(which, res)]
        if (!length(res)) res <- NULL
      }
      # Invert names/values when returning associations.
      if (invert) {
        res <- setNames(names(res), unname(res))
      }
      res
    },

    set_marketshare = function(..., list = NULL) {
      "Set marketshares."
      ent <- self$get_association("mainentity")
      if (is.null(ent)) {
        stop("'mainentity' is not specified. See help(set_association).", call. = FALSE)
      } else if (length(ent) > 1L) {
        stop("More than one 'mainentity' specified. See help(set_association).", call. = FALSE)
      } else {
        ent <- self$data[[ent]]
        ent <- if (is.factor(ent)) levels(ent) else unique(ent)
      }

      new <- merge_vectors(..., list, private$.marketshares, default = ent)
      private$.marketshares <- new
      invisible(self)
    },

    get_marketshare = function(which = NULL) {
      "Get marketshares."
      res <- private$.marketshares
      if (!is.null(res) && !is.null(which)) {
        res <- res[match_all(which, names(res))]
        if (!length(res)) res <- NULL
      }
      res
    },

    set_config = function(..., list = NULL) {
      "Set config."
      def <- get_default("config")
      def <- setNames(def$value, def$required)
      new <- merge_vectors(..., list, private$.config, default = def)
      private$.config <- new
      invisible(self)
    },

    get_config = function(which = NULL) {
      "Get config."
      res <- private$.config
      if (!is.null(res) && !is.null(which)) {
        res <- res[match_all(which, names(res))]
        if (!length(res)) res <- NULL
      }
      res
    },

    set_translation = function(..., list = NULL, language = NULL) {
      "Set translation."
      def <- get_default("translation")

      # Get language from internal_defaults (partial match/case insensitive)
      if (!is.null(language)) {
        found <- stri_detect(names(def), fixed = language, ignore_case = TRUE)
        if (!any(found)) stop("The specified language was not found.")
        lang <- setNames(def[[which(found)]], def$required)
      } else {
        lang <- NULL
      }

      new <- merge_vectors(..., list, lang, private$.translations, default = def$required)
      private$.translations <- new
      invisible(self)
    },

    get_translation = function(which = NULL) {
      "Get translation."
      res <- private$.translations
      if (!is.null(res) && !is.null(which)) {
        res <- res[match_all(which, names(res))]
        if (!length(res)) res <- NULL
      }
      res
    },

    # Access weights -----------------------------------------------------------
    set_inner_weight = function(x) {
      private$.inner_weight <- x
    },

    get_inner_weight = function(which = NULL) {
      res <- private$.inner_weight
      if (!is.null(res) && !is.null(which)) {
        res <- res[which]
        if (!length(res) > 1L)
          res <- if (!length(res)) NULL else if (length(res) == 1L) res[[1L]]
      }
      res
    },

    set_outer_weight = function(x) {
      private$.outer_weight <- x
    },

    get_outer_weight = function(which = NULL) {
      res <- private$.outer_weight
      if (!is.null(res) && !is.null(which)) {
        res <- res[which]
        if (!length(res) > 1L)
          res <- if (!length(res)) NULL else if (length(res) == 1L) res[[1L]]
      }
      res
    },

    # Model and entities -------------------------------------------------------
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
      "Return information on entities."
      me <- self$get_association("mainentity")
      if (!length(me) || is.null(me))
        stop("'mainentity' is not specified. See help(set_association).")

      dt <- data.table::as.data.table(self$get_data())
      pm <- unname(self$get_association("percent_missing"))
      if (!is.null(pm) && pm != "percent_missing")
        data.table::setnames(dt, pm, "percent_missing")
      if ("percent_missing" %in% names(dt)) {
        cutoff <- self$get_config("cutoff")
        cutoff <- if (is.null(cutoff)) NA else as.numeric(cutoff)
        dt <- dt[, list("n" = .N, "valid" = sum(percent_missing <= cutoff)), keyby = me]
      } else {
        dt <- dt[, list("n" = .N, "valid" = NA_integer_), keyby = me]
      }

      ms <- self$get_marketshare()
      if (!is.null(ms) && all(names(ms) %in% dt[[me]])) {
        ms <- setNames(list(names(ms), unname(ms)), c(me, "marketshare"))
        ms <- data.table::as.data.table(ms)
        # Join on mainentity, convert ms to numeric and drop entities with
        # NA observations. I.e., not part of the original result
        dt <- dt[ms[, marketshare := as.numeric(marketshare)]][!is.na(n), ]
      } else {
        dt[, marketshare := NA_real_]
      }

      data.table::setnames(dt, me, "entity")
      structure(as.data.frame(dt), class = c("survey_entities", "data.frame"))
    }
  )
)

#' @export
.datatable.aware <- TRUE

