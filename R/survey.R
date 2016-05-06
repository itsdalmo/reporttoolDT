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


# Survey (R6 Class) ------------------------------------------------------------
#' @importFrom R6 R6Class
Survey <- R6::R6Class("Survey",


  # Private methods ------------------------------------------------------------
  private = list(
    .labels = NULL,
    .config = NULL,
    .associations = NULL,
    .translations = NULL,
    .marketshares = NULL,

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
        x <- officeR::from_labelled(x)
      }
      # Copy labels from attr (returns null if they do not exist.)
      private$.labels <- attr(x, "labels")

      # Set fields if provided
      if (!is.null(fields))
        private$set_fields(fields)

      self$data <- x
      self$update()
    },

    initialize_subset = function(x) {
      "(Re)Initialize a sliced/subset survey."
      if (requireNamespace("dplyr") && is_tbl(x)) {
        slice <- self$as_tbl(clone = TRUE)
      } else if (data.table::is.data.table(x)) {
        slice <- self$as_dt(clone = TRUE)
      } else {
        slice <- self$as_df(clone = TRUE)
      }
      if (!identical(class(slice), class(self))) {
        new <- class(slice); old <- class(self)
        warning("Class has changed from ", setdiff(old, new), " to ", setdiff(new, old), call. = FALSE)
      }
      slice$data <- x
      slice
    },

    names = function() {
      names(self$data)
    },

    print = function(...) {
      print(self$data)
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

    do = function(f, dots, renamed = NULL) {
      "Perform operations directly on the Survey."
      # Original call is 2 layers up at this point. parent.frame(n = 2L)
      res <- do.call(f, c(list(self$data), dots), envir = parent.frame(n = 2L))

      if (identical(data.table::address(res), data.table::address(self$data))) {
        invisible(self$update(renamed))
      } else {
        if (is.data.frame(res)) {
          self$initialize_subset(res)$update(renamed)
        } else {
          res
        }
      }
    },

    do_merge = function(f, dots) {
      "Do merging operations on a Survey."
      # Get labels and associations
      lbl <- lapply(dots, function(x) { if (is.survey(x)) x$get_label() })
      aso <- lapply(dots, function(x) { if (is.survey(x)) x$get_association(invert = FALSE) })

      # Unlist and assign to private fields (self$do will remove duplicates)
      private$.labels <- merge_vectors(private$.labels, lbl)
      private$.associations <- merge_vectors(private$.associations, aso)

      # Extract data and apply function
      dots <- lapply(dots, function(x) { if (is.survey(x)) x$get_data() else x })
      self$do(f, dots)
    },

    set_names = function(new_names) {
      "Set colnames/named vectors with new names."
      if (data.table::is.data.table(self$data)) {
        data.table::setnames(self$data, new_names)
      } else {
        names(self$data) <- new_names
      }
      self$update_field_names(new_names)
      invisible(self)
    },

    update_field_names = function(new_names) {
      "Update fields with new names."
      private$.labels <- setNames(private$.labels, new_names)
      private$.associations <- setNames(private$.associations, new_names)
      invisible(self)
    },

    # Accessors ----------------------------------------------------------------
    get_data = function(copy = TRUE) {
      "Return a copy of the data."
      if (copy && data.table::is.data.table(self$data)) {
        data.table::copy(self$data)
      } else {
        self$data
      }
    },

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

    set_label = function(..., list = NULL) {
      "Set labels."
      new <- merge_vectors(..., list, private$.labels, default = self$names())
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
      if ("percent_missing" %in% self$names()) {
        cutoff <- self$get_config("cutoff")
        cutoff <- if (is.null(cutoff)) NA else as.numeric(cutoff)
        dt <- dt[, list("n" = .N, "valid" = sum(percent_missing <= cutoff)), keyby = me]
      } else {
        dt <- dt[, list("n" = .N, "valid" = NA_integer_), keyby = me]
      }

      ms <- self$get_marketshare()
      if (!is.null(ms)) {
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

# S3 methods -------------------------------------------------------------------
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
  x$do("[<-", capture_dots(...))
}

#' @export
`[[<-.Survey` <- function(x, ...) {
  x$do("[[<-", capture_dots(...))
}

#' @export
names.Survey <- function(x) {
  x$names()
}

#' @export
`names<-.Survey` <- function(x, value) {
  x$set_names(value)
}

#' @importFrom utils head
#' @export
head.Survey <- function(x, ...) {
  f <- get("head", asNamespace("utils"))
  x$do(f, list(...))
}

#' @importFrom utils tail
#' @export
tail.Survey <- function(x, ...) {
  f <- get("tail", asNamespace("utils"))
  x$do(f, list(...))
}

#' @export
dimnames.Survey <- function(x) {
  # NOTE: Might need to use data.table::copy() here.
  dimnames(x$data)
}

#' @export
dim.Survey <- function(x) {
  dim(x$data)
}

#' @export
length.Survey <- function(x) {
  length(x$data)
}
