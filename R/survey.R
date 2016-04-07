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
  private = list(
    .labels = NULL,
    .config = NULL,
    .associations = NULL,
    .translations = NULL,
    .marketshares = NULL,

    # Get/set all private fields -----------------------------------------------
    all_fields = function() {
      list(
        labels = private$.labels,
        config = private$.config,
        associations = private$.associations,
        translations = private$.translations,
        marketshares = private$.marketshares
      )
    },

    # TODO: Check consistency with data?
    set_fields = function(fields) {
        private$.labels = fields$labels
        private$.config = fields$config
        private$.associations = fields$associations
        private$.translations = fields$translations
        private$.marketshares = fields$marketshares
    }

  ),

  public = list(

    data = NULL,

    initialize = function(x, fields = NULL) {
      if (missing(x) || !is.data.frame(x))
        stop("Expecting a data.frame or data.table.", call. = FALSE)
      if (any_labelled(x)) {
        # NOTE: from_labelled returns a data.frame. This can cause issues if data
        # is read from sav, converted to data.table and then initialized using survey().
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
      slice$data <- x
      slice
    },

    get_data = function(copy = TRUE) {
      if (copy && data.table::is.data.table(self$data)) {
        data.table::copy(self$data)
      } else {
        self$data
      }
    },

    # Mutate the Survey --------------------------------------------------------
    update = function(renamed = NULL) {
      "Update the survey. (Associations, labels, etc.)"
      if (!is.null(renamed)) {
        self$set_names(renamed)
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
      aso <- lapply(dots, function(x) { if (is.survey(x)) x$get_association() })

      # Unlist and assign to private fields (self$do will remove duplicates)
      private$.labels <- merge_vectors(self$get_label(), lbl)
      private$.associations <- merge_vectors(self$get_association(), aso)

      # Extract data and apply function
      dots <- lapply(dots, function(x) { if (is.survey(x)) x$get_data() else x })
      self$do(f, dots)
    },

    set_names = function(new_names) {
      "Set colnames/named vectors with new names."
      if (!length(new_names) == length(self$data))
        stop("set_names: New names must be of same length as the data.", call. = FALSE)
      if (data.table::is.data.table(self$data)) {
        data.table::setnames(self$data, new_names)
      } else {
        names(self$data) <- new_names
      }

      private$.labels <- setNames(self$get_label(), new_names)
      private$.associations <- setNames(self$get_association(), new_names)
      invisible(self)
    },

    names = function() {
      names(self$data)
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
      me <- names(self$get_association("mainentity"))
      if (!length(me) || is.null(me)) stop("'mainentity' has not been specified yet. See help(set_association).", call. = FALSE)

      cutoff <- as.numeric(self$get_config("cutoff"))
      valid <- !is.null(cutoff) && "percent_missing" %in% self$names()

      df <- data.table::as.data.table(self$get_data())
      df <- df[, list("n" = .N, "valid" = if (valid) sum(percent_missing <= cutoff) else NA_integer_), keyby = me]

      ms <- self$get_marketshare()
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

    # set/get individual private fields ----------------------------------------
    set_label = function(..., lst = NULL) {
      "Set labels."
      new <- merge_vectors(..., lst, self$get_label(), default = self$names())
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

    set_association = function(..., lst = NULL) {
      "Set associations."
      # Associations are specified as value = c(vars), i.e. we have to reverse name and value.
      lst <- c(list(...), lst)
      lst <- lapply(names(lst), function(nm) {
        x <- lst[[nm]]
        setNames(rep(stri_trans_tolower(nm), length(x)), x)
        })

      new <- merge_vectors(lst, self$get_association(), default = self$names())
      private$.associations <- new
      invisible(self)
    },

    get_association = function(which = NULL) {
      "Get associations."
      res <- private$.associations
      if (!is.null(res) && !is.null(which)) {
        res <- res[match_all(which, res)]
        if (!length(res)) res <- NULL
      }
      res
    },

    set_marketshare = function(..., lst = NULL) {
      "Set marketshares."
      ent <- self$get_association("mainentity")
      if (is.null(ent)) {
        stop("'mainentity' is not specified. See help(set_association).", call. = FALSE)
      } else if (length(ent) > 1L) {
        stop("More than one 'mainentity' specified. See help(set_association).", call. = FALSE)
      } else {
        ent <- self$data[[names(ent)]]
        ent <- if (is.factor(ent)) levels(ent) else unique(ent)
      }

      new <- merge_vectors(..., lst, self$get_marketshare(), default = ent)
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

    set_config = function(..., lst = NULL) {
      "Set config."
      def <- get_default("config")
      def <- setNames(def$value, def$required)
      new <- merge_vectors(..., lst, self$get_config(), default = def)
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

    set_translation = function(..., lst = NULL, language = NULL) {
      "Set translation."
      def <- get_default("translation")

      # Get language from internal_defaults (partial match/case insensitive)
      if (!is.null(language)) {
        found <- stri_detect(names(def), fixed = language, ignore_case = TRUE)
        if (!any(found)) stop("The specified language was not found.")
        def <- setNames(def[[which(found)]], def$required)
      } else {
        def <- def$required
      }

      new <- merge_vectors(..., lst, self$get_translation(), default = def)
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

    print = function(...) {
      print(self$data)
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
