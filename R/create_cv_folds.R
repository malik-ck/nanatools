### TO DO
### Proper CV creation and estimation in cases where both inner_cv and outer_cv are NULL
### Essentially just runs the learners then on the full data
### Also, make sure this code is good. Read it and understand it fully.
### Also, the vfold fun should by default have clustering arguments.
### y is currently assumed everywhere to be a vector. Adjust subsetting logic to admit matrices and data frames.
### Also, wrappers around make_cv_funs with more complex schemes would be nice. Maybe around create_cv_folds even?
### Also: Remove documentation from new_cv, add it to create_cv_folds and make_cv_funs


make_vfold_fun <- function(v) {
  force(v)
  function(n, ...) origami::make_folds(n = n, V = v,
                                       fold_fun = origami::folds_vfold)
}

# ══════════════════════════════════════════════════════════════════════════════
# nana_fold — single fold
# ══════════════════════════════════════════════════════════════════════════════

#' Construct a single cross-validation fold
#'
#' @param validation_set Integer vector of validation indices.
#' @param n Total number of observations. Required when \code{training_set}
#'   is \code{NULL}; the training set is then the complement of
#'   \code{validation_set} within \code{1:n}.
#' @param training_set Integer vector of training indices, or \code{NULL}
#'   (default) to derive as the complement.
#' @param excluded Integer vector of indices excluded from both sets (e.g.
#'   censored observations). Defaults to \code{NULL}.
#' @return An S3 object of class \code{nana_fold}.
#' @export
new_fold <- function(validation_set,
                     n            = NULL,
                     training_set = NULL,
                     excluded     = NULL) {
  if (is.null(training_set) && is.null(n))
    stop("Either `training_set` or `n` must be provided.")

  validation_set <- as.integer(validation_set)
  excluded       <- if (!is.null(excluded)) as.integer(excluded) else NULL

  # Determine storage type
  if (!is.null(training_set)) {
    training_set <- as.integer(training_set)
    # Promote to complementary if the sets actually are complementary
    if (!is.null(n)) {
      full <- seq_len(as.integer(n))
      if (length(intersect(training_set, validation_set)) == 0L &&
          identical(sort(c(training_set, validation_set)), full)) {
        training_set <- NULL   # redundant — derive on access
      }
    }
  }

  structure(
    list(
      validation_set = validation_set,
      training_set   = training_set,
      n              = if (!is.null(n)) as.integer(n) else NULL,
      excluded       = excluded,
      complementary  = is.null(training_set)
    ),
    class = "nana_fold"
  )
}

# ── Accessors ──────────────────────────────────────────────────────────────

#' Extract training indices from a fold
#' @param fold A \code{nana_fold}.
#' @param ... Ignored.
#' @export
training_set <- function(fold, ...) UseMethod("training_set")

#' @export
training_set.nana_fold <- function(fold, ...) {
  idx <- if (fold$complementary)
    setdiff(seq_len(fold$n), fold$validation_set)
  else
    fold$training_set
  if (!is.null(fold$excluded)) idx <- setdiff(idx, fold$excluded)
  idx
}

#' Extract validation indices from a fold
#' @param fold A \code{nana_fold}.
#' @param ... Ignored.
#' @export
validation_set <- function(fold, ...) UseMethod("validation_set")

#' @export
validation_set.nana_fold <- function(fold, ...) {
  idx <- fold$validation_set
  if (!is.null(fold$excluded)) idx <- setdiff(idx, fold$excluded)
  idx
}

# ── exclude() generic ──────────────────────────────────────────────────────

#' Exclude indices from a fold or fold list
#'
#' Returns a new object with the specified indices removed from both training
#' and validation sets on access. The original object is not modified.
#'
#' @param x A \code{nana_fold} or \code{nana_fold_list}.
#' @param indices Integer vector of indices to exclude.
#' @param ... Ignored.
#' @export
exclude <- function(x, indices, ...) UseMethod("exclude")

#' @export
exclude.nana_fold <- function(x, indices, ...) {
  x$excluded <- unique(c(x$excluded, as.integer(indices)))
  x
}

# ── print ──────────────────────────────────────────────────────────────────

#' @export
print.nana_fold <- function(x, ...) {
  tr  <- training_set(x)
  val <- validation_set(x)
  cat(sprintf(
    "nana_fold | %s | train: %d | val: %d%s\n",
    if (x$complementary) "complementary" else "explicit",
    length(tr), length(val),
    if (!is.null(x$excluded) && length(x$excluded) > 0L)
      sprintf(" | %d excluded", length(x$excluded)) else ""
  ))
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# nana_fold_list — ordered collection of folds
# ══════════════════════════════════════════════════════════════════════════════

#' Construct a list of folds
#'
#' @param folds A list of \code{nana_fold} objects.
#' @return An S3 object of class \code{nana_fold_list}.
#' @export
new_fold_list <- function(folds) {
  if (!is.list(folds) || !all(vapply(folds, inherits, logical(1L), "nana_fold")))
    stop("All elements must be `nana_fold` objects.")
  structure(folds, class = c("nana_fold_list", "list"))
}

#' @export
exclude.nana_fold_list <- function(x, indices, ...) {
  indices <- as.integer(indices)
  new_fold_list(lapply(x, exclude, indices = indices))
}

#' @export
print.nana_fold_list <- function(x, ...) {
  n_excl <- sum(vapply(x, function(f)
    !is.null(f$excluded) && length(f$excluded) > 0L, logical(1L)))
  cat(sprintf(
    "nana_fold_list | %d fold(s)%s\n",
    length(x),
    if (n_excl > 0L) sprintf(" | %d fold(s) with exclusions", n_excl) else ""
  ))
  invisible(x)
}

# ══════════════════════════════════════════════════════════════════════════════
# nana_cv — full nested CV structure
# ══════════════════════════════════════════════════════════════════════════════

#' Construct a nested cross-validation object
#'
#' @param performance_sets A \code{nana_fold_list} for outer (performance
#'   evaluation) folds, or \code{NULL}.
#' @param build_sets A list of \code{nana_fold_list} objects, one per outer
#'   fold, for inner (ensemble building) folds, or \code{NULL}.
#' @return An S3 object of class \code{nana_cv}.
#' @export
new_cv <- function(performance_sets = NULL, build_sets = NULL) {
  if (is.null(performance_sets) && is.null(build_sets))
    stop("At least one of `performance_sets` or `build_sets` must be non-NULL.")
  if (!is.null(performance_sets) && !inherits(performance_sets, "nana_fold_list"))
    stop("`performance_sets` must be a nana_fold_list or NULL.")
  if (!is.null(build_sets)) {
    if (!is.list(build_sets) ||
        !all(vapply(build_sets, inherits, logical(1L), "nana_fold_list")))
      stop("`build_sets` must be a list of nana_fold_list objects or NULL.")
  }
  structure(
    list(performance_sets = performance_sets,
         build_sets       = build_sets),
    class = "nana_cv"
  )
}

#' @export
exclude.nana_cv <- function(x, indices, ...) {
  new_cv(
    performance_sets = if (!is.null(x$performance_sets))
      exclude(x$performance_sets, indices) else NULL,
    build_sets = if (!is.null(x$build_sets))
      lapply(x$build_sets, exclude, indices = indices) else NULL
  )
}

#' @export
print.nana_cv <- function(x, ...) {
  cat("nana_cv\n")
  if (!is.null(x$performance_sets))
    cat(sprintf("  Performance folds : %d\n", length(x$performance_sets)))
  else
    cat("  Performance folds : none\n")
  if (!is.null(x$build_sets))
    cat(sprintf("  Build fold sets   : %d outer x %d inner\n",
                length(x$build_sets), length(x$build_sets[[1L]])))
  else
    cat("  Build fold sets   : none\n")
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════════════
# create_cv_folds — internal constructor
# ══════════════════════════════════════════════════════════════════════════════

#' Create cross-validation folds
#'
#' @param n Total number of observations.
#' @param cv_instructions Either an integer vector of length 2
#'   \code{c(inner_cv, outer_cv)} with \code{NA} for no CV at that level,
#'   a named list with \code{$inner_cv} and \code{$outer_cv} (each a
#'   positive integer, a function of \code{(n, ...)}, or \code{NA}/\code{NULL}),
#'   or a pre-built \code{nana_cv} object returned as-is.
#' @param ... Additional named arguments forwarded to custom fold functions
#'   that declare them (e.g. \code{cluster}, \code{time}).
#' @return A \code{nana_cv} object.
#' @export
create_cv_folds <- function(n, inner_cv = NA, outer_cv = NA, ...) {

  # ── Resolve cv_instructions to inner_cv / outer_cv ────────────────────────
  resolve_one <- function(x, arg_name) {
    if (is.function(x)) return(x)
    if (is.null(x) || (length(x) == 1L && is.na(x))) return(NULL)
    if (is.numeric(x) && length(x) == 1L && x >= 2L)
      return(make_vfold_fun(as.integer(x)))
    if (is.list(x)) return(x)
    stop(sprintf(
      "`%s`: must be NA/NULL (no CV), a positive integer >= 2, a function of (n, ...), or a pre-specified list of fold index sets.",
      arg_name
    ))
  }

  inner_cv <- resolve_one(inner_cv, "inner_cv")
  outer_cv  <- resolve_one(outer_cv, "outer_cv")

  if (is.null(inner_cv) && is.null(outer_cv))
    stop("At least one of `inner_cv` or `outer_cv` must be non-NA/NULL.")

  dots <- list(...)

  call_cv_fun <- function(fun, n) {
    declared <- names(formals(fun))
    if ("..." %in% declared)
      do.call(fun, c(list(n = n), dots))
    else
      do.call(fun, c(list(n = n), dots[intersect(names(dots), declared)]))
  }

  wrap_folds <- function(raw, n) {
    new_fold_list(lapply(raw, function(f)
      new_fold(
        validation_set = f$validation_set,
        training_set   = f$training_set,
        n              = n
      )
    ))
  }

  outer_raw <- if (is.null(outer_cv)) {
    list(list(training_set = seq_len(n), validation_set = seq_len(n)))
  } else if (is.function(outer_cv)) {
    call_cv_fun(outer_cv, n)
  } else {
    outer_cv
  }

  outer_fold_list <- wrap_folds(outer_raw, n)

  inner_fold_lists <- if (!is.null(inner_cv)) {
    lapply(outer_fold_list, function(outer_fold) {
      tr   <- training_set(outer_fold)
      n_tr <- length(tr)
      raw  <- if (is.function(inner_cv))
        call_cv_fun(inner_cv, n_tr)
      else
        inner_cv
      new_fold_list(lapply(raw, function(f)
        new_fold(
          validation_set = tr[f$validation_set],
          training_set   = tr[f$training_set],
          n              = n
        )
      ))
    })
  } else NULL

  new_cv(
    performance_sets = if (!is.null(outer_cv)) outer_fold_list else NULL,
    build_sets       = inner_fold_lists
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# add_cv_folds — pipeline wrapper
# ══════════════════════════════════════════════════════════════════════════════

#' Add cross-validation folds to a study task
#'
#' @param task A \code{nana_task} object.
#' @param inner_cv \code{NULL}, a positive integer (number of V-folds), or a
#'   function with first argument \code{n} returning a list of fold index
#'   sets. Used for inner (ensemble-building) CV. If a function also declares
#'   \code{cluster} or \code{time} as named arguments, these are forwarded
#'   automatically from the task.
#' @param outer_cv As \code{inner_cv}, but for outer (performance-evaluation)
#'   CV.
#' @param ... Additional named arguments forwarded to custom fold functions.
#'   Take precedence over task-derived values.
#' @return The \code{nana_task} with \code{fold_store} populated.
#' @export
add_cv_folds <- function(task, inner_cv = 5L, outer_cv = 5L, ...) {
  if (!inherits(task, "nana_task"))
    stop("`task` must be a nana_task object.")

  # Task-level extras forwarded to custom fold functions
  task_extras <- Filter(Negate(is.null), list(
    cluster = task$cluster,
    time    = task$time
  ))
  dots   <- list(...)
  extras <- c(dots, task_extras[setdiff(names(task_extras), names(dots))])

  cv <- create_cv_folds(task$n_obs, inner_cv, outer_cv, ...)

  # Store in a reference environment so SL init objects can share without copy
  if (is.null(task$fold_store))
    task$fold_store <- new.env(parent = emptyenv())
  task$fold_store$cv <- cv

  if (task$verbose) {
    perf_n  <- if (!is.null(cv$performance_sets))
      length(cv$performance_sets) else 0L
    build_n <- if (!is.null(cv$build_sets))
      length(cv$build_sets[[1L]]) else 0L
    message(sprintf(
      "CV folds created: %d outer (performance) fold(s), %d inner (build) fold(s).",
      perf_n, build_n
    ))
  }

  task
}


# ══════════════════════════════════════════════════════════════════════════════
# outcome_folds — per-outcome fold derivation
# ══════════════════════════════════════════════════════════════════════════════

#' Derive a nana_cv with censored observations excluded for a given outcome
#'
#' @param cv A \code{nana_cv} object.
#' @param task A \code{nana_task} with censoring indicators populated.
#' @param outcome_name Character. Name of the outcome in \code{task$outcomes}.
#' @return A \code{nana_cv} with censored observations excluded from both
#'   build and performance folds.
#' @export
outcome_folds <- function(cv, task, outcome_name) {
  if (!inherits(cv, "nana_cv"))
    stop("`cv` must be a nana_cv object.")
  if (!inherits(task, "nana_task"))
    stop("`task` must be a nana_task object.")
  if (!outcome_name %in% names(task$outcomes))
    stop(sprintf("Outcome '%s' not found in task.", outcome_name))

  cens <- task$censoring[[outcome_name]]
  if (is.null(cens)) return(cv)

  censored_idx <- which(cens == 0L)
  if (length(censored_idx) == 0L) return(cv)

  if (task$verbose) message(sprintf(
    "Outcome '%s': excluding %d censored observation(s) from CV folds.",
    outcome_name, length(censored_idx)
  ))

  exclude(cv, censored_idx)
}
