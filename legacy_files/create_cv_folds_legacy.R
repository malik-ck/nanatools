
# Some helpers
make_vfold_fun <- function(v) {
  force(v)
  # Only needs n — ignores any extra args via ...
  function(n, ...) origami::make_folds(n = n, V = v,
                                       fold_fun = origami::folds_vfold)
}

cv_checker <- function(object, arg_name) {
  if (is.null(object)) return(invisible(NULL))
  if (is.function(object)) return(invisible(NULL))
  if (is.numeric(object) && length(object) == 1L && object >= 2L)
    return(invisible(NULL))
  if (is.list(object)) return(invisible(NULL))
  stop(sprintf(
    "`%s`: must be NULL, a positive integer >= 2, a function of (n, ...), ",
    "or a pre-specified list of fold index sets.",
    arg_name
  ))
}

make_cv_funs <- function(inner_cv, outer_cv) {
  cv_checker(inner_cv, "inner_cv")
  cv_checker(outer_cv, "outer_cv")

  if (is.null(inner_cv) && is.null(outer_cv))
    stop("At least one of `inner_cv` or `outer_cv` must be non-NULL.")

  list(
    inner_cv = if (is.numeric(inner_cv)) make_vfold_fun(inner_cv) else inner_cv,
    outer_cv = if (is.numeric(outer_cv)) make_vfold_fun(outer_cv) else outer_cv
  )
}

# The actual fold creator
create_cv_folds <- function(n, inner_cv, outer_cv, ...) {

  dots <- list(...)

  call_cv_fun <- function(fun, n) {
    # Pass only the named arguments the function actually declares,
    # plus n. This prevents errors when a simple function doesn't accept dots.
    declared <- names(formals(fun))
    if ("..." %in% declared) {
      do.call(fun, c(list(n = n), dots))
    } else {
      accepted <- intersect(names(dots), declared)
      do.call(fun, c(list(n = n), dots[accepted]))
    }
  }

  # ── Outer folds ───────────────────────────────────────────────────────────
  outer_fold_inds <- if (is.null(outer_cv)) {
    list(list(training_set = seq_len(n), validation_set = seq_len(n)))
  } else if (is.function(outer_cv)) {
    call_cv_fun(outer_cv, n)
  } else {
    outer_cv   # pre-specified list
  }

  # ── Inner folds (nested within each outer training set) ───────────────────
  inner_fold_inds <- vector("list", length(outer_fold_inds))

  for (i in seq_along(outer_fold_inds)) {
    n_inner <- length(outer_fold_inds[[i]]$training_set)

    inner_fold_inds[[i]] <- if (is.null(inner_cv)) {
      list(list(
        training_set   = seq_len(n_inner),
        validation_set = seq_len(n_inner)
      ))
    } else if (is.function(inner_cv)) {
      # Pass the inner n; dots may carry cluster/time subsets but that
      # requires the custom function to handle subsetting itself — documented
      # in the user-facing help for add_cv_folds
      call_cv_fun(inner_cv, n_inner)
    } else {
      inner_cv
    }
  }

  # ── Re-index inner folds to full-data indices ─────────────────────────────
  match_indices <- function(outer_set, inner_sets) {
    lapply(inner_sets, function(x) list(
      training_set   = outer_set$training_set[x$training_set],
      validation_set = outer_set$training_set[x$validation_set]
    ))
  }

  # ── Assemble ──────────────────────────────────────────────────────────────
  if (is.null(outer_cv)) {
    list(
      performance_sets = NULL,
      build_sets       = lapply(inner_fold_inds, function(flds) flds)
    )
  } else if (is.null(inner_cv)) {
    list(
      performance_sets = lapply(outer_fold_inds, function(x) list(
        training_set   = x$training_set,
        validation_set = x$validation_set
      )),
      build_sets = NULL
    )
  } else {
    list(
      performance_sets = lapply(outer_fold_inds, function(x) list(
        training_set   = x$training_set,
        validation_set = x$validation_set
      )),
      build_sets = mapply(match_indices,
                          outer_set   = outer_fold_inds,
                          inner_sets  = inner_fold_inds,
                          SIMPLIFY    = FALSE)
    )
  }
}


# The wrapper

#' Add cross-validation folds to a study task
#'
#' @param task A \code{nana_task} object.
#' @param inner_cv Either \code{NULL}, a positive integer (number of folds),
#'   or a function with first argument \code{n} used to construct inner
#'   (ensemble-building) folds. A function may also declare named arguments
#'   \code{cluster} and/or \code{time}, which will be forwarded automatically
#'   from the task.
#' @param outer_cv Either \code{NULL}, a positive integer, or a function of
#'   \code{n} used to construct outer (performance-evaluation) folds. Same
#'   rules as \code{inner_cv} for cluster/time.
#' @param ... Additional named arguments forwarded to custom fold functions.
#'   Values provided here take precedence over those extracted from the task.
#'
#' @return The \code{nana_task} with \code{cv_folds} populated.
#' @export
add_cv_folds <- function(task, inner_cv = 5L, outer_cv = 5L, ...) {
  if (!inherits(task, "nana_task"))
    stop("`task` must be a nana_task object.")

  cv_funs <- make_cv_funs(inner_cv, outer_cv)

  # Extract task-level structural variables as potential fold-function inputs.
  # User-supplied dots take precedence over task-derived values.
  task_extras <- list(
    cluster = task$cluster,
    time    = task$time
  )
  task_extras <- Filter(Negate(is.null), task_extras)

  dots <- list(...)
  extras <- c(dots, task_extras[setdiff(names(task_extras), names(dots))])

  task$cv_folds <- do.call(
    create_cv_folds,
    c(list(n        = task$n_obs,
           inner_cv = cv_funs$inner_cv,
           outer_cv = cv_funs$outer_cv),
      extras)
  )

  if (task$verbose) message(sprintf(
    "CV folds created: %s outer fold(s), %s inner fold(s) per outer fold.",
    if (is.null(task$cv_folds$performance_sets)) "none"
    else length(task$cv_folds$performance_sets),
    if (is.null(task$cv_folds$build_sets)) "none"
    else length(task$cv_folds$build_sets[[1L]])
  ))

  task
}
