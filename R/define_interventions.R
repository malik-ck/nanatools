
#' Validate that an intervention returns a result compatible with the treatment
#'
#' @param task A \code{nana_task} object.
#' @param intervention_name Character. Name of the intervention to validate.
#' @return Invisibly returns the intervened treatment value if valid.
#'   Stops with an informative error if not.
#' @export
validate_intervention <- function(task, intervention_name) {
  if (!inherits(task, "nana_task"))
    stop("`task` must be a nana_task object.")
  if (is.null(task$interventions))
    stop("No interventions defined. Run define_interventions() first.")
  if (is.null(task$interventions[[intervention_name]]))
    stop(sprintf("Intervention '%s' not found in task.", intervention_name))

  result <- tryCatch(
    intervene(task, intervention_name),
    error = function(e) stop(sprintf(
      "Intervention '%s' errored when called:\n  %s",
      intervention_name, conditionMessage(e)
    ))
  )

  trt <- task$treatment

  # Class check
  trt_class  <- class(trt)[[1L]]
  res_class  <- class(result)[[1L]]
  if (!identical(trt_class, res_class))
    stop(sprintf(
      "Intervention '%s': intervene() returned class '%s' but treatment is '%s'.\n%s",
      intervention_name, res_class, trt_class,
      "The intervention function must return the same class as the treatment block."
    ))

  # Dimension check — works for vectors, matrices, and data frames
  trt_dim <- if (is.null(dim(trt))) length(trt) else dim(trt)
  res_dim <- if (is.null(dim(result))) length(result) else dim(result)

  if (!identical(trt_dim, res_dim))
    stop(sprintf(
      "Intervention '%s': intervene() returned %s but treatment has %s.\n%s",
      intervention_name,
      if (length(res_dim) == 1L) sprintf("length %d", res_dim)
      else paste(res_dim, collapse = " x "),
      if (length(trt_dim) == 1L) sprintf("length %d", trt_dim)
      else paste(trt_dim, collapse = " x "),
      "The intervention function must return a result with the same dimensions as the treatment block."
    ))

  # Numeric check — all entries must be numeric after intervention
  vals <- if (is.data.frame(result)) unlist(result) else as.vector(result)
  if (!is.numeric(vals))
    stop(sprintf(
      "Intervention '%s': intervene() returned non-numeric values (class '%s').\n%s",
      intervention_name, class(vals)[[1L]],
      "All treatment values must be numeric after intervention."
    ))

  if (task$verbose) message(sprintf(
    "Intervention '%s': validated successfully.", intervention_name
  ))

  invisible(result)
}



#' Create a causal intervention object
#'
#' @param intervene A function with signature \code{function(a, l)} where
#'   \code{a} is a numeric vector of observed treatment values and \code{l} is
#'   a data frame or matrix of covariates (one row per observation). Must
#'   return a numeric vector of length \code{length(a)} representing the
#'   intervened treatment values. For stochastic interventions, the function
#'   should return a single draw per observation; multiple draws for variance
#'   reduction are handled internally via \code{n_draws}.
#' @param mtp Logical. If \code{TRUE}, the density ratio is estimated via the
#'   2n augmented dataset classification approach, which is required when the
#'   intervention depends on the natural value of treatment. If \code{FALSE},
#'   the density ratio is computed directly from the propensity score model.
#' @param stochastic Logical. If \code{TRUE}, the plug-in step averages
#'   \code{n_draws} random draws from the intervention distribution per
#'   observation for Monte Carlo variance reduction. Requires \code{intervene}
#'   to be a random function.
#' @param n_draws Positive integer. Number of Monte Carlo draws per observation
#'   used in the plug-in step when \code{stochastic = TRUE}. Ignored when
#'   \code{stochastic = FALSE}. Defaults to 100L.
#' @param label Optional character string for display in print methods and
#'   result tables.
#'
#' @return An S3 object of class \code{nana_intervention}.
#' @export
intervention <- function(intervene,
                         mtp        = FALSE,
                         stochastic = FALSE,
                         n_draws    = 100L,
                         label      = NULL) {

  if (!is.function(intervene))
    stop("`intervene` must be a function.")
  params <- names(formals(intervene))
  if (length(params) < 2L)
    stop("`intervene` must accept at least two arguments: (a, l).")
  if (!is.logical(mtp) || length(mtp) != 1L)
    stop("`mtp` must be a single logical value.")
  if (!is.logical(stochastic) || length(stochastic) != 1L)
    stop("`stochastic` must be a single logical value.")
  if (!is.null(label) && (!is.character(label) || length(label) != 1L))
    stop("`label` must be a single character string or NULL.")
  if (stochastic) {
    if (!is.numeric(n_draws) || length(n_draws) != 1L || n_draws < 1L)
      stop("`n_draws` must be a positive integer when `stochastic = TRUE`.")
    n_draws <- as.integer(n_draws)
  } else {
    n_draws <- NULL
  }

  structure(
    list(
      intervene  = intervene,
      mtp        = mtp,
      stochastic = stochastic,
      n_draws    = n_draws,
      label      = label
    ),
    class = "nana_intervention"
  )
}

#' @export
define_interventions <- function(task, ...) {
  if (!inherits(task, "nana_task"))
    stop("`task` must be a nana_task object.")

  interventions <- list(...)

  if (length(interventions) == 0L)
    stop("At least one intervention must be provided.")
  if (is.null(names(interventions)) || any(names(interventions) == ""))
    stop("All interventions must be named.")

  bad <- !vapply(interventions, inherits, logical(1L), "nana_intervention")
  if (any(bad))
    stop(sprintf(
      "The following are not valid intervention objects: %s\n%s",
      paste(names(interventions)[bad], collapse = ", "),
      "Use intervention(), or a wrapper like intervention_static()."
    ))

  # Propagate argument name as label if none was set
  interventions <- lapply(names(interventions), function(nm) {
    obj <- interventions[[nm]]
    if (is.null(obj$label)) obj$label <- nm
    obj
  })
  names(interventions) <- names(list(...))

  task$interventions <- interventions

  # Validate interventions
  for (nm in names(interventions)) validate_intervention(task, nm)

  task
}

# ── Some methods ────────────────────────────────────────────────────────

#' @export
print.nana_intervention <- function(x, ...) {
  lab      <- if (!is.null(x$label)) x$label else "(unlabelled)"
  mtp_str  <- if (x$mtp) "MTP" else "non-MTP"
  sto_str  <- if (x$stochastic)
    sprintf("stochastic [M = %d]", x$n_draws) else "deterministic"
  cat(sprintf("nana_intervention | %s | %s | %s\n", lab, mtp_str, sto_str))
  invisible(x)
}

#' Get one realization of an intervention
#'
#' @param study A study, which is a \code{list} of class \code{nana_task}.
#' @param intervention_name Name of the intervention to apply to the study.
#' @export
intervene <- function(study, intervention_name) {
  UseMethod("intervene")
}

#' @export
intervene.nana_task <- function(study, intervention_name) {

  # Check existence of interventions
  if (is.null(study$interventions)) {
    stop("No interventions found. Please use define_interventions() on the study before intervening.")
  }

  # Check existence of intervention
  if (is.null(study$interventions[[intervention_name]])) {
    stop("Intervention not found.")
  }

  get_intervention_function <- study$interventions[[intervention_name]]$intervene

  get_intervention_function(study$treatment, study$confounders)

}

# ── Common wrappers ────────────────────────────────────────────────────────

#' Single-arm intervention: set one treatment column to 1, all others to 0
#'
#' @param column Column name or integer index identifying the treatment arm to
#'   activate. All other treatment columns are set to 0.
#' @param label Optional display label.
#' @export
intervention_arm <- function(column, label = NULL) {
  if (!is.character(column) && !is.numeric(column))
    stop("`column` must be a column name or integer index.")
  force(column)
  intervention(
    intervene = function(a, l) {
      idx <- if (is.character(column)) which(colnames(a) == column) else as.integer(column)
      if (length(idx) == 0L || idx < 1L || idx > ncol(a))
        stop(sprintf("Column '%s' not found in treatment matrix.", column))
      a[] <- 0
      a[, idx] <- 1
      a
    },
    mtp        = FALSE,
    stochastic = FALSE,
    label      = label %||% paste0("Arm: ", column)
  )
}

#' Static intervention: set treatment to a fixed value for all observations
#'
#' @param value A single numeric value.
#' @param label Optional display label.
#' @export
static_intervention <- function(value, label = NULL) {
  if (!is.numeric(value) || length(value) != 1L)
    stop("`value` must be a single numeric scalar.")
  force(value)
  intervention(
    intervene  = function(a, l) {
      a[] <- value
      a
    },
    mtp        = FALSE,
    stochastic = FALSE,
    label      = label %||% paste0("A = ", value)
  )
}

#' MTP intervention: shift treatment as a deterministic function of (a, l)
#'
#' @param shift_fn A function \code{function(a, l)} returning the shifted
#'   treatment value.
#' @param label Optional display label.
#' @export
mtp_intervention <- function(shift_fn, label = NULL) {
  if (!is.function(shift_fn))
    stop("`shift_fn` must be a function.")
  intervention(
    intervene  = shift_fn,
    mtp        = TRUE,
    stochastic = FALSE,
    label      = label
  )
}

#' Stochastic MTP intervention: draw treatment from a shifted distribution
#'
#' @param draw_fn A function \code{function(a, l)} returning one random draw
#'   per observation from the post-intervention distribution.
#' @param n_draws Number of Monte Carlo draws per observation for the plug-in
#'   step. Defaults to 100L.
#' @param label Optional display label.
#' @export
stochastic_intervention <- function(draw_fn, n_draws = 100L, label = NULL) {
  if (!is.function(draw_fn))
    stop("`draw_fn` must be a function.")
  intervention(
    intervene  = draw_fn,
    mtp        = TRUE,
    stochastic = TRUE,
    n_draws    = n_draws,
    label      = label
  )
}

#' Pure stochastic intervention: draw from a distribution not depending on
#' natural treatment value. Density ratio estimated directly from propensity
#' score model rather than via the 2n classification approach.
#'
#' @param draw_fn A function \code{function(a, l)} returning one random draw
#'   per observation. The \code{a} argument may be ignored.
#' @param n_draws Number of Monte Carlo draws per observation for the plug-in
#'   step. Defaults to 100L.
#' @param label Optional display label.
#' @export
pure_stochastic_intervention <- function(draw_fn, n_draws = 100L, label = NULL) {
  if (!is.function(draw_fn))
    stop("`draw_fn` must be a function.")
  intervention(
    intervene  = draw_fn,
    mtp        = FALSE,
    stochastic = TRUE,
    n_draws    = n_draws,
    label      = label
  )
}
