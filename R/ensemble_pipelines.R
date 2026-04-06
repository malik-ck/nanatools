# List flattener
make_learner_list <- function(...) {
  dots <- list(...)
  is_learner <- function(x) inherits(x, "SL_Learner") || inherits(x, "SL_Metalearner")
  res <- lapply(dots, function(x) {
    if (is_learner(x)) {
      return(list(x)) # Pack into a list
    } else {
      return(x) # Is already a list
    }
  })
  # Flatten one level to get a single list of learners, check that all are now individual learners
  collected_learners <- do.call(c, res)
  if (!all(unlist(lapply(collected_learners, function(x) inherits(x, "SL_Learner") || inherits(x, "SL_Metalearner"))))) {
    stop("'collect_learners' can only handle individual learners of class 'SL_Leaerner' or 'SL_Metalearner' and lists of them.")
  }
  collected_learners
}

# ── Internal DAG helpers ───────────────────────────────────────────────────

# Each pipeline stage is a list of SL_Learner objects.
# The pipeline is stored as an ordered list of stages, where each stage
# contains one or more learners. Every learner in stage i+1 is fitted on
# the output of every learner in stage i, creating independent branches.
# A pipeline with stages list(A, list(B,C), D) produces branches:
#   A -> B -> D  (named "A/B/D")
#   A -> C -> D  (named "A/C/D")

# Enumerate all root-to-leaf paths through the stage list.
# Returns a list of integer vectors, each giving one path as stage indices
# (always one per stage since branching multiplies learners within stages,
# not across them — each path picks exactly one learner per stage).
enumerate_paths <- function(stages) {
  # Each stage contributes one index per learner in that stage.
  # A path picks exactly one learner from each stage.
  # Total paths = product of stage sizes.
  stage_sizes <- vapply(stages, length, integer(1L))
  n_paths     <- prod(stage_sizes)

  # Generate all combinations (like expand.grid but as a list of index vectors)
  paths <- vector("list", n_paths)
  for (p in seq_len(n_paths)) {
    idx    <- integer(length(stages))
    rem    <- p - 1L
    for (s in seq_along(stages)) {
      idx[[s]] <- (rem %% stage_sizes[[s]]) + 1L
      rem       <- rem %/% stage_sizes[[s]]
    }
    paths[[p]] <- idx
  }
  paths
}

# Build the name for a path by concatenating learner names with "/"
path_name <- function(stages, path_idx) {
  paste(vapply(seq_along(stages), function(s)
    stages[[s]][[path_idx[[s]]]]$name,
    character(1L)
  ), collapse = "/")
}


# ── make_pipeline ──────────────────────────────────────────────────────────

#' Make a pipeline of learners for use in ensembles
#'
#' @param ... Learners (\code{SL_Learner}) or lists of learners in the order
#'   they are to be executed. The first argument must be a single
#'   \code{SL_Learner}. Each subsequent argument receives the predictions of
#'   all learners in the previous argument as its \code{x}. If an argument
#'   is a list of learners, the pipeline branches — one independent branch
#'   per learner — multiplying the number of terminal outputs. The terminal
#'   outputs (last stage) are what the metalearner receives as
#'   \code{preds_list} entries.
#'
#' @details
#' Learners designed for pipeline use should have \code{predict} functions
#' that return objects usable as \code{x} for the next learner. A screener's
#' \code{predict} might return a column-subsetted matrix; a basis expander's
#' \code{predict} might return a transformed feature matrix; a standard
#' learner's \code{predict} returns a prediction vector. The pipeline places
#' no constraints on these types — compatibility between adjacent nodes is
#' the user's responsibility.
#'
#' @return An object of class \code{SL_Pipeline}.
#' @export
make_pipeline <- function(...) {
  raw <- list(...)

  if (length(raw) < 2L)
    stop("`make_pipeline` requires at least two arguments (a source learner and at least one successor).")

  # First argument must be a single learner
  if (!inherits(raw[[1L]], "SL_Learner"))
    stop("The first argument to `make_pipeline` must be a single `SL_Learner`.")

  # Resolve each argument into a flat list of SL_Learner objects
  stages <- lapply(seq_along(raw), function(i) {
    x <- raw[[i]]
    if (inherits(x, "SL_Learner")) {
      list(x)
    } else if (is.list(x)) {
      flat <- make_learner_list(x)
      if (!all(vapply(flat, inherits, logical(1L), "SL_Learner")))
        stop(sprintf(
          "Argument %d to `make_pipeline` contains non-SL_Learner objects.",
          i
        ))
      flat
    } else {
      stop(sprintf(
        "Argument %d to `make_pipeline` must be an SL_Learner or a list of SL_Learners.",
        i
      ))
    }
  })

  # Enumerate all root-to-leaf paths and name them
  paths     <- enumerate_paths(stages)
  path_nms  <- vapply(paths, function(p) path_name(stages, p), character(1L))

  if (length(unique(path_nms)) != length(path_nms))
    stop("Some pipeline paths have identical names. This is not allowed.")

  structure(
    list(
      stages    = stages,
      paths     = paths,
      path_names = path_nms
    ),
    class = "SL_Pipeline"
  )
}


# ── fit.SL_Pipeline ────────────────────────────────────────────────────────

#' @export
fit.SL_Pipeline <- function(object, x, y, ...) {

  stages <- object$stages
  n_stages <- length(stages)

  # fitted_nodes[[s]][[j]] = fitted learner for stage s, learner j
  fitted_nodes <- vector("list", n_stages)

  # stage_outputs[[s]][[j]] = predict(fitted_nodes[[s]][[j]], x_input)
  # where x_input is the output of the predecessor node on this path
  # For stage 1, x_input is always the original x
  # For stage s > 1, x_input is stage_outputs[[s-1]][[ancestor_j]]
  # Since paths pick one learner per stage, we need outputs per learner per stage

  # x_inputs[[s]][[j]] = the x that stage s, learner j was fitted on
  x_inputs <- vector("list", n_stages)

  # ── Stage 1: always fit on original x ──────────────────────────────────
  x_inputs[[1L]] <- rep(list(x), length(stages[[1L]]))
  fitted_nodes[[1L]] <- lapply(seq_along(stages[[1L]]), function(j) {
    lrn <- stages[[1L]][[j]]
    tryCatch(
      fit(lrn, x, y),
      error = function(e) stop(sprintf(
        "Pipeline stage 1, learner '%s': fit failed.\n  %s",
        lrn$name, conditionMessage(e)
      ))
    )
  })

  # Compute stage 1 outputs (predictions used as x for stage 2)
  stage_outputs <- vector("list", n_stages)
  stage_outputs[[1L]] <- lapply(seq_along(stages[[1L]]), function(j) {
    tryCatch(
      predict(fitted_nodes[[1L]][[j]], newdata = x),
      error = function(e) stop(sprintf(
        "Pipeline stage 1, learner '%s': predict failed.\n  %s",
        stages[[1L]][[j]]$name, conditionMessage(e)
      ))
    )
  })

  # ── Stages 2..n: each learner j in stage s gets its x from its
  #    ancestor in stage s-1. The ancestor is determined by the paths.
  #    Since all paths pick exactly one learner per stage, learner j in
  #    stage s is the successor of whichever stage-(s-1) learner feeds it.
  #    We find unique predecessors for each learner in stage s.
  if (n_stages > 1L) {
    for (s in 2L:n_stages) {
      n_prev <- length(stages[[s - 1L]])
      n_curr <- length(stages[[s]])

      # For each learner j in stage s, find all predecessor indices in stage s-1
      # A learner in stage s receives the output of *every* predecessor
      # (by design: every learner in stage s sees all outputs of stage s-1
      #  that lead to it via some path). Since the DAG here is a full
      #  bipartite connection between consecutive stages, every learner in s
      #  is reached from every learner in s-1 via some path.
      # Each learner j in stage s is fitted once on the output of its
      # direct predecessor — but since paths are independent branches,
      # learner j in stage s may appear in multiple paths with different
      # predecessors. In that case it is fitted independently per branch,
      # which means we need one fitted object per (predecessor, j) pair.
      #
      # We store fitted_nodes[[s]] as a list of length n_prev * n_curr,
      # one per (predecessor_idx, successor_idx) pair, and track which
      # path uses which fitted object.

      fitted_nodes[[s]]  <- vector("list", n_prev * n_curr)
      stage_outputs[[s]] <- vector("list", n_prev * n_curr)
      x_inputs[[s]]      <- vector("list", n_prev * n_curr)

      for (prev_j in seq_len(n_prev)) {
        x_in <- stage_outputs[[s - 1L]][[prev_j]]
        for (curr_j in seq_len(n_curr)) {
          slot <- (prev_j - 1L) * n_curr + curr_j
          lrn  <- stages[[s]][[curr_j]]

          x_inputs[[s]][[slot]] <- x_in

          fitted_nodes[[s]][[slot]] <- tryCatch(
            fit(lrn, x_in, y),
            error = function(e) stop(sprintf(
              "Pipeline stage %d, learner '%s' (predecessor slot %d): fit failed.\n  %s",
              s, lrn$name, prev_j, conditionMessage(e)
            ))
          )

          stage_outputs[[s]][[slot]] <- tryCatch(
            predict(fitted_nodes[[s]][[slot]], newdata = x_in),
            error = function(e) stop(sprintf(
              "Pipeline stage %d, learner '%s' (predecessor slot %d): predict failed.\n  %s",
              s, lrn$name, prev_j, conditionMessage(e)
            ))
          )
        }
      }
    }
  }

  structure(
    list(
      stages        = stages,
      paths         = object$paths,
      path_names    = object$path_names,
      fitted_nodes  = fitted_nodes,
      stage_outputs = stage_outputs,
      x_train       = x,
      y_train       = y
    ),
    class = c("SL_Pipeline_Fitted", "SL_Learner_Fitted")
  )
}


# ── predict.SL_Pipeline ────────────────────────────────────────────────────

#' @export
predict.SL_Pipeline_Fitted <- function(object, newdata = NULL, ...) {

  if (is.null(newdata)) newdata <- object$x_train

  stages       <- object$stages
  fitted_nodes <- object$fitted_nodes
  paths        <- object$paths
  n_stages     <- length(stages)

  # For each path, replay the pipeline:
  # - stage 1: predict from fitted_nodes[[1]][[path[[1]]]] on newdata
  # - stage s: predict from the fitted node for this (predecessor, curr) pair
  #   on the output of the previous stage for this path

  results <- lapply(seq_along(paths), function(p) {
    path    <- paths[[p]]
    current <- newdata

    for (s in seq_len(n_stages)) {
      j <- path[[s]]

      # Slot in fitted_nodes[[s]]:
      # stage 1: just j
      # stage s > 1: (prev_j - 1) * n_curr + j
      # where prev_j is path[[s-1]]
      slot <- if (s == 1L) {
        j
      } else {
        n_curr <- length(stages[[s]])
        (path[[s - 1L]] - 1L) * n_curr + j
      }

      fitted_lrn <- fitted_nodes[[s]][[slot]]

      current <- tryCatch(
        predict(fitted_lrn, newdata = current),
        error = function(e) stop(sprintf(
          "Pipeline predict, path '%s', stage %d, learner '%s': failed.\n  %s",
          object$path_names[[p]], s, stages[[s]][[j]]$name,
          conditionMessage(e)
        ))
      )
    }

    current
  })

  names(results) <- object$path_names
  results
}


# ── print methods ──────────────────────────────────────────────────────────

#' @export
print.SL_Pipeline <- function(x, ...) {
  cat(sprintf("SL_Pipeline | %d stage(s) | %d terminal path(s)\n",
              length(x$stages), length(x$paths)))
  for (s in seq_along(x$stages)) {
    lrn_names <- vapply(x$stages[[s]], `[[`, character(1L), "name")
    cat(sprintf("  Stage %d: %s\n", s, paste(lrn_names, collapse = ", ")))
  }
  cat("Paths:\n")
  for (nm in x$path_names) cat(sprintf("  %s\n", nm))
  invisible(x)
}

#' @export
print.SL_Pipeline_Fitted <- function(x, ...) {
  cat(sprintf("SL_Pipeline_Fitted | %d stage(s) | %d terminal path(s)\n",
              length(x$stages), length(x$paths)))
  for (s in seq_along(x$stages)) {
    lrn_names <- vapply(x$stages[[s]], `[[`, character(1L), "name")
    cat(sprintf("  Stage %d: %s\n", s, paste(lrn_names, collapse = ", ")))
  }
  cat("Paths:\n")
  for (nm in x$path_names) cat(sprintf("  %s\n", nm))
  invisible(x)
}


# ── collect_learners ───────────────────────────────────────────────────────

#' Collect learners and pipelines into a setup-ready format
#'
#' @param ... Individual \code{SL_Learner} objects, \code{SL_Pipeline}
#'   objects, or lists of learners (e.g. from \code{lrn_grid}). All are
#'   flattened into a single collection. Pipelines are kept as atomic units
#'   and not flattened.
#' @return A named list of class \code{SL_Learner_Collection} where each
#'   element is either an \code{SL_Learner} or an \code{SL_Pipeline}.
#' @export
collect_learners <- function(...) {
  raw <- list(...)

  result <- list()

  for (item in raw) {
    if (inherits(item, "SL_Pipeline")) {
      # Pipelines are kept as single units — their internal paths are
      # expanded at fit time
      result <- c(result, list(item))
    } else if (inherits(item, "SL_Learner")) {
      result <- c(result, list(item))
    } else if (is.list(item)) {
      flat <- make_learner_list(item)
      result <- c(result, flat)
    } else {
      stop("Each argument to `collect_learners` must be an SL_Learner, SL_Pipeline, or a list of these.")
    }
  }

  # Assign names from learner $name fields and pipeline path names
  nms <- vapply(result, function(x) {
    if (inherits(x, "SL_Pipeline")) paste(x$path_names, collapse = "|")
    else x$name
  }, character(1L))

  names(result) <- nms
  structure(result, class = c("SL_Learner_Collection", "list"))
}



# ── snip_paths ───────────────────────────────────────────────────────-------

#' Snip some paths of a full pipeline
#'
#' @param object Object of class \code{SL_Pipeline} within which to snip.
#' @param ... Character vectors of length two. First entry should be the name of a
#' path, second entry the name of the last learner to be trained on that path.
#' @param keep Should the full (non-snipped) paths be retained as terminal nodes?
#' If you want the full non-snipped path to be trained alongside the snipped path, set to \code{TRUE}.
#' Default is \code{FALSE}, which discards all nodes after the snipped node.
#' @return A list of class \code{SL_Pipeline}.
#' @export
snip_paths <- function(object, ..., keep = FALSE) {
  return(NULL)
}



