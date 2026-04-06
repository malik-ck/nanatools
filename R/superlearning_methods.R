
#' Calculate losses after superlearning.
#'
#' @param obj An object of class lazycv.
#' @param ... Further arguments.
#' @export
loss <- function(obj, ...) {
  UseMethod("loss")
}

# ── Internal helpers ───────────────────────────────────────────────────────

# Build a named preds_list from fitted learners for a given newdata block
make_preds_list <- function(fit_objects_fold, newdata) {
  preds <- lapply(fit_objects_fold, function(lrn)
    predict(lrn, newdata = newdata)
  )
  names(preds) <- vapply(fit_objects_fold, `[[`, character(1L), "name")
  preds
}

# Apply one named metalearner from a fold's ensemble list to a preds_list
apply_metalearner <- function(ensembles_fold, mtl_name, preds_list) {
  mtl_names <- get_learner_names(ensembles_fold)
  mtl_pos   <- which(mtl_names == mtl_name)
  if (length(mtl_pos) == 0L)
    stop(sprintf("Metalearner '%s' not found in this fold's ensembles.\nAvailable: %s",
                 mtl_name, paste(mtl_names, collapse = ", ")))
  predict(ensembles_fold[[mtl_pos]], preds_list)
}

# Resolve which metalearner names to use given user input and object state
resolve_metalearner_names <- function(object, metalearner_name, output_best) {

  all_names     <- get_learner_names(object$metalearners)

  if (!is.null(metalearner_name)) {
    if (output_best)
      warning("`output_best` is ignored when `metalearner_name` is explicitly provided.")
    bad <- setdiff(metalearner_name, all_names)
    if (length(bad))
      stop(sprintf(
        "Metalearner name(s) not found: %s\nAvailable: %s",
        paste(bad, collapse = ", "),
        paste(all_names, collapse = ", ")
      ))
    return(metalearner_name)
  }

  if (output_best) {
    if (is.null(object$best_metalearner))
      stop(paste0("No best metalearner stored. Ensure the object was fitted ",
                  "with outer CV (`was_cv_ensemble = TRUE`)."))
    return(object$best_metalearner)
  }

  # Default: all metalearners and individual learners
  all_names
}



# ── predict.Ensemble ─────────────────────────────────────────────────────────
# ── Internal helper: build preds_list from fit_objects for one fold ────────
make_preds_list <- function(fit_objects_fold, newdata) {
  preds_list <- list()
  for (j in seq_along(fit_objects_fold)) {
    fitted_lrn <- fit_objects_fold[[j]]
    raw <- tryCatch(
      predict(fitted_lrn, newdata = newdata),
      error = function(e) {
        warning(sprintf(
          "Learner '%s' failed during prediction: %s",
          get_lrn_display_name(fitted_lrn), conditionMessage(e)
        ))
        NULL
      }
    )
    if (is.null(raw)) next

    # Splice pipeline/grid outputs as independent entries
    if (is.list(raw) && !is.data.frame(raw) &&
        inherits(fitted_lrn, c("SL_Pipeline_Fitted", "SL_Grid_Fitted"))) {
      # Skip errored paths
      for (nm in names(raw)) {
        if (inherits(raw[[nm]], "SL_Pipeline_Path_Error")) {
          warning(sprintf("Pipeline path '%s' errored during prediction and is skipped.", nm))
          next
        }
        preds_list[[nm]] <- raw[[nm]]
      }
    } else {
      preds_list[[get_lrn_display_name(fitted_lrn)]] <- raw
    }
  }
  if (length(preds_list) == 0L)
    stop("All learners failed during prediction.")
  preds_list
}


#' @export
predict.Ensemble <- function(object,
                             newdata          = NULL,
                             metalearner_name = NULL,
                             output_best      = FALSE,
                             ensemble_fold_id = NULL,
                             type             = NULL,
                             ...) {

  # ── Validate type ──────────────────────────────────────────────────────────
  if (is.null(type))
    stop("Please provide `type`: 'cv' for cross-validated predictions or ",
         "'ensemble' for ensemble predictions.")
  type <- match.arg(type, c("cv", "ensemble"))

  # ── Resolve metalearner names ──────────────────────────────────────────────
  use_names <- resolve_metalearner_names(object, metalearner_name, output_best)

  # ══════════════════════════════════════════════════════════════════════════
  # type = "cv"
  # ══════════════════════════════════════════════════════════════════════════
  if (type == "cv") {

    if (!object$was_cv_ensemble)
      stop("Cross-validated predictions require outer CV. ",
           "Refit with a non-NULL `outer_cv`.")

    if (is.null(newdata)) {
      if (is.null(object$x))
        stop("No data stored. Refit with `return_data = TRUE` or supply `newdata`.")
      newdata <- object$x
    } else {
      if (!is.null(dim(newdata)) && !is.null(dim(object$x)))
        if (ncol(newdata) != ncol(object$x))
          stop("ncol(newdata) must match ncol of the training data.")
    }

    perf_folds <- object$cv$performance_sets
    n_folds    <- length(perf_folds)

    result <- lapply(use_names, function(nm) {

      chunks <- lapply(seq_len(n_folds), function(i) {
        val        <- validation_set(perf_folds[[i]])
        preds_list <- make_preds_list(object$fit_objects[[i]],
                                      newdata[val, , drop = FALSE])
        preds      <- apply_metalearner(object$ensembles[[i]], nm, preds_list)
        list(idx = val, preds = preds)
      })

      all_idx  <- unlist(lapply(chunks, `[[`, "idx"))
      combined <- combine_preds(lapply(chunks, `[[`, "preds"))
      sort_ord <- order(all_idx)

      sorted <- if (is.data.frame(combined) || is.matrix(combined))
        combined[sort_ord, , drop = FALSE]
      else
        combined[sort_ord]

      structure(sorted, indices = all_idx[sort_ord])
    })
    names(result) <- use_names

    if (length(result) == 1L) return(result[[1L]])
    return(result)
  }

  # ══════════════════════════════════════════════════════════════════════════
  # type = "ensemble"
  # ══════════════════════════════════════════════════════════════════════════
  if (type == "ensemble") {

    if (!is.null(ensemble_fold_id) && !object$was_cv_ensemble)
      warning("`ensemble_fold_id` is ignored for non-cross-validated objects.")

    if (object$was_cv_ensemble && is.null(ensemble_fold_id))
      stop("For a cross-validated object, provide `ensemble_fold_id` to select ",
           "which fold's ensemble to use.\n",
           "Consider refitting without outer CV using only the best metalearner.")

    if (!object$was_cv_ensemble)
      ensemble_fold_id <- 1L

    if (is.null(newdata)) {
      if (is.null(object$x))
        stop("No data stored. Refit with `return_data = TRUE` or supply `newdata`.")
      newdata <- object$x
    }

    preds_list <- make_preds_list(
      object$fit_objects[[ensemble_fold_id]], newdata
    )

    result <- lapply(use_names, function(nm)
      apply_metalearner(object$ensembles[[ensemble_fold_id]], nm, preds_list)
    )
    names(result) <- use_names

    if (length(result) == 1L) return(result[[1L]])
    return(result)
  }
}


# ── loss.Ensemble ────────────────────────────────────────────────────────────

#' Compute loss for a fitted SuperLearner
#'
#' @param object A fitted \code{Ensemble} object.
#' @param newdata Optional new predictor data. Defaults to stored training
#'   data. For \code{type = "cv"}, applied fold-wise to validation sets.
#' @param loss_fun_list Optional \code{mtl_loss} object. Defaults to the loss
#'   used during fitting.
#' @param ensemble_fold_id Integer. For \code{type = "ensemble"} with outer
#'   CV, selects which fold's ensemble to evaluate.
#' @param type Character. One of \code{"cv"} or \code{"ensemble"}.
#' @param ... Ignored.
#' @return A named numeric vector of mean losses, one per metalearner.
#' @export
loss.Ensemble <- function(object,
                        newdata          = NULL,
                        loss_fun_list    = NULL,
                        ensemble_fold_id = NULL,
                        type             = NULL,
                        ...) {

  # ── Validate type ──────────────────────────────────────────────────────────
  if (is.null(type))
    stop("Please provide `type`: 'cv' or 'ensemble'.")
  type <- match.arg(type, c("cv", "ensemble"))

  # ── Resolve loss function ──────────────────────────────────────────────────
  if (is.null(loss_fun_list)) {
    loss_fun_list <- object$loss_fun_list
  } else {
    if (!inherits(loss_fun_list, "mtl_loss"))
      stop("`loss_fun_list` must be of class 'mtl_loss'.\n",
           "You can specify a custom loss function via 'loss_custom'. Example: loss_custom(function(y, y_hat) abs(y - y_hat)).")
  }

  # ── Resolve y ─────────────────────────────────────────────────────────────
  if (is.null(object$y))
    stop(paste0("No outcome stored in object. ",
                "Refit with `return_data = TRUE` to enable loss computation."))
  y <- object$y

  # ── Get predictions for all metalearners ──────────────────────────────────
  # Always request all metalearners so we return a complete loss table
  all_names <- c(
    get_learner_names(object$metalearners),
    get_learner_names(object$learners)
  )

  get_preds <- suppressWarnings(predict(
    object           = object,
    newdata          = newdata,
    metalearner_name = all_names,
    output_best      = FALSE,
    ensemble_fold_id = ensemble_fold_id,
    type             = type
  ))

  # predict() returns a named list when multiple metalearners are requested
  # Wrap in list if somehow a single object slipped through
  if (!is.list(get_preds) || !is.null(attr(get_preds, "indices")))
    get_preds <- setNames(list(get_preds), all_names[[1L]])

  # ── Compute mean loss per metalearner ──────────────────────────────────────
  # For cv type, predictions have an indices attribute — subset y accordingly
  vapply(names(get_preds), function(nm) {
    preds <- get_preds[[nm]]
    idx   <- attr(preds, "indices")
    y_use <- if (!is.null(idx)) subset_y(y, idx) else y
    mean(loss_fun_list$loss_fun(y_use, preds))
  }, numeric(1L))
}


# ── print.Ensemble ───────────────────────────────────────────────────────────

#' @export
print.Ensemble <- function(object, ...) {
  cv_word <- if (object$was_cv_ensemble)
    sprintf("Cross-validated across %d outer fold(s).",
            length(object$cv$performance_sets))
  else
    "Not cross-validated."

  n_build <- if (!is.null(object$cv$build_sets))
    length(object$cv$build_sets[[1L]])
  else
    0L

  cat("── Ensemble ", paste(rep("\u2500", 38), collapse = ""), "\n", sep = "")
  cat(sprintf("  Learners     : %d\n", length(object$learners)))
  cat(sprintf("  Metalearners : %d\n", length(object$metalearners)))
  cat(sprintf("  Inner folds  : %d\n", n_build))
  cat(sprintf("  %s\n", cv_word))

  if (!is.null(object$best_metalearner))
    cat(sprintf("  Best         : %s\n", object$best_metalearner))

  cat(paste(rep("\u2500", 50), collapse = ""), "\n")
  invisible(object)
}
