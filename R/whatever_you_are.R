# Functions that aren't needed in the main loop anymore. Off-loading them here. Doesn't hurt to export them.

#' Fit a learner on every fold's training set
#'
#' @param fold_list A \code{nana_fold_list}.
#' @param learner A \code{SL_Learner} object.
#' @param x Full predictor matrix or data frame.
#' @param y Full outcome vector.
#' @param future_pkgs Character vector of packages required by workers.
#' @return A list of \code{SL_Learner_Fitted} objects, one per fold.
#' @export
fit_folds <- function(fold_list, learner, x, y,
                      future_pkgs = character(0L)) {
  if (!inherits(fold_list, "nana_fold_list"))
    stop("`fold_list` must be a nana_fold_list.")
  future.apply::future_lapply(fold_list, function(fold) {
    tr <- training_set(fold)
    fit(learner, x[tr, , drop = FALSE], y[tr])
  }, future.packages = future_pkgs, future.seed = TRUE)
}

#' Generate cross-validated predictions from a learner across folds
#'
#' Fits each learner on the training set of each fold and predicts on the
#' validation set, preserving the raw learner output type. Returns a data
#' frame with columns \code{index} (original observation position) and
#' \code{prediction} (raw output), sorted by index. Duplicate indices
#' (bootstrap, repeated CV) are preserved — the caller decides whether to
#' average or keep all.
#'
#' @param fold_list A \code{nana_fold_list}.
#' @param fitted_list A list of fitted learner objects, one per fold.
#' @param x Full predictor matrix or data frame.
#' @return A data frame with columns \code{index} and \code{prediction},
#'   sorted by \code{index}.
#' @export
predict_folds <- function(fold_list, fitted_list, x) {
  if (!inherits(fold_list, "nana_fold_list"))
    stop("`fold_list` must be a nana_fold_list.")
  if (length(fold_list) != length(fitted_list))
    stop("`fold_list` and `fitted_list` must have the same length.")

  chunks <- lapply(seq_along(fold_list), function(i) {
    val   <- validation_set(fold_list[[i]])
    preds <- predict(fitted_list[[i]],
                     newdata = x[val, , drop = FALSE])
    list(index = val, prediction = preds)
  })

  all_idx   <- unlist(lapply(chunks, `[[`, "index"))
  all_preds <- combine_preds(lapply(chunks, `[[`, "prediction"))
  sort_ord  <- order(all_idx)

  sorted_idx   <- all_idx[sort_ord]
  sorted_preds <- if (is.data.frame(all_preds) || is.matrix(all_preds))
    all_preds[sort_ord, , drop = FALSE]
  else
    all_preds[sort_ord]

  n      <- fold_list[[1L]]$n
  counts <- tabulate(sorted_idx, nbins = n)

  if (any(counts == 0L) && !all(counts == 0L))
    warning(sprintf(
      "%d observation(s) have no validation prediction. Expected for ",
      "subsampling; may indicate a problem for standard CV.",
      sum(counts == 0L)
    ))
  if (any(counts > 1L))
    warning(sprintf(
      "%d observation(s) appear in more than one validation set. ",
      "Predictions are sorted by index — use attr(result, 'indices') ",
      "with tapply() or similar to average if needed.",
      sum(counts > 1L)
    ))

  structure(sorted_preds, indices = sorted_idx)
}
