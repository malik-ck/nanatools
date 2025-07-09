
#' Calculate losses after superlearning.
#'
#' @param obj An object of class lazycv.
#' @param ... Further arguments.
#' @export
loss <- function(obj, ...) {
  UseMethod("loss")
}

#' Caulculate marginal effects after superlearning.
#'
#' @param obj An object of class lazycv.
#' @param ... Further arguments.
#' @export
marginals <- function(obj, ...) {
  UseMethod("marginals")
}

#' @export
predict.LazySL <- function(object, newdata = NULL, metalearner_name = NULL, output_best = FALSE, ensemble_fold_id = NULL, type = NULL) {

  # Small starting type checks
  if (is.null(type)) {

    stop("Please provide a valid option for the type of prediction: cv for cross-validated predictions on (usually) the original observations,
         ensemble for predictions using one ensemble.")

  }

  if (!type %in% c("cv", "ensemble")) {

    stop("Please provide a valid option for the type of prediction: cv for cross-validated predictions on (usually) the original observations,
         ensemble for predictions using one ensemble.")

  }

  if (type == "cv") {

    # CV predictions only make sense for validated ensembles
    if (object$was_cv_ensemble == FALSE) {

      stop("Cross-validated predictions can only be used for cross-validated ensembles
         (i.e., with outer_cv not NULL).")

    }

    if (is.null(newdata)) newdata <- object$x

    # If someone provided new data, ensure that its number of columns matches
    # that of the original data set
    if (ncol(object$x) != ncol(newdata)) {

      stop("Please ensure that newdata has the same number of columns as training data.")

    }

    if (!is.null(metalearner_name)) {
      warning("Metalearner name provided. Option for output_best is ignored.")
    }

    if (is.null(metalearner_name) & output_best == TRUE) {

      warning("No metalearner specified. Output is provided for the best metalearner.
            If you instead want predictions from all metalearners, set output_best to FALSE.")

      metalearner_name <- object$best_metalearner

    } else if (is.null(metalearner_name) & output_best == FALSE) {

      warning("No metalearner specified. Output is provided for all metalearners.
            If you instead want predictions from the best metalearner only and have a cross-validated ensemble, set output_best to TRUE.")

      metalearner_name <- names(object$metalearners)

    }

    # Write a function that predicts for one set of folds
    predict_one_fold <- function(ensemble, weights, validation_set, newdata = NULL) {

      preds_vec <- rep(0, length(validation_set))

      for (i in 1:length(ensemble)) {

        current_preds <- predict(ensemble[[i]], newdata = newdata)
        current_weight <- weights[ensemble[[i]]$name]

        preds_vec <- preds_vec + current_preds*current_weight

      }

      return_matrix <- cbind(index = validation_set, preds = preds_vec)
      colnames(return_matrix) <- c("index", "preds")

      return(return_matrix)

    }

    # Run this function across all folds
    pred_list <- vector("list", length(object$cv$performance_sets))

    for (i in 1:length(pred_list)) {

      current_validation_set <- object$cv$performance_sets[[i]]$validation_set

      # Get relevant subset of ensemble weights
      weight_subset <- object$ensembles[[i]][names(object$ensembles[[i]]) %in% metalearner_name]


      pred_list[[i]] <- lapply(weight_subset, predict_one_fold,
                               ensemble = object$fit_objects[[i]], validation_set = current_validation_set,
                               newdata = newdata[current_validation_set,])

    }

    wrangled_preds <- lapply(metalearner_name,
                             function(x) lapply(pred_list, function(y) y[[x]])
    )

    combined_preds <- lapply(wrangled_preds, function(x) do.call("rbind", x))
    names(combined_preds) <- metalearner_name

    sorted_preds <- lapply(combined_preds, function(x) x[order(x[,"index"]),])

    final_preds <- lapply(sorted_preds, function(x) x[,"preds"])

    if (length(metalearner_name) == 1) final_preds <- unname(unlist(final_preds))

    return(final_preds)

  }

  if (type == "ensemble") {

    if (!is.null(ensemble_fold_id)) {

      warning("The fold ID is not necessary for predictions that are not cross-validated. Input is ignored.")

    }

    # Set newdata to be saved x if newdata is NULL
    if (is.null(newdata)) newdata <- object$x

    # Do some checks
    if (object$was_cv_ensemble == FALSE & output_best == TRUE) {
      stop("Please cross-validate your ensemble to be able to set output_best to TRUE.")
    }

    if (is.null(ensemble_fold_id) & object$was_cv_ensemble == TRUE) {

      stop("For a cross-validated ensemble, you need to provide the index for the ensemble
         you want to use for prediction. You could instead consider re-training the
         ensemble without cross-validation via outer_cv = NULL using the best metalearner.")

    }

    if (object$was_cv_ensemble == FALSE) ensemble_fold_id <- 1

    if (object$had_metalearner == TRUE) {

      if(!is.null(metalearner_name)) {

        if(!metalearner_name %in% names(object$metalearners)) stop("Please provide a metalearner name matching an actual metalearner.")

      }

      if (is.null(metalearner_name) & object$metalearner_count > 1) {

        # Some warning logic and metalearner name assignment
        if (output_best == FALSE) {

          warning("No metalearner specified. Output is provided for all of them.
              If you instead want predictions from the best metalearner only and have a cross-validated ensemble, set output_best to TRUE.")

          metalearner_name <- names(object$metalearners)

        } else if (output_best == TRUE) {

          warning("No metalearner specified. Output is provided for the best metalearner.
              If you instead want predictions from all metalearners, set output_best to FALSE.")

          metalearner_name <- object$best_metalearner

        }

      }

    }

    if (object$had_metalearner == TRUE && object$metalearner_count == 1) metalearner_name <- names(object$metalearners)[[1]]
    if (object$had_metalearner == FALSE) metalearner_name <- names(object$ensembles[[1]])
    if (!is.null(metalearner_name) & (object$had_metalearner  == FALSE)) warning("Metalearner name is ignored as there was no metalearner.")

    # Now do some predicting...

    pred_engine <- function(metalearner_name, newdata, list_obj, ensemble_fold_id) {

      preds_vec <- rep(0, nrow(newdata))

      for (i in 1:length(list_obj$learners)) {

        current_preds <- predict(list_obj$fit_objects[[ensemble_fold_id]][[i]], newdata)
        current_weight <- list_obj$ensembles[[ensemble_fold_id]][[metalearner_name]][list_obj$learners[[i]]$name]

        preds_vec <- preds_vec + current_preds*current_weight

        names(preds_vec) <- NULL

      }

      return(preds_vec)

    }

    return_list <- lapply(metalearner_name, function(x) pred_engine(metalearner_name = x, newdata = newdata, list_obj = object, ensemble_fold_id = ensemble_fold_id))

    if(length(return_list) == 1) return_list <- unlist(return_list) else names(return_list) <- metalearner_name

    # Now do some adjustments in case we set output_best to TRUE:
    if (is.list(return_list) & output_best == TRUE) {



    }

    return(return_list)

  }


}



### Now losses

#' @export
loss.LazySL <- function(object, newdata = NULL, ensemble_fold_id = NULL, type = NULL) {

  # Small starting type checks
  if (is.null(type)) {

    stop("Please provide a valid option for the type of prediction: cv for cross-validated predictions on (usually) the original observations,
         ensemble for predictions using one ensemble.")

  }

  if (!type %in% c("cv", "ensemble")) {

    stop("Please provide a valid option for the type of prediction: cv for cross-validated predictions on (usually) the original observations,
         ensemble for predictions using one ensemble.")

  }


  if (type == "cv") {

    get_preds <- suppressWarnings(predict(object = object, newdata = newdata, type = "cv",
                                          metalearner_name = NULL, output_best = FALSE))

    loss_out <- lapply(get_preds, object$loss_fun_list$loss_fun, y = object$y)


    return(unlist(loss_out))

  }

  if (type == "ensemble") {

    if (!is.null(ensemble_fold_id)) {

      warning("The fold ID is not necessary for losses that are not cross-validated. Input is ignored.")

    }

    get_preds <- suppressWarnings(predict(object = object, newdata = newdata, metalearner_name = NULL, type = "ensemble",
                                          ensemble_fold_id = ensemble_fold_id, output_best = FALSE))

    loss_out <- lapply(get_preds, object$loss_fun_list$loss_fun, y = object$y)


    return(unlist(loss_out))

  }


}

#' @export
marginals.LazySL <- function(obj, subset = NULL, metalearner_name, type = "ensemble", h = NULL) {

  data <- obj$x

  if (is.null(subset)) subset <- colnames(data) else subset <- subset

  estimate_list <- rep(NA, length(subset))

  for (i in 1:length(subset)) {

    if (length(unique(data[,subset[[i]]])) == 2) step_size <- ifelse(data[,subset[[i]]] == 1, -1, 1) else {

      if (is.null(h)) step_size <- abs(diff(range(data[,subset[[i]]])))/10000 else step_size = h

    }

    # Get original data back, change data copy variable by step size
    data_copy <- data
    data_copy[,subset[[i]]] <- data_copy[,subset[[i]]] + step_size

    first_preds <- predict(obj, metalearner_name = metalearner_name, type = type)

    diff_preds <- predict(obj, newdata = data_copy, metalearner_name = metalearner_name, type = type)

    estimate_list[[i]] <- mean((diff_preds - first_preds) / step_size)

  }

  return(data.frame(term = subset, estimate = unlist(estimate_list)))


}


#' @export
print.LazySL <- function(object) {

  if (object$was_cv_ensemble == TRUE) {
    has_cv_word <- paste0("Has been cross-validated across ", length(object$cv$performance_sets), " folds.")
  } else has_cv_word <- "Has not been cross-validated."

  cat("A superlearning object with:\n\n")

  cat(length(object$learners), "learner(s)\n")

  cat(length(object$metalearners), "ensemble(s)\n\n")

  cat(has_cv_word)

}
