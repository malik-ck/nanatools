
# Wrapper for lazy_cv with multiply imputed data


#' A wrapper for lazy_cv with multiply imputed data via MICE
#'
#' @param mi_data A mids-object generated via mice() containing multiply imputed data.
#' @param y_name The name of the outcome variable which is extracted from data sets in mi_data.
#' @param var_subset An optional vector of variable names which are handed into lazy_cv. Useful if not all variables used for imputations are to be used in cross-validation.
#' @param init The cross-validation initialization object generated via setup_learner().
#' @param use_future_plan If you use a future plan, set to TRUE to use it.
#'
#' @returns A list of class LazyCV_SL, which contains lists of class LazyCV (one per imputed data set)
#' @import mice
#' @import future
#' @export
#'
#' @examples
#' data(iris)
lazy_cv_mi <- function(mi_data, y_name, var_subset = NULL, init, use_future_plan = FALSE) {

  # Ensure one used an accepted MI-method
  if (class(mi_data) != "mids") stop("Please provide a mids-object generated via mice().")

  # Ensure that one does not have y_name in the var_subset
  if (!is.null(var_subset)) {

    if (y_name %in% var_subset) stop ("y cannot be in the variable subset.")

  }

  # Check that future input okay
  if (use_future_plan != TRUE & use_future_plan != FALSE) stop("Need TRUE or FALSE for use_future_plan.")

  # Write wrapper that separates a data set received via complete() into x and y,
  # removing variables not in var_subset
  do_cv_mi <- function(mi_df, y_name, var_subset, init) {

    # First extract the treatment variable
    get_y <- mi_df[,y_name]

    # Then limit the subset of variables to those in var_subset, also excluding y
    # Can then already run the function!

    if (!is.null(var_subset)) {

      return(lazy_cv(x = mi_df[,colnames(mi_df) %in% var_subset, drop = FALSE], y = get_y, init))

    } else {

      return(lazy_cv(x = mi_df[,-which(colnames(mi_df) == y_name), drop = FALSE], y = get_y, init))

    }

  }

  # Now simply loop over the imputed data
  # To be memory-efficient, do it one-by-one
  # Can optionally be set up using future

  if (use_future_plan == FALSE) {

    results_list <- vector("list", length = mi_data$m)

    for (i in 1:mi_data$m) {

      results_list[[i]] <- do_cv_mi(complete(mi_data, action = i), y_name, var_subset, init)

    }

    class(results_list) <- "LazySL_CV"

    return(results_list)


  } else if (use_future_plan == TRUE) {

    futures_list <- vector("list", length = mi_data$m)

    for (i in 1:mi_data$m) {

      futures_list[[i]] <- future::future({
        do_cv_mi(mice::complete(mi_data, action = i), y_name, var_subset, init)
      }, seed = TRUE, packages = init$future_pkgs)

    }

    results_list <- lapply(futures_list, future::value)

    class(results_list) <- "LazySL_CV"

    return(results_list)

  }

}

#' A wrapper for fractional weighted bootstrap TMLE with multiply imputed data via MICE
#'
#' @param treatment_model An object of class LazySL_CV or weightthem. For LazySL_CV objects, applying "predict" to individual ensembles should provide estimated propensity scores.
#' @param or_model An object of class LazySL. Applying "predict" should provide estimates depending on the value provided to treatment_name.
#' @param treatment_name he name of the treatment variable. Has to have values in {1, 0}.
#' @param metalearner_treatment If the treatment model is of class LazySL, which metalearner is to be used. If NULL and the ensemble is cross-validated, defaults to the best metalearner.
#' @param metalearner_outcome Which metalearner is to be used. If NULL and the ensemble is cross-validated, defaults to the best metalearner.
#' @param trim_ipw Specifies how weights are to be trimmed. NULL does no trimming. "gruber" applies the data-adaptive method provided by Gruber et al. (2022).
#' @param n_bstrap The number of bootstrap samples used to estimate the variance of the influence curve.
#' @param fluctuation_family Family object used for targeting. Should match the way the outcome was fit originally.
#' @param y_bounds Two-length vector. If the outcome was rescaled to be in [0, 1], the upper and lower bounds used. Will rescale estimands accordingly.
#'
#' @returns A list containing a data-frame summarizing results after pooling for multiple imputation, additional data frames for each imputed data set, as well as some additional objects used for fitting.
#'
#' @import mice
#' @export
#'
#' @examples
#' data(iris)
fwb_tmle_bin_mi <- function(treatment_models, or_models, treatment_name, metalearner_treatment = NULL,
                            metalearner_outcome = NULL, trim_ipw = NULL, n_bstrap = 5000,
                            fluctuation_family = gaussian(), y_bounds = NULL) {

  # Some modest typechecking. First, ensure number of imputations are identical across treatment and outcome model
  # Of course, imputations also need to be identical, but checking this is too data-intensive (have some trust here)
  if ("wimids" %in% class(treatment_models)) tx_imps <- length(treatment_models$models) else tx_imps <- length(treatment_models)
  if (length(or_models) != tx_imps) stop("Different number of imputations between treatment and outcome models.\nPlease make sure you used the same imputed data.")

  # Now a matter of looping to calculate results
  all_means <- matrix(rep(NA, 4 * tx_imps), ncol = 4)
  all_vars <- matrix(rep(NA, 4 * tx_imps), ncol = 4)
  temp <- vector("list", length = tx_imps)

  if ("wimids" %in% class(treatment_models)) {

    for (i in 1:tx_imps) {

      temp[[i]] <- fwb_tmle_bin(treatment_models$models[[i]], or_models[[i]], treatment_name,
                                metalearner_treatment, metalearner_outcome,
                                trim_ipw, n_bstrap, fluctuation_family, y_bounds)

      all_means[i,] <- temp[[i]]$results[,2]
      all_vars[i,] <- temp[[i]]$results[,3]

    }

  } else {

    for (i in 1:tx_imps) {

      temp[[i]] <- fwb_tmle_bin(treatment_models[[i]], or_models[[i]], treatment_name,
                                metalearner_treatment, metalearner_outcome,
                                trim_ipw, n_bstrap, fluctuation_family, y_bounds)

      all_means[i,] <- temp[[i]]$results[,2]
      all_vars[i,] <- temp[[i]]$results[,3]

    }

  }

  # Lastly, pooling!

  # Pooled effect estimates
  pooled_effects <- apply(all_means, 2, mean)
  within_var <- apply(all_vars, 2, mean)
  between_var <- apply(t(t(all_means) - pooled_effects), 2, var)
  total_var <- within_var + between_var + (between_var / tx_imps)

  # Again get it into a nice-looking data frame
  results_df <- data.frame(
    Estimand = c("EY1", "EY0", "ATE", "RR"),
    Pooled_Estimate = pooled_effects,
    Pooled_Variance = total_var,
    Pooled_Lower_CI = pooled_effects - (1.96 * sqrt(total_var)),
    Pooled_Upper_CI = pooled_effects + (1.96 * sqrt(total_var))
  )

  # Now return
  results_list <- list(
    results = results_df,
    objects = temp
  )

  class(results_list) <- "FWB_TMLE_MI"


  return(results_list)

}
