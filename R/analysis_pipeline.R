
### Given treatment models (GLM, WeightIt or LazyCV) and outcome models (GLM or LazyCV),
### perform a fractional weighted bootstrap TMLE with weighted updates.
### This function is specific to binary treatments; a generalized version will also be available.


#' Fractional Weighted Bootstrap-based TMLE, for use in nanatools
#'
#' @param treatment_model An object of class weightit, LazySL or glm such that applying "predict" to it provides propensity scores for a binary treatment.
#' @param or_model For unstratified fits: An object of class LazySL or glm. Applying "predict" should provide estimates depending on the value provided to treatment_name.
#' @param treatment_name For unstratified fits: The name of the treatment variable. Has to have values in {1, 0}.
#' @param metalearner_treatment If the treatment model is of class LazySL, which metalearner is to be used. If NULL and the ensemble is cross-validated, defaults to the best metalearner.
#' @param metalearner_outcome If the outcome model is of class LazySL, which metalearner is to be used.  If NULL and the ensemble is cross-validated, defaults to the best metalearner.
#' @param trim_ipw Specifies how weights are to be trimmed. NULL does no trimming. "gruber" applies the data-adaptive method provided by Gruber et al. (2022).
#' @param n_bstrap The number of bootstrap samples used to estimate the variance of the influence curve.
#' @param fluctuation_family Family object used for targeting. Should match the way the outcome was fit originally.
#' @param y_bounds Two-length vector. If the outcome was rescaled to be in [0, 1], the upper and lower bounds used. Will rescale estimands accordingly.
#'
#' @returns A list containing a data-frame summarizing results and some additional objects used for fitting.
#' @import tmle
#' @export
#'
#' @details Gruber et al. sets the lower bound of propensity scores to 5 / (sqrt(n) log(n/5)) and sets propensity scores below this threshold to this threshold.
#' More details in: https://doi.org/10.1093/aje/kwac087.
#' If weightit is used for the treatment model, its provided propensity score is used, not the weight. The provided estimand is ignored.
#' If LazySL is used and cv_fit_treatment is TRUE, the best ensemble is used, unless another is specified
#'
#' @examples
#' data(iris)
fwb_tmle_bin <- function(treatment_model, or_model, treatment_name, metalearner_treatment = NULL,
                         metalearner_outcome = NULL, trim_ipw = NULL, n_bstrap = 5000,
                         fluctuation_family = gaussian(), y_bounds = NULL) {

  if (!is.null(y_bounds)) warning("Rescaling y not yet implemented. Will report on provided scale.")

  # Do typechecks for metalearning
  typecheck_lazy_sl_tmle(treatment_model, metalearner_treatment)
  typecheck_lazy_sl_tmle(or_model, metalearner_outcome)
  if (sum(class(or_model) %in% "weightit") > 0) stop("An outcome model cannot be a weightit object.")

  # Further typechecks
  typechecks_fwb_tmle_bin(treatment_model, or_model, treatment_name,
                          metalearner_treatment, metalearner_outcome, trim_ipw, n_bstrap)

  if (class(treatment_model) %in% "weightit") warning("The estimand handed in for WeightIt model creation is ignored.")

  # Get treatment and outcome variables, depending on outcome regression model type
  if ("LazySL" %in% class(or_model)) {

    a <- or_model$x[,treatment_name]
    y <- or_model$y

  } else if ("glm" %in% class(or_model)) {

    a <- model.frame(or_model)[,treatment_name]
    y <- or_model$y

  }

  # Extract propensity scores (via ps-object from weightit, otherwise via predict)
  if ("weightit" %in% class(treatment_model)) {

    get_ps <- treatment_model$ps

  } else if ("glm" %in% class(treatment_model)) {

    get_ps <- predict(treatment_model, type = "response")
    get_ps <- ifelse(a == 1, get_ps, 1 - get_ps)

  } else if ("LazySL" %in% class(treatment_model)) {

    if (is.null(metalearner_treatment)) {

      get_ps <- predict(treatment_model, output_best = TRUE, type = "cv")
      get_ps <- ifelse(a == 1, get_ps, 1 - get_ps)

    } else if (treatment_model$was_cv_ensemble == TRUE) {

      get_ps <- predict(treatment_model, metalearner_name = metalearner_treatment, type = "cv")
      get_ps <- ifelse(a == 1, get_ps, 1 - get_ps)

    } else if (treatment_model$was_cv_ensemble == FALSE) {

      get_ps <- predict(treatment_model, metalearner_name = metalearner_treatment, type = "ensemble")
      get_ps <- ifelse(a == 1, get_ps, 1 - get_ps)

    }

  }


  # Write some logic to get the outcome predictions we want for Lazy CV
  if ("LazySL" %in% class(or_model)) {

    ensemble_type <- ifelse(or_model$was_cv_ensemble == TRUE, "cv", "ensemble")
    output_best <- ifelse(is.null(metalearner_outcome) & ensemble_type == "cv", TRUE, FALSE)

    ya1 <- predict(or_model, metalearner_name = metalearner_outcome, output_best = output_best, type = ensemble_type, newdata = quick_transform(or_model$x, treatment_name, 1))
    ya0 <- predict(or_model, metalearner_name = metalearner_outcome, output_best = output_best, type = ensemble_type, newdata = quick_transform(or_model$x, treatment_name, 0))

  } else {

    ya1 <- predict(or_model, newdata = quick_transform(model.frame(or_model), treatment_name, 1))
    ya0 <- predict(or_model, newdata = quick_transform(model.frame(or_model), treatment_name, 0))

  }

  # Now have all info I need to do tmle
  # First, transform to weights from PS and truncate if requested
  if (!is.null(trim_ipw)) {

    if (trim_ipw == "gruber") {

      ps_floor <- 5 / (sqrt(length(get_ps)) * log(length(get_ps)/5))
      get_ps <- pmax(get_ps, ps_floor)

    }

    # Space for more here!

  } else ps_floor <- 0

  # Do the function call
  # Separate options for extracting y and x, depending on whether LazySL or GLM was used
  results_list <- matrix(rep(NA, length.out = n_bstrap * 4), ncol = 4)
  colnames(results_list) <- c("EY1", "EY0", "ATE", "RR")

  # Now bootstrap!

  for (i in 1:nrow(results_list)) {

    bstrap_weight <- r_unif_dirichlet(1, length(y))

    # Clever covariates:
    get_ccov_ref <- ifelse(a == 0, 1 / get_ps, 0)
    get_ccov_tx <- ifelse(a == 1, 1 / get_ps, 0)

    # Targeting:
    targeted_ref <- predict(stats::glm(y ~ 1, offset = ya0, weights = get_ccov_ref * bstrap_weight, family = fluctuation_family), type = "response")
    targeted_tx <- predict(stats::glm(y ~ 1, offset = ya1, weights = get_ccov_tx * bstrap_weight, family = fluctuation_family), type = "response")

    # Map into estimands
    est_ey1 <- weighted.mean(targeted_tx, bstrap_weight)
    est_ey0 <- weighted.mean(targeted_ref, bstrap_weight)
    est_ate <- est_ey1 - est_ey0
    est_rr <- est_ey1 / est_ey0

    results_list[i,] <- c(est_ey1, est_ey0, est_ate, est_rr)

  }

  # Summarize in data.frame
  estims <- colnames(results_list)
  means <- colMeans(results_list)
  vars <- apply(results_list, 2, var)
  lower_ci_nonp <- apply(results_list, 2, function(x) quantile(x, 0.025))
  upper_ci_nonp <- apply(results_list, 2, function(x) quantile(x, 0.975))
  upper_ci_param <- apply(results_list, 2, function(x) mean(x) + 1.96 * sd(x))
  lower_ci_param <- apply(results_list, 2, function(x) mean(x) - 1.96 * sd(x))

  get_results_df <- data.frame(
    Estimand = estims,
    Estimate = means,
    Estimator_Variance = vars,
    NP_Lower_CI = lower_ci_nonp,
    NP_Upper_CI = upper_ci_nonp,
    P_Lower_CI = lower_ci_param,
    P_Upper_CI = upper_ci_param
  )

  results_list <- list(
    results = get_results_df,
    initial_estimates = cbind(Treated = ya1, Untreated = ya0),
    ps = get_ps,
    clever_covariate_treatment = get_ccov_tx,
    clever_covariate_control = get_ccov_ref,
    bootstrap_samples = n_bstrap,
    fluctuation = fluctuation_family,
    ps_bound = ps_floor
  )

  class(results_list) <- "FWB_TMLE"

  # Finally, return!
  return(results_list)

}


### Plot results from the previous TMLE procedure
