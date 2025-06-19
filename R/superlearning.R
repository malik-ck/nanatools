
### Viable upper list structure
# Data bank
# Fold specification
# List of learners
# Metalearner
# Counterfactual treatment values
# An adaptable config-list that is automatically created with reasonable defaults (say, for CV)

### Viable learner list structure
# Name
# Function taking in data, returning fit object
# Function for making predictions
# Optional: Function with other stuff to return
# Optional and a to-do: grids for hyperparameter tuning

# Depend on origami for fold specification

make_cv_funs <- function(inner_cv, outer_cv) {

  # Check type for CV
  cv_checker <- function(object) {

    if (!is.null(object) & !is.function(object) &
        ((!is.numeric(object)) | (is.numeric(object) && (length(object) != 1 || object < 2))) &
        (!is.list(object) | (is.list(object) & !all(unlist(lapply(object, function(x) "training_set" %in% names(x) && "validation_set" %in% names(x))))))) {

      stop("Please input a positive integer >=2, a function, or correctly formatted lists of training and validation sets for CV.")
    }

  }

  cv_checker(inner_cv)
  cv_checker(outer_cv)


  # Ensure some CV is performed
  if (is.null(inner_cv) & is.null(outer_cv)) {

    stop("Please enable cross-validation for either ensemble building or out-of-sample prediction.")

  }

  # Now input the correct functions
  if (is.numeric(outer_cv)) {

    outer_cv_fun <- function(data) make_folds(n = data, V = outer_cv, fold_fun = folds_vfold)

    body(outer_cv_fun)[[3]] <- outer_cv

  } else outer_cv_fun <- outer_cv

  if (is.numeric(inner_cv)) {

    inner_cv_fun <- function(data) make_folds(n = data, V = inner_cv, fold_fun = folds_vfold)

    body(inner_cv_fun)[[3]] <- inner_cv

  } else inner_cv_fun <- inner_cv

  return(list(
    inner_cv = inner_cv_fun,
    outer_cv = outer_cv_fun
  ))

}

create_cv_folds <- function(data, inner_cv, outer_cv) {

  if(is.null(outer_cv)) {

    # This outer set is only a helper and will be removed later
    outer_fold_inds <- list(
      list(training_set = 1:nrow(data))
    )

  } else if (is.function(outer_cv)) {

    outer_fold_inds <- outer_cv(data)

  } else if (is.list(outer_cv)) outer_fold_inds <- outer_cv

  # Use training set indices to repeatedly apply CV fold creation logic
  inner_fold_inds <- vector("list", length(outer_fold_inds))

  for (i in 1:length(inner_fold_inds)) {

    if(is.null(inner_cv)) {

      inner_fold_inds[[i]] <- list(
        list(
          training_set = outer_fold_inds[[i]]$training_set,
          validation_set = outer_fold_inds[[i]]$training_set
        )
      )

    } else if (is.function(inner_cv)) {

      inner_fold_inds[[i]] <- inner_cv(data = data[outer_fold_inds[[i]]$training_set,])

    } else if (is.list(inner_cv)) inner_fold_inds[[i]] <- inner_cv

  }

  # Change indices of inner folds to match full data set
  # Then, combine all into one list
  # Write corresponding function first

  match_indices <- function(outer_set, inner_sets) {

    lapply(inner_sets, function(x) {
      list(training_set = outer_set$training_set[x$training_set],
           validation_set = outer_set$training_set[x$validation_set])

    }
    )

  }

  nested_cv_folds <- list(performance_sets = vector("list", length(outer_fold_inds)),
                          build_sets = vector("list", length(outer_fold_inds)))

  # Indices only have to be matched if both inner and outer CV are performed
  # Write other specifications for cases where one isn't done
  if (is.null(outer_cv)) {

    nested_cv_folds$performance_sets <- NULL

    for (i in 1:length(inner_fold_inds)) {

      nested_cv_folds$build_sets[[i]] <- lapply(inner_fold_inds[[i]],
                                                function(x) list(
                                                  training_set = x$training_set,
                                                  validation_set = x$validation_set
                                                )
      )

    }

  } else if (is.null(inner_cv)) {

    nested_cv_folds$performance_sets <- lapply(outer_fold_inds,
                                               function(x) list(
                                                 training_set = x$training_set,
                                                 validation_set = x$validation_set
                                               )
    )

    nested_cv_folds$build_sets <- NULL

  } else {


    for (i in 1:length(outer_fold_inds)) {

      nested_cv_folds[[1]][[i]] <- list(training_set = outer_fold_inds[[i]]$training_set,
                                        validation_set = outer_fold_inds[[i]]$validation_set)

      nested_cv_folds[[2]][[i]] <- match_indices(outer_fold_inds[[i]], inner_fold_inds[[i]])


    }

  }

  return(nested_cv_folds)

}

get_lrn_packages <- function(learner_list, metalearners) {

  lrnr_pkg_list <- lapply(learner_list, function(x) {

    fit_pkgs <- unique(unlist(lapply(fun_calls(x$fit), find)))
    pred_pkgs <- unique(unlist(lapply(fun_calls(x$preds), find)))

    return(c(fit_pkgs, pred_pkgs))

  })

  metalearn_pkgs <- lapply(metalearners, function(x) lapply(fun_calls(x), find))

  pkgs_only <- unlist(c(lrnr_pkg_list, metalearn_pkgs))[substr(unlist(c(lrnr_pkg_list, metalearn_pkgs)), 1, 8) == "package:"]

  pkg_vec <- substr(pkgs_only, 9, 100)

  return(pkg_vec)

}

make_loss_list <- function(loss) {

  if (loss == "gaussian") {

    loss_fun <- function(y, preds) mean((y - preds)^2)

    loss_fun_gradient <- function(y, preds_list, params) {

      # Calculate loss here, output numeric vector
      alp <- do.call("cbind", preds_list)

      return(crossprod(alp) %*% params - t(alp) %*% y)

    }

    loss_fun_list <- list(
      loss_fun = loss_fun,
      loss_fun_gradient = loss_fun_gradient
    )

  } else if (loss == "binomial") {

    loss_fun <- function(y, preds) (y * log(preds)) + ((1 - y) * log(1 - preds))

    loss_fun_gradient <- function(y, preds_list, params) {

      # Calculate loss here, output numeric vector
      alp <- do.call("cbind", preds_list)

      return(t(alp) %*% (y - (alp %*% params)))

    }

    loss_fun_list <- list(
      loss_fun = loss_fun,
      loss_fun_gradient = loss_fun_gradient
    )

  } else if (loss == "poisson") {

    stop("Poisson not yet implemented.")

  }

}


#' Title
#'
#' @param learner_list A list of learners, where each learner is specified as a list with entries name (character string), fit (function of data), and preds (function of fit and object)
#' @param inner_cv Either NULL, a positive integer indicating the amount of folds, or a function of data, for construction of the superlearner.
#' @param outer_cv Either NULL, a positive integer indicating the amount of folds, or a function of data, for validation of the superlearner on held-out folds.
#' @param loss The loss function used to estimate weights for ensembles. Either one of 'gaussian', 'binomial', and 'poisson', or a list with functions 'loss_fun' and 'loss_fun_gradient'
#' @param metalearners How the learners are to be ensembled. One of 'NNLogLik', 'select', or a custom function with arguments 'y', 'preds_list', and 'loss_fun_list'.
#'
#' @returns An initialization object to be used for superlearning.
#' @import future.apply
#' @import nloptr
#' @import origami
#' @import pryr
#' @export
#'
#' @examples
#' data(iris)
learner_setup <- function(learner_list, inner_cv = 5, outer_cv = 5, loss = "gaussian",
                          metalearners = list(NNLogLik = "NNLogLik")) {

  # Extracting a function accessing the outcome

  # Options for metalearner: "NNLogLik", "NNLS", "select", custom function
  # Options for loss: "MSE", "LogLik, custom function

  # Generate weight-creating metalearning function depending on input
  # An AUC-based metalearner might be a nice addition for the future

  # Check that all metalearners specified have a name
  if (any(names(metalearners) == "")) {
    stop("Please give each metalearner in the metalearner list a name.")
  }

  metalearner_list <- vector("list", length(metalearners))
  metalearner_names <- rep(NA, length(metalearners))

  # Create a list for the provided loss function
  if (!is.character(loss) & !is.list(loss)) {
    "Please provide either a string or a list with (a) loss function(s) for loss."
  }

  if (is.character(loss) &
      (!loss %in% c("gaussian", "binomial", "poisson"))) {
    stop("If not providing (a) custom function(s), please provide a valid loss.")
  }

  if (is.character(loss)) {

    loss_fun_list <- make_loss_list(loss)

  } else {

    loss_fun_list <- loss

  }

  # Create all metalearners

  for (i in 1:length(metalearner_list)) {

    metalearner <- metalearners[[i]]
    metalearner_name <- names(metalearners)[[i]]

    if (!is.function(metalearner)) {

      if (metalearner != "NNLogLik" & metalearner != "select") {

        stop("Please specify a valid option or a custom function for all metalearners.")

      }

      if (metalearner == "select") {

        meta_fx <- function(y, preds_list, loss_fun_list) {

          # Calculate loss for each learner in list
          all_losses <- sapply(preds_list, function(x) loss_fun_list$loss_fun(y = y, preds = x))

          best <- which.min(sapply(all_losses, function(x) x))

          weights <- rep(0, length(preds_list))

          weights[[best]] <- 1

          names(weights) <- names(preds_list)

          return(weights)

        }


      } else if (metalearner == "NNLogLik") {

        meta_fx <- function(y, preds_list, loss_fun_list) {

          # Ensure that gradient is provided for the superlearner-metalearner
          if (is.null(loss_fun_list$loss_fun_gradient)) {
            stop("For the superlearner metalearner, please provide the gradient of the loss function.")
          }

          # Get nloptr-format loss-functions
          get_ensemble_from_preds <- function(y, loss_fun, preds_list, params) apply(sapply(preds_list, cbind) %*% params, 2, loss_fun, y = y)
          nloptr_loss <- function(x) get_ensemble_from_preds(y = y, loss_fun = loss_fun_list$loss_fun, preds_list = preds_list, params = x)

          nloptr_gradient <- function(x) loss_fun_list$loss_fun_gradient(y = y, preds_list = preds_list, params = x)

          # Equality constraint: sum of coefficients = 1
          eval_g_eq <- function(x) {
            sum(x) - 1
          }

          # Inequality constraint: all coefficients >= 0
          eval_g_ineq <- function(x) {
            -x # Each beta_i should be >= 0, so -beta <= 0
          }

          # Jacobian for equality constraint
          jacobian_eq <- function(x) {
            matrix(1, nrow = 1, ncol = length(x)) # A row vector of 1s
          }

          # Jacobian for inequality constraint
          jacobian_ineq <- function(x) {
            diag(-1, length(x)) # A diagonal matrix with -1 on the diagonal
          }

          # Starting values
          init_params <- rep(1/length(preds_list), length(preds_list))

          # Other args
          result <- nloptr(
            x0 = init_params,             # Initial guess
            eval_f = nloptr_loss,     # Objective function
            eval_grad_f = nloptr_gradient,
            eval_g_eq = eval_g_eq,
            eval_g_ineq = eval_g_ineq,
            eval_jac_g_eq = jacobian_eq,
            eval_jac_g_ineq = jacobian_ineq,
            opts = list(algorithm = "NLOPT_LD_SLSQP", # Optimization algorithm
                        xtol_rel = 1e-8,
                        maxeval = 1e8),               # Convergence tolerance
          )

          sl_coefs <- result$solution
          names(sl_coefs) <- names(preds_list)

          return(sl_coefs)

        }

      }

    } else if (is.function(metalearner)) {

      meta_fx <- metalearner

    }

    metalearner_list[[i]] <- meta_fx
    metalearner_names[[i]] <- names(metalearners)[[i]]

  }

  names(metalearner_list) <- metalearner_names


  # Create a config-file. Should eventually be adjustable.
  config <- list(
    w_tol = 0.001,
    remove_zero_weights = FALSE
  )

  # Get dependencies
  depends_pkgs <- get_lrn_packages(learner_list = learner_list, metalearners = metalearner_list)

  # Already save the return list, except CV
  return_list <- list(
    learner_list = learner_list,
    metalearners = metalearner_list,
    loss_fun_list = loss_fun_list,
    future_pkgs = depends_pkgs
  )

  # Create folds
  all_cv_funs <- make_cv_funs(inner_cv, outer_cv)

  return_list$inner_fold_creator <- all_cv_funs$inner_cv
  return_list$outer_fold_creator <- all_cv_funs$outer_cv

  return_list$config <- config

  # Assign this list a setup-class
  class(return_list) <- "LazySL_Setup"

  # Now return
  return(return_list)

}


# Train ensemble nuisance

train_ensemble_nuisance <- function(x, y, cv_list, metalearners, learner_list, loss_fun_list, w_tol) {

  ### Fit to all existing nested folds

  # Define a function that provides predictions
  fit_across_folds <- function(x, y, learner, folds) {

    # Save order of unlisted validation sets
    valid_order <- order(unlist(lapply(folds, function(x) x$validation_set)))

    all_preds <- lapply(folds,
                        function(fld) predict(object = fit(learner, x = x[fld$training_set,], y = y[fld$training_set]), newdata = x[fld$validation_set,])
    )

    # Get predictions into the correct order
    return(unlist(all_preds)[valid_order])

  }

  # Use function across all sets of fit and prediction function couplings
  # Then save as data frame with requested learner names
  preds_list <- lapply(learner_list, fit_across_folds, y = y, folds = cv_list, x = x)
  names(preds_list) <- lapply(learner_list, function(x) x$name)

  # Apply metalearners to prediction df iteratively
  wt_list <- lapply(metalearners, function(mtl) mtl(y = y[sort(unlist(cv_list[[1]]))], preds_list = preds_list, loss_fun_list = loss_fun_list))

  # Then, round according to weight tolerance and rescale
  rounded_wt_list <- lapply(wt_list, function(x) ifelse(abs(x) < w_tol, 0, x) / sum(ifelse(abs(x) < w_tol, 0, x)))


  # Round weights down if below tolerance and return
  return(rounded_wt_list)

}


# Function for training models after generating ensembles
fit_ensemble <- function(x, y, cv_list, learner_list, future_pkgs) {

  # Ensure that training happens on all data if no ensemble CV happens
  if (is.null(cv_list)) {

    cv_list <- list(list(training_set = 1:length(y)))

  }

  # Train across performance sets
  # First, define a function training all learners on a data set
  fit_all_learns <- function(learner_list, x, y) {

    lapply(learner_list, function(lrns) fit(lrns, x, y))

  }

  # Then, apply learners to all training sets
  all_learned_list <- future_lapply(cv_list,
                                    function(flds) fit_all_learns(
                                      learner_list = learner_list, x = x[flds$training_set,], y = y[flds$training_set]),
                                    future.packages = future_pkgs, future.seed = TRUE
  )

  return(all_learned_list)

}

get_losses_across_ensembles <- function(x, y, cv_list, get_all_learners, get_all_ensembles, loss_fun) {

  # Get indices of validation sets on which you can calculate losses
  val_sets <- lapply(cv_list, function(flds) flds$validation_set)

  # Apply all ensembles to all validation sets
  calc_one_ensemble <- function(y_ss, val_set, learner_subset, ensemble_subset, loss_fun) {


    # Get all predictions first
    all_preds <- vector("list", length(learner_subset))
    for (i in 1:length(all_preds)) all_preds[[i]] <- predict(learner_subset[[i]], newdata = val_set)

    # Now apply all ensembles to predictions
    pred_mat <- do.call("cbind", all_preds)
    colnames(pred_mat) <- names(learner_subset)

    return(sapply(ensemble_subset, function(x) loss_fun(y_ss, pred_mat %*% x)))

  }

  # Now calculate across all folds
  all_fold_losses <- vector("list", length(cv_list))

  for (i in 1:length(val_sets)) {

    all_fold_losses[[i]] <- calc_one_ensemble(y[val_sets[[i]]], x[val_sets[[i]],], get_all_learners[[i]], get_all_ensembles[[i]], loss_fun)

  }


  # Now find the minimizer
  return(names(which.min(colMeans(do.call("rbind", all_fold_losses)))))

}



###### Full function

#' Title
#'
#' @param y Either the outcome variable itself, or a character string containing it.
#' @param data A data.frame or matrix containing all data (both X and y).
#' @param init An initialization object.
#'
#' @returns A list of class lazycv containing results of the superlearner.
#' @export
#' @import future.apply
#' @import nloptr
#'
#' @examples
#' data(iris)
lazy_cv <- function(x, y, init) {

  if(class(init) != "LazySL_Setup") stop("Please provide a LazySL_Setup object as initialization list.")

  # Check whether data is a data frame or a matrix
  if (!is.matrix(x) & !is.data.frame(x)) {

    stop("Please provide either a data frame or a matrix for x")

  }


  # Now create CV folds to use later
  init$cv <- create_cv_folds(x, init$inner_fold_creator, init$outer_fold_creator)


  if (is.null(init$cv$build_sets)) {

    if (length(init$learner_list) > 1) {

      stop("If you do not do inner CV, you can only use one learner.")

    }

    warning("Only one learner specified. Metalearners are ignored.")

    one_vec <- c(1)
    names(one_vec) <- init$learner_list[[1]]$name

    get_all_ensembles <- future_lapply(replicate(length(init$cv$performance_sets), one_vec, simplify = FALSE),
                                       function(x) list(Learner = x), future.seed = TRUE)

  } else {

    # First, train all ensembles iteratively
    get_all_ensembles <- future_lapply(init$cv$build_sets,
                                       train_ensemble_nuisance,
                                       x = x,
                                       y = y,
                                       learner_list = init$learner_list,
                                       metalearners = init$metalearners,
                                       loss_fun_list = init$loss_fun_list,
                                       w_tol = init$config$w_tol,
                                       future.packages = init$future_pkgs,
                                       future.seed = TRUE
    )

  }

  # Then, get all learners trained on outer fold training sets
  get_all_learners <- fit_ensemble(
    x,
    y,
    init$cv$performance_sets,
    init$learner_list,
    init$future_pkgs
  )

  # Some information on CV for output
  ensemble_cv <- ifelse(!is.null(init$cv$performance_sets), TRUE, FALSE)

  metalearner_count <- length(init$metalearners)

  # List with all that's needed...
  return_list <- list(
    x = x,
    y = y,
    learners = init$learner_list,
    metalearners = init$metalearners,
    metalearner_count = metalearner_count,
    loss_fun_list = init$loss_fun_list,
    cv = init$cv,
    config = init$config,
    ensembles = get_all_ensembles,
    fit_objects = get_all_learners,
    was_cv_ensemble = ensemble_cv
  )

  # If the ensemble is cross-validated, also provide the best-performing one
  if (ensemble_cv == TRUE & metalearner_count > 1) {

    return_list$best_metalearner <- get_losses_across_ensembles(x, y, init$cv$performance_sets,
                                                                init$learner_list, get_all_learners, get_all_ensembles,
                                                                init$loss_fun_list$loss_fun)

  } else if (ensemble_cv == TRUE) return_list$best_metalearner <- names(init$metalearners)

  return_list$had_metalearner <- TRUE

  # Except if there is only one learner, in which case we don't really have a metalearner
  if (length(init$learner_list) == 1) {

    return_list$metalearners <- NULL
    return_list$metalearner_count <- NULL
    return_list$had_metalearner <- FALSE

  }

  class(return_list) <- "LazySL"

  return(return_list)


}
