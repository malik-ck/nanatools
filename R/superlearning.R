
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

### When predicting with cv, should ensure original and new data have same number of rows

make_cv_funs <- function(inner_cv, outer_cv) {

  # Check type for CV
  cv_checker <- function(object) {

    if (!is.null(object) & !is.function(object) &
        ((!is.numeric(object)) | (is.numeric(object) && (length(object) != 1 || object < 2))) &
        (!is.list(object))) {

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

get_lrn_packages <- function(learners, metalearners) {

  lrnr_pkg_list <- lapply(learners, function(x) {

    fit_pkgs <- unique(unlist(lapply(fun_calls(x$fit), find)))
    pred_pkgs <- unique(unlist(lapply(fun_calls(x$preds), find)))

    return(c(fit_pkgs, pred_pkgs))

  })

  mtl_pkg_list <- lapply(metalearners, function(x) {

    fit_pkgs <- unique(unlist(lapply(fun_calls(x$fit), find)))
    pred_pkgs <- unique(unlist(lapply(fun_calls(x$preds), find)))

    return(c(fit_pkgs, pred_pkgs))

  })

  pkgs_only <- unlist(c(lrnr_pkg_list, mtl_pkg_list))[substr(unlist(c(lrnr_pkg_list, mtl_pkg_list)), 1, 8) == "package:"]

  pkg_vec <- unique(c(substr(pkgs_only, 9, 100), "nanatools"))

  return(pkg_vec)

}

make_loss_list <- function(loss) {

  if (loss == "gaussian") {

    loss_fun <- function(y, preds) mean((y - preds)^2)

    loss_fun_gradient <- function(y, preds_list, params) {

      # Calculate loss here, output numeric vector
      alp <- do.call("cbind", preds_list)

      return(as.vector(crossprod(alp) %*% params - t(alp) %*% y))

    }

    loss_fun_list <- list(
      loss_fun = loss_fun,
      loss_fun_gradient = loss_fun_gradient
    )

  } else if (loss == "binomial") {

    loss_fun <- function(y, preds) -mean((y * log(pmin(pmax(preds, 1e-15), 1 - 1e-15))) + ((1 - y) * log(pmin(pmax(1 - preds, 1e-15), 1 - 1e-15))))

    loss_fun_gradient <- function(y, preds_list, params) {

      # Calculate loss here, output numeric vector
      alp <- do.call("cbind", preds_list)

      return(as.vector(t(alp) %*% (y - (alp %*% params))))

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
#' @param learners A list of learners, where each learner is specified as a list with entries name (character string), fit (function of data), and preds (function of fit and object)
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
learner_setup <- function(learners, inner_cv = 5, outer_cv = 5, loss = "gaussian",
                          metalearners = list(mtl_selector("Selected"))) {



  # Extracting a function accessing the outcome

  # Options for metalearner: "NNLogLik", "NNLS", "select", custom function
  # Options for loss: "MSE", "LogLik, custom function

  # Generate weight-creating metalearning function depending on input
  # An AUC-based metalearner might be a nice addition for the future

  # Check that names across learners and metalearners are unique

  get_lrn_names <- c(lapply(learners, function(x) x$name),
                     lapply(metalearners, function(x) x$name))

  if (length(get_lrn_names) != length(unique(get_lrn_names))) {
    stop("Please ensure that learner names are unique.\nNames can also not be shared between learners and metalearners.")
  }

  # Ensure that no metalearner is provided if inner_cv is NULL.
  if (is.null(inner_cv) & !is.null(metalearners)) {
    stop("If inner_cv is NULL, metalearners also needs to be NULL.")
  }

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

  # Create a config-file. Should eventually be adjustable.
  config <- list(
    w_tol = 0.001,
    remove_zero_weights = FALSE
  )

  # Get dependencies
  depends_pkgs <- get_lrn_packages(learners = learners, metalearners = metalearners)

  # Already save the return list, except CV
  return_list <- list(
    learners = learners,
    metalearners = metalearners,
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

train_ensemble_nuisance <- function(x, y, cv_list, metalearners, learners, loss_fun_list, w_tol) {

  # Fit to all existing nested folds

  # Define a function that provides predictions
  fit_across_folds <- function(x, y, learner, folds) {

    # Save order of unlisted validation sets
    valid_order <- order(unlist(lapply(folds, function(x) x$validation_set)))

    all_preds <- lapply(folds,
                        function(fld) predict(object = nanatools::fit(learner, x = x[fld$training_set,], y = y[fld$training_set]), newdata = x[fld$validation_set,])
    )

    # Get predictions into the correct order
    return(unlist(all_preds)[valid_order])

  }

  # Use function across all sets of fit and prediction function couplings
  # Then save as data frame with requested learner names
  preds_list <- lapply(learners, fit_across_folds, y = y, folds = cv_list, x = x)
  names(preds_list) <- lapply(learners, function(x) x$name)

  # Apply metalearners to predictions iteratively
  wt_list <- lapply(metalearners, function(mtl) fit(mtl, y = y[sort(unlist(cv_list[[1]]))], preds_list = preds_list, loss_fun_list = loss_fun_list))

  return(wt_list)

}

# Function for training models after generating ensembles
fit_ensemble <- function(x, y, cv_list, learners, future_pkgs) {

  # Train across performance sets
  # First, define a function training all learners on a data set
  fit_all_learns <- function(learners, x, y) {

    lapply(learners, function(lrns) nanatools::fit(lrns, x, y))

  }

  # Then, apply learners to all training sets
  all_learned_list <- future_lapply(cv_list,
                                    function(flds) fit_all_learns(
                                      learners = learners, x = x[flds$training_set,], y = y[flds$training_set]),
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

    # Now apply all ensembles to predictions and calculate losses
    ensembled_predictions <- lapply(ensemble_subset, function(mtl) predict(mtl, all_preds))
    ensembled_losses <- lapply(ensembled_predictions, function(x) loss_fun(y_ss, x))
    names(ensembled_losses) <- lapply(ensemble_subset, function(x) x$name)

    return(do.call("cbind", ensembled_losses))

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
#'
#' @examples
#' data(iris)
lazy_cv <- function(x, y, init) {

  if(class(init) != "LazySL_Setup") stop("Please provide a LazySL_Setup object as initialization list.")

  # Check whether data is a data frame or a matrix
  if (!is.matrix(x) & !is.data.frame(x)) {

    stop("Please provide either a data frame or a matrix for x")

  }

  # Check that, if outer_cv is NULL, inner_cv is only of length one
  if (is.null(init$performance_sets) & length(init$build_sets) > 1) {
    stop("If you do not outer cross-validation, you can only provide one set of inner folds.")
  }


  # Now create CV folds to use later
  init$cv <- create_cv_folds(x, init$inner_fold_creator, init$outer_fold_creator)


  # Then train all ensembles iteratively
  # If build sets are NULL, just instantiate an empty list

  if (is.null(init$cv$build_sets)) {
    get_all_ensembles <- lapply(1:length(init$cv$performance_sets), function(x) list())
  } else {
    get_all_ensembles <- future_lapply(init$cv$build_sets,
                                       train_ensemble_nuisance,
                                       x = x,
                                       y = y,
                                       learners = init$learners,
                                       metalearners = init$metalearners,
                                       loss_fun_list = init$loss_fun_list,
                                       w_tol = init$config$w_tol,
                                       future.packages = init$future_pkgs,
                                       future.seed = TRUE
    )
  }

  # We want to now define an ensemble for each learner.
  # This is kind of a fake ensemble, but allows the same machinery to predict for individual CV'd learners.
  add_mtl_list <- vector("list", length(init$learners))

  for (i in 1:length(add_mtl_list)) {
    add_mtl_list[[i]] <- fit(nanatools::mtl_learner(init$learners[[i]]$name, i))
  }

  # Now append this
  get_all_ensembles <- lapply(get_all_ensembles, function(x, new_lrn) c(x, new_lrn), new_lrn = add_mtl_list)


  # Then, get all learners trained on outer fold training sets.
  # If no outer CV, need to collect the full data from inner CV.
  # That means: no cross-validation, just train on the full data

  ensemble_cv <- ifelse(!is.null(init$cv$performance_sets), TRUE, FALSE)

  if (is.null(init$cv$performance_sets)) {
    init$cv$performance_sets <- unlist(init$cv$build_sets, recursive = FALSE)
  }

  get_all_learners <- fit_ensemble(
    x,
    y,
    init$cv$performance_sets,
    init$learners,
    init$future_pkgs
  )

  metalearner_count <- length(init$metalearners)

  if (!ensemble_cv) {
    init$cv$performance_sets <- NULL
  }

  # List with all that's needed...
  return_list <- list(
    x = x,
    y = y,
    learners = init$learners,
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
  if (ensemble_cv == TRUE) {

    return_list$best_metalearner <- get_losses_across_ensembles(x, y, init$cv$performance_sets,
                                                                get_all_learners, get_all_ensembles,
                                                                init$loss_fun_list$loss_fun)
  }

  class(return_list) <- "LazySL"

  return(return_list)


}
