
### Viable upper list structure
# Data bank
# Fold specification
# List of learners
# Metalearner
# Counterfactual treatment values

### Viable learner list structure
# Name
# Function taking in data, returning fit object
# Function for making predictions
# Optional: Function with other stuff to return
# Optional and a to-do: grids for hyperparameter tuning

get_lrn_packages <- function(learners, metalearners) {

  lrnr_pkg_list <- lapply(learners, function(x) {

    fit_pkgs <- unique(unlist(lapply(get_funs(x$fit), function(f) {
      loc <- utils::getAnywhere(f)$where
      if (is.null(loc)) return(NULL)
      pkgs <- grep("package:|namespace:", loc, value = TRUE)
      gsub("package:|namespace:", "", pkgs)
    })))


    pred_pkgs <- unique(unlist(lapply(get_funs(x$preds), function(f) {
      loc <- utils::getAnywhere(f)$where
      if (is.null(loc)) return(NULL)
      pkgs <- grep("package:|namespace:", loc, value = TRUE)
      gsub("package:|namespace:", "", pkgs)
    })))

    return(c(fit_pkgs, pred_pkgs))

  })

  mtl_pkg_list <- lapply(metalearners, function(x) {

    fit_pkgs <- unique(unlist(lapply(get_funs(x$fit), function(f) {
      loc <- utils::getAnywhere(f)$where
      if (is.null(loc)) return(NULL)
      pkgs <- grep("package:|namespace:", loc, value = TRUE)
      gsub("package:|namespace:", "", pkgs)
    })))


    pred_pkgs <- unique(unlist(lapply(get_funs(x$preds), function(f) {
      loc <- getAnywhere(f)$where
      if (is.null(loc)) return(NULL)
      pkgs <- grep("package:|namespace:", loc, value = TRUE)
      gsub("package:|namespace:", "", pkgs)
    })))

    return(c(fit_pkgs, pred_pkgs))

  })

  pkgs_only <- unlist(c(lrnr_pkg_list, mtl_pkg_list))[substr(unlist(c(lrnr_pkg_list, mtl_pkg_list)), 1, 8) == "package:"]

  pkg_vec <- unique(c(substr(pkgs_only, 9, 100), "nanatools"))

  return(pkg_vec)

}

# ── Type-agnostic prediction combiner ────────────────────────────────────

combine_preds <- function(chunks) {
  first <- chunks[[1L]]
  if (is.data.frame(first) || is.matrix(first))
    do.call(rbind, chunks)
  else
    do.call(c, chunks)
}


#' Title
#'
#' @param learners A list of learners, where each learner is specified as a list with entries name (character string), fit (function of data), and preds (function of fit and object)
#' @param inner_cv Either NULL, a positive integer indicating the amount of folds, or a function of data, for construction of the superlearner.
#' @param outer_cv Either NULL, a positive integer indicating the amount of folds, or a function of data, for validation of the superlearner on held-out folds.
#' @param loss A list of class \code{mtl_loss}, containing all information the user needs for ensembling. For most cases, can use one of: \code{loss_gaussian()}, \code{loss_binomial()}, \code{loss_poisson()}, \code{loss_gamma()}
#' @param metalearners How the learners are to be ensembled. Can be NULL, which makes each learner its own metalearner. \code{mtl_selector()} adds a discrete superlearner, while \code{mtl_fw()} adds a standard superlearner.
#'
#' @returns An initialization object to be used for superlearning.
#' @import future.apply
#' @import nloptr
#' @import origami
#' @export
#'
#' @examples
#' data(iris)
setup_ensemble <- function(learners, metalearners = list(mtl_selector("Selected")), loss = loss_gaussian()) {

  # Generate weight-creating metalearning function depending on input
  # An AUC-based metalearner might be a nice addition for the future

  # Check that names across learners and metalearners are unique

  get_lrn_names <- c(lapply(learners, function(x) x$name),
                     lapply(metalearners, function(x) x$name))

  if (length(get_lrn_names) != length(unique(get_lrn_names))) {
    stop("Please ensure that learner names are unique.\nNames can also not be shared between learners and metalearners.")
  }

  # Check whether loss function resolved to a list
  if (!inherits(loss, "mtl_loss")) {
    stop("Please ensure the provided loss is a list of class 'mtl_loss'.\n",
         "You can specify a custom loss function via 'loss_custom'. Check the documentation for more details.")
  }

  # Get dependencies
  depends_pkgs <- get_lrn_packages(learners = learners, metalearners = metalearners)

  # Already save the return list, except CV
  return_list <- list(
    learners = learners,
    metalearners = metalearners,
    loss_fun_list = loss,
    future_pkgs = depends_pkgs
  )


  # Assign this list a setup-class
  class(return_list) <- "Ensemble_Setup"
  return(return_list)

}


#' Fit a cross-validated SuperLearner
#'
#' @param init An initialization object of class \code{Ensemble_Setup} created
#'   via \code{setup_ensemble()}.
#' @param cv_instructions Either a \code{nana_cv} object created via
#'   \code{create_cv_folds()} or \code{add_cv_folds()}, or an integer vector
#'   of length 2 giving \code{c(inner_cv, outer_cv)} fold counts (either may
#'   be \code{NA} or \code{NULL} to indicate no CV at that level).
#' @param x A data frame or matrix of predictor variables.
#' @param y A numeric vector of outcome values.
#' @param return_data Logical. If \code{TRUE} (default), \code{x} and
#'   \code{y} are stored in the returned object, enabling standalone
#'   prediction without re-supplying data. Set to \code{FALSE} in pipeline
#'   use where the task is the source of truth.
#'
#' @return An S3 object of class \code{Ensemble}.
#' @export
#' @import future.apply
train_ensemble <- function(init, cv_instructions, x, y, return_data = TRUE) {

  # ── 1. Validate inputs ────────────────────────────────────────────────────
  if (!inherits(init, "Ensemble_Setup"))
    stop("Please provide an Ensemble_Setup object as `init`.")
  if (!is.matrix(x) && !is.data.frame(x))
    stop("`x` must be a data frame or matrix.")

  y_len <- if (is.vector(y)) length(y) else nrow(y)

  if (nrow(x) != y_len)
    stop("`x` and `y` must have the same number of observations.")

  # ── 2. Resolve cv_instructions → nana_cv ──────────────────────────────────
  if (inherits(cv_instructions, "nana_cv")) {
    cv <- cv_instructions
  } else {
    if (length(cv_instructions) != 2 || !is.numeric(cv_instructions)) {
      stop("If cv_instructions is not already a list created via create_cv_folds,",
           " it needs to be an integer vector of length 2.")
    }
    cv <- create_cv_folds(nrow(x), cv_instructions[[1]], cv_instructions[[2]])
  }

  # One typecheck here after resolving CV instructions
  if (!is.null(init$metalearners) && is.null(cv$build_sets)) {
    warning("Metalearners passed without inner cross-validation, which is required to build them. They are removed")
    init$metalearners <- NULL
  }

  # ── 3. Determine whether outer CV is being used ───────────────────────────
  ensemble_cv <- !is.null(cv$performance_sets)

  # ── 4. Build ensembles (inner CV) ─────────────────────────────────────────
  # build_ensembles() fits each learner on each inner fold, collects
  # predictions, fits metalearners, and discards intermediate objects.
  # Returns: list of length n_outer_folds, each a list of fitted metalearners.
  get_all_ensembles <- if (!is.null(cv$build_sets)) {
    build_ensembles(
      cv           = cv,
      learners     = init$learners,
      metalearners = init$metalearners,
      loss_fun_list = init$loss_fun_list,
      x            = x,
      y            = y,
      future_pkgs  = init$future_pkgs
    )
  } else {
    # No inner CV — empty ensemble slot per outer fold (or one if no outer CV)
    # Ensures that learners that are "made to be metalearners" have something to be appended to right below
    n_slots <- if (ensemble_cv) length(cv$performance_sets) else 1L
    vector("list", n_slots)
  }

  # ── 5. Append per-learner "fake" metalearners ─────────────────────────────
  # This preserves the original behaviour: each individual learner is
  # accessible via the same prediction machinery as a real ensemble.
  add_mtl_list <- lapply(seq_along(init$learners), function(i)
    fit(mtl_learner(init$learners[[i]]$name, i))
  )
  get_all_ensembles <- lapply(get_all_ensembles, c, add_mtl_list)

  # ── 6. Fit learners on outer fold training sets ───────────────────────────
  # If no outer CV, train on the full dataset (one synthetic fold).
  perf_folds <- if (ensemble_cv) {
    cv$performance_sets
  } else {
    new_fold_list(list(new_fold(
      validation_set = seq_len(nrow(x)),
      training_set   = seq_len(nrow(x)),
      n              = nrow(x)
    )))
  }

  # fit_ensemble() returns: list over learners, each a list over folds
  # get_all_learners[[j]][[i]] = j-th learner fitted on i-th fold training set
  get_all_learners <- fit_ensemble(
    x           = x,
    y           = y,
    perf_folds  = perf_folds,
    learners    = init$learners,
    future_pkgs = init$future_pkgs
  )

  # ── 7. Find best metalearner via cross-validated loss ────────────────────
  best_metalearner <- if (ensemble_cv) {
    find_best_metalearner(
      x             = x,
      y             = y,
      perf_folds    = cv$performance_sets,
      all_learners  = get_all_learners,
      all_ensembles = get_all_ensembles,
      loss_fun      = init$loss_fun_list$loss_fun
    )
  } else NULL

  # ── 8. Assemble return object ─────────────────────────────────────────────
  return_list <- list(
    x                 = if (return_data) x else NULL,
    y                 = if (return_data) y else NULL,
    learners          = init$learners,
    metalearners      = init$metalearners,
    metalearner_count = length(init$metalearners),
    loss_fun_list     = init$loss_fun_list,
    cv                = cv,
    ensembles         = get_all_ensembles,
    fit_objects       = get_all_learners,
    was_cv_ensemble   = ensemble_cv,
    best_metalearner  = best_metalearner
  )

  class(return_list) <- "Ensemble"
  return_list
}

# ── Helper: fit learners on each fold's training set ──────────────────────

#' Fit all learners on each performance fold's training set
#'
#' @param x Full predictor matrix or data frame.
#' @param y Full outcome vector.
#' @param perf_folds A \code{nana_fold_list} of performance (outer) folds.
#' @param learners A list of \code{SL_Learner} objects.
#' @param future_pkgs Character vector of packages for parallel workers.
#' @return A list of length \code{n_learners}, each a list of length
#'   \code{n_folds} containing fitted learner objects.
fit_ensemble <- function(x, y, perf_folds, learners, future_pkgs) {
  future.apply::future_lapply(perf_folds, function(fold) {
    tr <- training_set(fold)
    lapply(learners, function(lrn)
      fit(lrn, x[tr, , drop = FALSE], subset_y(y, tr))
    )
  }, future.packages = future_pkgs,
  future.globals = list(
    x             = x,
    y             = y,
    learners      = learners
  ),
  future.seed = TRUE)
}


# ── Helper: find best metalearner across outer folds ──────────────────────

#' Compute mean CV loss per metalearner and return the best name
#'
#' @param x Full predictor matrix or data frame.
#' @param y Full outcome vector.
#' @param perf_folds A \code{nana_fold_list} of performance folds.
#' @param all_learners Output of \code{fit_ensemble()}: list over learners,
#'   each a list over folds.
#' @param all_ensembles Output of \code{build_ensembles()} with fake
#'   per-learner metalearners appended: list over outer folds, each a list
#'   of fitted metalearner objects.
#' @param loss_fun A function \code{function(y, preds)} returning a scalar
#'   loss.
#' @return Character scalar naming the metalearner with minimum mean CV loss.
find_best_metalearner <- function(x, y, perf_folds, all_learners,
                                  all_ensembles, loss_fun) {

  n_folds <- length(perf_folds)

  fold_losses <- lapply(seq_len(n_folds), function(i) {
    fold <- perf_folds[[i]]
    val  <- validation_set(fold)

    # Predictions from each learner on this fold's validation set
    # all_learners[[j]][[i]] = j-th learner fitted on i-th fold
    preds_list <- lapply(seq_along(all_learners[[i]]), function(j)
      predict(all_learners[[i]][[j]], newdata = x[val, , drop = FALSE])
    )
    names(preds_list) <- vapply(all_learners[[i]], `[[`, character(1L), "name")

    y_val <- subset_y(y, val)

    # Loss per metalearner on this fold
    vapply(all_ensembles[[i]], function(mtl) {
      ensembled <- predict(mtl, preds_list)
      mean(loss_fun(y_val, ensembled))
    }, numeric(1L))
  })

  # Average loss across folds; names come from the first fold's metalearners
  loss_mat   <- do.call(rbind, fold_losses)
  mean_loss  <- colMeans(loss_mat)

  # Metalearner names — use the name slot on each fitted metalearner
  mtl_names  <- vapply(all_ensembles[[1L]], `[[`, character(1L), "name")
  names(mean_loss) <- mtl_names

  names(which.min(mean_loss))
}


#' Fit ensembles across inner folds without retaining fitted learner objects
#'
#' For each set of inner folds (one per outer fold), fits every learner on
#' each training set, immediately collects validation predictions as raw
#' learner output (preserving type), then discards the fitted object.
#' Predictions across folds are combined per learner and passed to
#' metalearners. Duplicate validation indices (bootstrap, repeated CV) are
#' preserved — the metalearner receives all predictions alongside the
#' corresponding y values.
#'
#' @param cv A \code{nana_cv} with \code{build_sets} populated.
#' @param learners A list of \code{SL_Learner} objects.
#' @param metalearners A list of metalearner objects.
#' @param loss_fun_list A \code{mtl_loss} object.
#' @param x Full predictor matrix or data frame.
#' @param y Full outcome vector.
#' @param future_pkgs Character vector of packages for parallel workers.
#' @return A list of length \code{length(cv$build_sets)}, each element a
#'   list of fitted metalearner objects (one per metalearner).
#' @export
build_ensembles <- function(cv, learners, metalearners, loss_fun_list,
                            x, y, future_pkgs = character(0L)) {
  if (!inherits(cv, "nana_cv"))
    stop("`cv` must be a nana_cv object.")
  if (is.null(cv$build_sets))
    stop("No build folds found. Please add them before calling build_ensembles().")

  lrn_names <- vapply(learners, `[[`, character(1L), "name")

  future.apply::future_lapply(cv$build_sets, function(inner_folds) {

    # ── Step 1: accumulate (idx, raw predictions) per fold ────────────────
    # fold_results: list over folds, each a list with $idx and $preds
    # $preds is a named list over learners containing raw learner output
    fold_results <- vector("list", length(inner_folds))

    ### If I ever want to implement optimization on grids (say random sample from CV setups),
    ### I think I would need to swap the order from iterate folds -> iterate learners to learners -> folds.
    ### Then finishing an inner iteration trains a learner on each fold, which is where grid logic could live.
    for (k in seq_along(inner_folds)) {
      fold  <- inner_folds[[k]]
      tr    <- training_set(fold)
      val   <- validation_set(fold)
      x_tr  <- x[tr,  , drop = FALSE]
      y_tr  <- subset_y(y, tr)
      x_val <- x[val, , drop = FALSE]

      fold_preds <- vector("list", length(learners))
      names(fold_preds) <- lrn_names

      for (j in seq_along(learners)) {
        fitted_lrn       <- fit(learners[[j]], x_tr, y_tr)
        fold_preds[[j]]  <- predict(fitted_lrn, newdata = x_val)
        rm(fitted_lrn)
      }

      fold_results[[k]] <- list(idx = val, preds = fold_preds)
      rm(x_tr, y_tr, x_val)
    }

    # ── Step 2: Combine predictions across folds
    all_idx   <- unlist(lapply(fold_results, `[[`, "idx"))
    all_preds_per_learner <- lapply(lrn_names, function(nm)
      combine_preds(lapply(fold_results, function(fr) fr$preds[[nm]]))
    )
    names(all_preds_per_learner) <- lrn_names
    y_sub <- structure(subset_y(y, all_idx), indices = all_idx)

    # ── Step 3: fit metalearners — preds_list goes out of scope after ──────

    fitted_metalearners <- lapply(metalearners, function(mtl)
      fit(mtl, y = y_sub, preds_list = all_preds_per_learner,
          loss_fun_list = loss_fun_list)
    )

    rm(fold_results, all_preds_per_learner, y_sub)
    structure(fitted_metalearners, indices = all_idx)

  }, future.packages = c(future_pkgs, "nanatools"),
  future.globals = list(
    x             = x,
    y             = y,
    learners      = learners,
    metalearners  = metalearners,
    loss_fun_list = loss_fun_list,
    lrn_names     = lrn_names,
    combine_preds = combine_preds
  ),
  future.seed = TRUE)
}
