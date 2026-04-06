
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
  names_lrn <- lapply(learners, function(x) {
    if (inherits(x, "SL_Pipeline")) enum_names <- x$path_names
    else enum_names <- x$name
    return(enum_names)
  })

  names_mtl <- lapply(metalearners, function(x) {
    if (inherits(x, "SL_Pipeline")) enum_names <- x$path_names
    else enum_names <- x$name
    return(enum_names)
  })

  enum_all_names <- c(unlist(names_lrn), unlist(names_mtl))

  if (length(enum_all_names) != length(unique(enum_all_names))) {
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
    loss_fun = loss,
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
    warning("Metalearners passed without inner cross-validation, which is required to build them. They are removed.")
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
      x            = x,
      y            = y,
      future_pkgs  = init$future_pkgs
    )
  }

  # ── 5. Fit learners on outer fold training sets ───────────────────────────
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

  # Train all learners
  get_all_learners <- fit_ensemble(
    x           = x,
    y           = y,
    perf_folds  = perf_folds,
    learners    = init$learners,
    future_pkgs = init$future_pkgs
  )

  # ── 6. Assemble return object ─────────────────────────────────────────────
  return_list <- list(
    x                 = if (return_data) x else NULL,
    y                 = if (return_data) y else NULL,
    learners          = init$learners,
    metalearners      = init$metalearners,
    metalearner_count = length(init$metalearners),
    loss_fun          = init$loss_fun,
    cv                = cv,
    ensembles         = get_all_ensembles,
    fit_objects       = get_all_learners,
    was_cv_ensemble   = ensemble_cv
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



#' Fit ensembles across inner folds without retaining fitted learner objects
#'
#' Iterates learners in the outer loop and folds in the inner loop, enabling
#' grid-based early stopping. For each learner, predictions are accumulated
#' across all folds before moving to the next learner. List outputs from
#' \code{SL_Pipeline} and \code{SL_Grid} objects are spliced into
#' \code{preds_list} as independent entries; list outputs from other learner
#' types are stored as-is.
#'
#' @param cv A \code{nana_cv} with \code{build_sets} populated.
#' @param learners A list of \code{SL_Learner} or \code{SL_Pipeline} objects.
#' @param metalearners A list of \code{SL_Metalearner} objects.
#' @param x Full predictor matrix or data frame.
#' @param y Full outcome vector or matrix.
#' @param future_pkgs Character vector of packages for parallel workers.
#' @return A list of length \code{length(cv$build_sets)}, each element a
#'   list of fitted metalearner objects with an \code{indices} attribute.
#' @export
build_ensembles <- function(cv, learners, metalearners,
                            x, y, future_pkgs = character(0L)) {
  if (!inherits(cv, "nana_cv"))
    stop("`cv` must be a nana_cv object.")
  if (is.null(cv$build_sets))
    stop("No build folds found. Please add them before calling build_ensembles().")

  splice_classes <- c("SL_Pipeline", "SL_Grid")

  should_splice <- function(obj, preds) {
    is.list(preds) &&
      !is.data.frame(preds) &&
      any(vapply(splice_classes, function(cl) inherits(obj, cl), logical(1L)))
  }

  future.apply::future_lapply(cv$build_sets, function(inner_folds) {

    fold_data <- lapply(inner_folds, function(fold) {
      tr  <- training_set(fold)
      val <- validation_set(fold)
      list(
        tr    = tr,
        val   = val,
        x_tr  = x[tr,  , drop = FALSE],
        y_tr  = subset_y(y, tr),
        x_val = x[val, , drop = FALSE]
      )
    })

    all_idx <- unlist(lapply(fold_data, `[[`, "val"))

    preds_list      <- list()
    failed_learners <- character(0L)

    for (j in seq_along(learners)) {
      lrn         <- learners[[j]]
      is_grid     <- inherits(lrn, "SL_Grid")
      lrn_nm      <- get_lrn_display_name(lrn)
      fold_chunks <- vector("list", length(inner_folds))

      for (k in seq_along(inner_folds)) {
        fd <- fold_data[[k]]

        fold_chunks[[k]] <- tryCatch({
          fitted_lrn <- fit(lrn, fd$x_tr, fd$y_tr)
          out        <- predict(fitted_lrn, newdata = fd$x_val)
          rm(fitted_lrn)
          list(idx = fd$val, preds = out, failed = FALSE)
        }, error = function(e) {
          warning(sprintf(
            "Learner '%s' failed on fold %d: %s",
            lrn_nm, k, conditionMessage(e)
          ))
          list(idx = fd$val, preds = NULL, failed = TRUE)
        })
      }

      # ── Grid early stopping ──────────────────────────────────────────────
      if (is_grid && !is.null(lrn$should_stop)) {
        surviving <- lrn$should_stop(fold_chunks, y, all_idx)
        for (nm in names(surviving))
          preds_list[[nm]] <- combine_preds(
            lapply(fold_chunks, function(fc) fc$preds[[nm]])
          )
        next
      }

      # ── Check for any fold failure ────────────────────────────────────
      failed_folds <- which(vapply(fold_chunks, `[[`, logical(1L), "failed"))

      if (length(failed_folds) > 0L) {
        warning(sprintf(
          "Learner '%s' failed on fold(s) %s and is excluded from the ensemble.",
          lrn_nm, paste(failed_folds, collapse = ", ")
        ))
        failed_learners <- c(failed_learners, lrn_nm)
        rm(fold_chunks)
        next
      }

      # ── Combine across folds ──────────────────────────────────────────
      # Check splice on the first fold's prediction, not on the combined result
      first_pred <- fold_chunks[[1L]]$preds

      if (should_splice(lrn, first_pred)) {
        # Pipeline/Grid: combine each path's predictions across folds separately
        path_nms <- names(first_pred)
        for (nm in path_nms) {
          path_chunks     <- lapply(fold_chunks, function(fc) fc$preds[[nm]])
          preds_list[[nm]] <- combine_preds(path_chunks)
        }
      } else {
        combined          <- combine_preds(lapply(fold_chunks, `[[`, "preds"))
        preds_list[[lrn_nm]] <- combined
        rm(combined)
      }

      rm(fold_chunks)
    }

    if (length(preds_list) == 0L)
      stop("All learners failed on all folds. Cannot build ensemble.")

    y_sub <- structure(subset_y(y, all_idx), indices = all_idx)

    fitted_metalearners <- lapply(metalearners, function(mtl)
      fit(mtl, x = preds_list, y = y_sub)
    )

    rm(fold_data, preds_list, y_sub)

    structure(
      fitted_metalearners,
      indices         = all_idx,
      failed_learners = if (length(failed_learners) > 0L) failed_learners else NULL
    )

  }, future.packages = c(future_pkgs, "nanatools"),
  future.globals  = list(
    x               = x,
    y               = y,
    learners        = learners,
    metalearners    = metalearners,
    combine_preds   = combine_preds,
    subset_y        = subset_y,
    should_splice   = should_splice,
    splice_classes  = splice_classes,
    get_lrn_display_name = get_lrn_display_name
  ),
  future.seed = TRUE)
}
