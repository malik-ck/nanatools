
#==============================================================================
# For learners
#==============================================================================

# fit() and predict() generics for individual learners

#' @export
fit <- function(object, ...) {
  UseMethod("fit")
}

#' @export
fit.SL_Learner <- function(object, x, y, ...) {
  copy_structure <- object
  copy_structure$model <- object$fit(x, y)
  class(copy_structure) <- c("SL_Learner_Fitted", "SL_Learner")
  return(copy_structure)
}

#' @export
predict.SL_Learner <- function(object, newdata, ...) {

  stop("Please train this learner before using it for predictions.")

}

#' @export
predict.SL_Learner_Fitted <- function(object, newdata, ...) {

  object$preds(object$model, newdata)
}

# Also printing!
#' @export
print.SL_Learner <- function(object, ...) {

  message(cat("Learner object with name ", object$name, ".\nNot yet fitted.", sep = ""))

}

#' @export
print.SL_Learner_Fitted <- function(object, ...) {

  message(cat("Learner object with name ", object$name, ".\nHas been fitted.", sep = ""))
}

#' Create a learner factory
#'
#' @param fit A function taking `x` and `y` that returns a fitted model.
#' @param preds A function taking `object` and `data` that returns predictions.
#' @param ... Hyperparameters. Pass as `name = value` for defaults, or just `name` for required arguments.
#' @export
make_learner_factory <- function(fit, preds, ..., is_metalearner) {
  factory_ns <- parent.env(environment())
  raw_dots <- substitute(list(...))[-1]

  # Check whether any names are bad
  bad_names <- c("name", "fit", "preds")
  clashing <- intersect(names(raw_dots), bad_names)
  if (length(clashing) > 0)
    stop("It is not allowed to pass arguments to 'make_learner_factory' ",
         "named 'name', 'fit', or 'preds'. Clashing: ",
         paste(clashing, collapse = ", "))

  # Check that fit and preds only have as arguments the two arguments they need
  extra_fit_args <- setdiff(names(formals(fit)), c("x", "y"))
  extra_preds_args <- setdiff(names(formals(preds)), c("object", "data"))
  if (length(c(extra_fit_args, extra_preds_args)) > 0)
    stop("fit and preds must only have (x, y) and (object, data) as arguments, ",
         "respectively.\nEvery argument you pass into make_learner_factory() ",
         "is available inside fit() and predict() regardless.", call. = FALSE)

  # Build formals as a plain list using alist() for missing-value sentinels
  constr_args <- alist(name = )   # name is always required

  for (i in seq_along(raw_dots)) {
    arg_name <- names(raw_dots)[[i]]
    if (is.null(arg_name) || arg_name == "") {
      actual_name        <- as.character(raw_dots[[i]])
      new_arg            <- alist(x = )
      names(new_arg)     <- actual_name
      constr_args        <- c(constr_args, new_arg)
    } else {
      constr_args[arg_name] <- list(raw_dots[[i]])  # single bracket + list()
    }
  }

  constructor          <- function() {}
  hyperparam_names     <- setdiff(names(constr_args), c("name", "is_metalearner"))
  formals(constructor) <- constr_args

  body(constructor) <- bquote({
    p <- mget(.(hyperparam_names), envir = environment())

    # Minimal environment: only fit, preds, p, nothing else
    closure_env <- list2env(p, parent = .(factory_ns))
    environment(fit)  <- closure_env
    environment(preds) <- closure_env
    closure_env$fit <- fit
    closure_env$preds <- preds

    wrapped_fit <- function(x, y) fit(x, y)
    environment(wrapped_fit) <- closure_env

    wrapped_preds <- function(object, data) preds(object, data)
    environment(wrapped_preds) <- closure_env

    if (is_metalearner) assign_class <- c("SL_Metalearner", "SL_Learner") else
      assign_class <- "SL_Learner"

    structure(
      list(name = name, fit = wrapped_fit, preds = wrapped_preds),
      class = assign_class
    )
  })

  constructor
}

get_params <- function(learner) {
  env <- environment(learner$fit)
  reserved <- c("fit", "preds")
  keys <- setdiff(ls(env), reserved)
  mget(keys, envir = env)
}

get_original_fit <- function(learner) {
  environment(learner$fit)$fit
}

get_original_preds <- function(learner) {
  environment(learner$fit)$preds
}

#' @export
inspect <- function(learner, ...) {
  UseMethod("inspect")
}

#' Investigate an \code{SL_Learner}
#'
#' @param learner an object of class \code{SL_Learner} to inspect.
#' @returns Invisibly returns a list containing the original \code{fit} and \code{predict} functions.
#' @export
inspect.SL_Learner <- function(learner) {
  cat("Learner Name: ", learner$name, "\n")
  cat("============================\n\n")

  # 1. Show Hyperparameters (using our environment trick)
  params <- get_params(learner)
  cat("--- Additional arguments ---\n")
  if(length(params) > 0) {
    print(params)
  } else {
    cat("None\n")
  }

  # 2. Extract the original functions
  # Note: I updated the indices to match the 'snapped' constructor body
  orig_fit <- get_original_fit(learner)
  orig_preds <- get_original_preds(learner)

  cat("\n--- Original Fit Logic ---\n")
  print(orig_fit)

  cat("\n--- Original Predict Logic ---\n")
  print(orig_preds)

  # Return them in a named list so the user can "intercept" them
  invisible(list(
    fit = orig_fit,
    preds = orig_preds,
    params = params
  ))
}

