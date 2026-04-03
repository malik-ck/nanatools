


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
lrn_custom <- function(fit, preds, ...) {

  # 1. Capture the dots as raw expressions
  raw_dots <- substitute(list(...))[-1]

  # Build formal arguments for the constructor
  constr_args <- pairlist(name = quote(expr = ))

  # Loop through dots
  for (i in seq_along(raw_dots)) {
    arg_name <- names(raw_dots)[i]

    if (is.null(arg_name) || arg_name == "") {
      # It's a required argument (no default provided)
      actual_name <- as.character(raw_dots[[i]])
      constr_args[actual_name] <- list(quote(expr = ))
    } else {
      # It's an argument with a default
      constr_args[arg_name] <- list(raw_dots[[i]])
    }
  }

  # Define the constructor shell
  constructor <- function() {}

  # Assign arguments here in a new environment to prevent bloat
  formals(constructor) <- constr_args
  body(constructor) <- bquote({
    current_env <- environment()
    invisible(as.list(current_env))
    parent.env(current_env) <- globalenv()

    wrapped_fit <- function(x, y) {
      p <- as.list(current_env)
      p$name <- NULL
      do.call(.(fit), c(list(x = x, y = y), p))
    }

    wrapped_preds <- function(object, data) {
      p <- as.list(current_env)
      p$name <- NULL
      do.call(.(preds), c(list(object = object, data = data), p))
    }

    # Return the lean SL_Learner object
    structure(
      list(
        name = name,
        fit = wrapped_fit,
        preds = wrapped_preds
      ),
      class = "SL_Learner"
    )
  })

  return(constructor)
}

get_params <- function(learner) {
  # Get the environment
  env <- environment(learner$fit)
  params <- as.list(env)

  # Strip out some additionals
  exclude_vars <- c("wrapped_fit", "wrapped_preds", "current_env", "name")
  params[exclude_vars] <- NULL

  return(params)
}

# A robust helper to find the original function
extract_original_func <- function(func) {
  code <- as.list(body(func))

  # Find which line is do.call
  is_docall <- sapply(code, function(x) {
    is.call(x) && x[[1]] == quote(do.call)
  })

  docall_line <- code[[which(is_docall)]]
  return(docall_line[[2]])
}

get_original_fit <- function(learner) {
  extract_original_func(learner$fit)
}

get_original_preds <- function(learner) {
  extract_original_func(learner$preds)
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
