

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

  message(cat("Learner object with name", object$name, ".\nNot yet fitted."))

}

#' @export
print.SL_Learner_Fitted <- function(object, ...) {

  message(cat("Learner object with name", object$name, ".\nHas been fitted."))
}


# Write a constructor for valid learner objects
# Still big work in progress
lrn_custom <- function(name, fit, preds) {

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}

# Template for empirical mean
lrn_mean <- function(name) {

  force(name)

  get_list <- list(
    name = name,
    fit = function(x, y) mean(y),
    preds = function(object, data) rep(object, nrow(data))
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}

# Template for GLM
lrn_glm <- function(name, family) {

  force(name)
  force(family)

  get_list <- list(
    name = name,
    fit = function(x, y) {

      fd <- data.frame(y = y, x)
      glm(y ~ ., family = family, data = fd)

    },
    preds = function(object, data) {

      if (!is.data.frame(data)) data <- data.frame(data)
      predict(object, newdata = data, type = "response")

    }
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}

