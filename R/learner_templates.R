

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


# Write a constructor for valid learner objects
# Still big work in progress
#' @export
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
#' @export
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
#' @export
lrn_glm <- function(name, family) {

  if (missing(family)) stop("Please explicitly specify a family object for glm.")

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


# Templates for GAMs
#' @export
#' @import mgcv
lrn_gam <- function(name, family, k = 10, method = "GCV.Cp", formula = NULL, smoother = "tp") {

  if (missing(family)) stop("Please explicitly specify a family object for glm.")

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(formula)
  force(family)
  force(method)
  force(smoother)

  fit <- function(x, y) {

    # Need a data frame for GAMs
    fd <- data.frame(y = y, x)

    # Now need to construct formula
    # If provided, just use provided formula
    if (!is.null(formula)) {

      use_frm <- formula

    } else {

      # Get numeric columns with sufficient length
      filter_numeric <- which(unlist(lapply(fd[,-1], function(x) is.numeric(x) & length(unique(x)) > 2)))

      # For those of sufficient length, get the number of unique values (capped at k)
      unique_vals <- unlist(lapply(fd[,(filter_numeric + 1)], function(x) ifelse(length(unique(x)) <= k, length(unique(x)), k)))

      # Now can construct formula
      numeric_part <- paste0(
        "s(", colnames(fd[,(filter_numeric + 1)]), ", k = ", unique_vals, ", bs = \"", smoother, "\") + ",
        collapse = ""
      )

      indicator_part <- paste0(
        colnames(fd[,-c(1, filter_numeric + 1)]), " + ", collapse = ""
      )

      # Combine all and remove last two of string (since that is an overhang +)
      use_frm <- paste("y ~ ", numeric_part, indicator_part, collapse = "")
      use_frm <- formula(substr(use_frm, 1, nchar(use_frm) - 3))

    }

    # Can now fit!
    return(
      mgcv::gam(use_frm, family = family, data = fd, method = method)
    )

  }

  preds <- function(object, data) {

    if (!is.data.frame(data)) data <- data.frame(data)
    predict(object, newdata = data, type = "response")

  }

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}


# Templates for mboost
#' @export
#' @import mboost
lrn_mboost <- function(name, family, mstop = 100, nu = 0.1, formula = NULL, lambda = 0.01, knots = 20) {

  if (missing(family)) stop("Please explicitly specify a family object for glm.")

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(family)
  force(formula)
  force(lambda)
  force(knots)

  fit <- function(x, y) {

    # Need a data frame for GAMs
    fd <- data.frame(y = y, intr = 1, x)

    # Now need to construct formula
    # If provided, just use provided formula
    if (!is.null(formula)) {

      use_frm <- formula

    } else {

      # Get numeric columns with sufficient length
      filter_numeric <- which(unlist(lapply(fd[,-1], function(x) is.numeric(x) & length(unique(x)) > 2)))

      # For those of sufficient length, get the number of unique values (capped at k)
      unique_vals <- unlist(lapply(fd[,(filter_numeric + 1)], function(x) ifelse(length(unique(x)) <= knots + 2, length(unique(x)), knots + 2)))

      # Now can construct formula
      numeric_part <- paste0(
        "bbs(", colnames(fd[,(filter_numeric + 1)]), ", knots = ", unique_vals - 2, ", center = TRUE, lambda = ", lambda, ") + ",
        collapse = ""
      )

      indicator_part <- paste0(
        "bols(", colnames(fd[,-1]), ", intercept = FALSE, lambda = ", lambda, ") + ", collapse = ""
      )

      # Combine all and remove last two of string (since that is an overhang +)
      use_frm <- paste("y ~ ", numeric_part, indicator_part, collapse = "")
      use_frm <- formula(substr(use_frm, 1, nchar(use_frm) - 3))

    }

    # Can now fit!
    return(
      mboost::mboost(use_frm, data = fd, family = family, control = boost_control(mstop = mstop, nu = nu))
    )

  }

  preds <- function(object, data) {

    if (!is.data.frame(data)) data <- data.frame(data)
    predict(object, newdata = data, type = "response")

  }

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}
