
# fit() and predict() generics for metalearners

#' @export
fit.SL_Metalearner <- function(object, y, preds_list, loss_fun_list, ...) {
  copy_structure <- object
  copy_structure$ensemble <- object$fit(y, preds_list, loss_fun_list)
  class(copy_structure) <- c("SL_Metalearner_Fitted", "SL_Metalearner")
  return(copy_structure)
}

#' @export
predict.SL_Metalearner <- function(object, newpreds, ...) {

  stop("Please train this learner before using it for predictions.")

}

#' @export
predict.SL_Metalearner_Fitted <- function(object, newpreds, ...) {

  object$preds(object$ensemble, newpreds)
}

# Also printing!
#' @export
print.SL_Metalearner <- function(object, ...) {

  message(cat("Metalearner with name ", object$name, ".\nNot yet fitted.", sep = ""))

}

#' @export
print.SL_Metalearner_Fitted <- function(object, ...) {

  message(cat("Metalearner with name ", object$name, ".\nHas been fitted.", sep = ""))
}

# Template to define individual learners as metalearners
# Not exported; just used internally
#' @export
mtl_learner <- function(name, index_pos) {
  get_list <- list(
    name = name,
    fit = function(y, preds_list, loss_fun_list) {
      # Just return the relevant index
      return(index_pos)
    },
    preds = function(object, preds) {
      # Then output this index
      return(as.vector(preds[[object]]))
    }
  )
  class(get_list) <- "SL_Metalearner"
  return(get_list)
}


# Template for selector
#' @export
mtl_selector <- function(name) {

  get_list <- list(
    name = name,
    fit = function(y, preds_list, loss_fun_list) {

      all_losses <- sapply(preds_list, function(x) loss_fun_list$loss_fun(y = y, preds = x))

      best <- which.min(sapply(all_losses, function(x) x))

      weights <- rep(0, length(preds_list))

      weights[[best]] <- 1

      names(weights) <- names(preds_list)

      return(weights)

    },
    preds = function(object, preds) {

      # Simply sapply on the list and then weight
      preds_mat <- do.call("cbind", preds)

      # Then output
      return(as.vector(preds_mat %*% object))

    }
  )

  class(get_list) <- "SL_Metalearner"

  return(get_list)

}


# Template for non-negative log-likelihood
#' @export
#' @import nloptr
mtl_NNLogLik <- function(name) {

  get_list <- list(
    name = name,
    fit = function(y, preds_list, loss_fun_list) {

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

    },
    preds = function(object, preds) {

      # Simply sapply on the list and then weight
      preds_mat <- do.call("cbind", preds)

      # Then output
      return(as.vector(preds_mat %*% object))

    }
  )

  class(get_list) <- "SL_Metalearner"

  return(get_list)

}

# Template for nnls
#' @export
#' @import nnls
mtl_nnls <- function(name, normalize = FALSE) {

  get_list <- list(
    name = name,
    fit = function(y, preds_list, loss_fun_list) {

      preds_mat <- do.call("cbind", preds_list)
      get_coefs <- nnls::nnls(preds_mat, y)

      coef_vec <- get_coefs$x
      names(coef_vec) <- colnames(preds_mat)

      if (normalize == TRUE) {
        coef_vec <- coef_vec / sum(coef_vec)
      }

      return(coef_vec)

    },
    preds = function(object, preds) {

      # Simply sapply on the list and then weight
      preds_mat <- do.call("cbind", preds)

      # Then output
      return(as.vector(preds_mat %*% object))

    }
  )

  class(get_list) <- "SL_Metalearner"

  return(get_list)

}
