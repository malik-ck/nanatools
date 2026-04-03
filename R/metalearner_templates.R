
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


#' A metalearner that selects the best individual learner.
#'
#' @param name Name of the metalearner.
#' @param reduce If \code{TRUE}, collapses the prediction list into one matrix or data frame.
#' Useful if y is a vector and the loss function evaluates vectors. Set to FALSE for multinomial outcomes and losses.
#' @return A list of class \code{SL_Metalearner_Fitted} with the fitted metalearner.
#' @export
mtl_selector <- function(name, reduce = TRUE) {
  get_list <- list(
    name = name,
    fit = function(y, preds_list, loss_fun_list) {
      if (!reduce) {
        all_losses <- vapply(preds_list, function(preds) {
          mean(loss_fun_list$loss_fun(y = y, y_hat = preds))
        }, numeric(1L))

        weights        <- rep(0, length(all_losses))
        weights[which.min(all_losses)] <- 1
        names(weights) <- names(preds_list)
        return(weights)
      }

      if (reduce) {
        # Unpack the list and bind into a matrix or data.frame
        preds_mat <- do.call(cbind, preds_list)

        # Evaluate loss function on the whole matrix (assuming y is length n!)
        loss_matrix <- loss_fun_list$loss_fun(y = y, y_hat = preds_mat)

        # Take the column-wise mean to get the average loss per learner
        if (is.matrix(loss_matrix) || is.data.frame(loss_matrix)) {
          all_losses <- colMeans(loss_matrix)
        } else {
          # Fallback in case the loss function output requires apply
          all_losses <- apply(as.matrix(loss_matrix), 2, mean)
        }

        weights <- rep(0, length(all_losses))
        weights[which.min(all_losses)] <- 1
        names(weights) <- colnames(preds_mat)
        return(weights)
      }

      stop("For the selector, 'reduce' needs to be TRUE or FALSE.")
    },

    preds = function(object, preds) {
      selected_name <- names(which(object == 1))

      if (!reduce) {
        return(preds[[selected_name]])
      }

      if (reduce) {
        # Coerce the prediction list to a matrix/data.frame just like in 'fit'
        preds_mat <- do.call(cbind, preds)

        # Subset the matrix by the winning learner's name.
        # drop = FALSE ensures the result stays a matrix/df and doesn't collapse to a flat vector
        return(preds_mat[, selected_name, drop = FALSE])
      }

      stop("For the selector, 'reduce' needs to be TRUE or FALSE.")
    }
  )
  class(get_list) <- "SL_Metalearner"
  get_list
}


# Superlearner for arbitrary loss functions via Frank-Wolfe optimization
#' @export
mtl_superlearner <- function(name, max_iter = 1000, tol = 1e-7) {
  make_mtl <- list(
    name = name,
    fit = function(y, preds_list, loss_fun_list) {

      # Argument filterer for evaluating losses n stuff
      filter_args <- function(fun, args) {
        fmls <- names(formals(fun))
        if ("..." %in% fmls) return(args)
        args[names(args) %in% fmls]
      }

      # Flatten into fat matrix
      col_names <- names(preds_list)
      P_list <- lapply(preds_list, as.matrix)
      P <- do.call(cbind, P_list)

      # Map learner to column
      col_to_learner <- rep(col_names, sapply(P_list, ncol))

      K_total <- ncol(P)
      n <- nrow(P)

      # Initialize via best learner
      eval_loss <- function(y_true, y_est) {
        f <- loss_fun_list$loss_fun
        args <- c(list(y = y_true, y_hat = y_est), loss_fun_list$extra_params)
        do.call(f, filter_args(f, args))
      }

      initial_losses <- colMeans(apply(P, 2, function(p_col) eval_loss(y, p_col)))
      best_idx <- which.min(initial_losses)

      w <- rep(0, K_total)
      w[best_idx] <- 1
      y_hat <- P[, best_idx]

      # Frank-Wolfe things
      for (t in 1:max_iter) {
        # Calculate residual gradient wrt the current ensemble mean
        grad_args <- c(list(y = y, y_hat = y_hat), loss_fun_list$extra_params)
        res_grad <- do.call(loss_fun_list$grad_fun, filter_args(loss_fun_list$grad_fun, grad_args))

        # Find best column
        all_grads <- colMeans(res_grad * P)
        idx_min <- which.min(all_grads)

        fw_gap <- sum(w * all_grads) - all_grads[idx_min]
        if (fw_gap < tol) break

        # Line Search
        d_y <- P[, idx_min] - y_hat
        opt_res <- optimize(f = function(g) mean(eval_loss(y, y_hat + g * d_y)),
                            interval = c(0, 1))
        gamma <- opt_res$minimum

        # Update
        w <- (1 - gamma) * w
        w[idx_min] <- w[idx_min] + gamma
        y_hat <- y_hat + gamma * d_y
      }

      # Unflatten
      names(w) <- make.unique(col_to_learner, sep = "_")

      # Store the 'map' so the predict function knows how to apply weights
      attr(w, "column_map") <- col_to_learner
      return(w)
    },

    preds = function(object, preds) {
      # object is the weight vector with the 'column_map' attribute
      P <- do.call(cbind, lapply(preds, as.matrix))
      return(as.vector(P %*% object))
    }
  )

  class(make_mtl) <- "SL_Metalearner"
  return(make_mtl)
}


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
