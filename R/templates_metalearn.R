
#' A metalearner that selects the best individual learner.
#'
#' @param name Name of the metalearner.
#' @param reduce If \code{TRUE}, collapses the prediction list into one matrix or data frame.
#' Useful if y is a vector, and a learner might output multiple columns. Set to FALSE for multinomial outcomes and losses.
#' @return A list of class \code{SL_Metalearner_Fitted} with the fitted metalearner.
#' @export
mtl_selector <- function(name, loss_fun = loss_gaussian(), reduce = TRUE) {

  make_learner_factory(
    fit = function(x, y) {
      if (!reduce) {
        all_losses <- vapply(x, function(preds) {
          mean(loss_fun$loss_fun(y = y, y_hat = preds))
        }, numeric(1L))

        weights <- rep(0, length(all_losses))
        weights[which.min(all_losses)] <- 1
        names(weights) <- names(preds_list)
        return(weights)
      }

      if (reduce) {
        # Unpack the list and bind into a matrix or data.frame
        preds_mat <- do.call(cbind, x)

        # Evaluate loss function on the whole matrix (assuming y is length n!)
        loss_matrix <- loss_fun$loss_fun(y = y, y_hat = preds_mat)

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
    preds = function(object, data) {
      selected_name <- names(which(object == 1))

      if (!reduce) {
        return(data[[selected_name]])
      }

      if (reduce) {
        # Coerce the prediction list to a matrix/data.frame just like in 'fit'
        preds_mat <- do.call(cbind, data)

        # Subset the matrix by the winning learner's name.
        # drop = FALSE ensures the result stays a matrix/df and doesn't collapse to a flat vector
        return(preds_mat[, selected_name, drop = FALSE])
      }
    },
    reduce = TRUE,
    loss_fun = loss_gaussian(),
    is_metalearner = TRUE
  )(name = name, loss_fun = loss_fun, reduce = reduce)
}


# Superlearner for arbitrary loss functions via Frank-Wolfe optimization
#' @export
mtl_superlearner <- function(name, loss_fun = loss_gaussian(), max_iter = 1000, tol = 1e-7) {

  make_learner_factory(
    fit = function(x, y) {

      # Flatten into fat matrix
      col_names <- names(x)
      P_list <- lapply(x, as.matrix)
      P <- do.call(cbind, P_list)

      # Map learner to column
      col_to_learner <- rep(col_names, sapply(P_list, ncol))

      K_total <- ncol(P)
      n <- nrow(P)

      # Initialize via best learner
      initial_losses <- colMeans(apply(P, 2, function(p_col) loss_fun$loss_fun(y, p_col)))
      best_idx <- which.min(initial_losses)

      w <- rep(0, K_total)
      w[best_idx] <- 1
      y_hat <- P[, best_idx]

      # Frank-Wolfe things
      for (t in 1:max_iter) {
        # Calculate residual gradient wrt the current ensemble mean
        res_grad <- loss_fun$grad_fun(y = y, y_hat = y_hat)

        # Find best column
        all_grads <- colMeans(res_grad * P)
        idx_min <- which.min(all_grads)

        fw_gap <- sum(w * all_grads) - all_grads[idx_min]
        if (fw_gap < tol) break

        # Line Search
        d_y <- P[, idx_min] - y_hat
        opt_res <- optimize(f = function(g) mean(loss_fun$loss_fun(y, y_hat + g * d_y)),
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
    preds = function(object, data) {
      # object is the weight vector with the 'column_map' attribute
      P <- do.call(cbind, lapply(data, as.matrix))
      return(as.vector(P %*% object))
    },
    is_metalearner = TRUE,
    loss_fun = gaussian(), max_iter = 1000, tol = 1e-7
  )(name = name, loss_fun = loss_fun, max_iter = max_iter, tol = tol)
}


mtl_nnls <- function(name, normalize = FALSE) {

  make_learner_factory(
    fit = function(x, y) {

      preds_mat <- do.call("cbind", x)
      get_coefs <- nnls::nnls(x, y)

      coef_vec <- get_coefs$x
      names(coef_vec) <- colnames(preds_mat)

      if (normalize == TRUE) coef_vec <- coef_vec / sum(coef_vec)

      return(coef_vec)
    },
    preds = function(object, data) {
      preds_mat <- do.call("cbind", preds)
      return(as.vector(preds_mat %*% object))
    },
    is_metalearner = TRUE,
    normalize = TRUE
  )(name = name, normalize = normalize)
}


# Template to define individual learners as metalearners
# Not exported; just used internally
mtl_learner <- function(name, index_pos) {
  make_learner_factory(
    fit = function(x, y) return(index_pos),
    preds = function(object, data) return(as.vector(data[[object]])),
    index_pos,
    is_metalearner = TRUE
  )(name = name, index_pos = index_pos)
}
