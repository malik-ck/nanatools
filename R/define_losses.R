
#' Create a Loss Object for Metalearning
#' @param loss A function(y, y_hat) returning a vector of losses.
#' @param gradient An optional function(y, y_hat) evaluating the gradient of the loss. Required for some of the pre-implemented metalearners.
#' @param ... Additional elements (e.g., Hessian, family metadata) required by custom learners defined by users.
#' @export
loss_custom <- function(loss, gradient = NULL, ...) {
  stopifnot(is.function(loss))
  out <- list(
    loss_fun = loss,
    grad_fun = gradient,
    extra_params = list(...)
  )
  class(out) <- "mtl_loss"
  out
}

# A loss computation generic
#' @export
compute_loss <- function(obj, ...) UseMethod("compute_loss")

#' @export
compute_loss.mtl_loss <- function(obj, y, y_hat, ...) {
  do.call(obj$loss_fun, c(list(y = y, y_hat = y_hat), obj$extra_params))
}

# A little printer because why not
#' @export
print.mtl_loss <- function(obj, ...) {
  cat("<mtl_loss>\n")
  cat("  extra_params:",
      if (length(obj$extra_params)) paste(names(obj$extra_params), collapse = ", ")
      else "none", "\n")
  invisible(obj)
}

# Gaussian loss
#' @export
loss_gaussian <- function() {
  loss_custom(
    loss = function(y, y_hat) (y - y_hat)^2,
    gradient = function(y, y_hat) -2 * (y - y_hat),
    family = "gaussian"
  )
}

# Logistic loss
#' @export
loss_logistic <- function() {
  loss_custom(
    loss = function(y, y_hat) {
      # Add small epsilon to prevent log(0)
      eps <- 1e-15
      y_hat <- pmax(pmin(y_hat, 1 - eps), eps)
      -(y * log(y_hat) + (1 - y) * log(1 - y_hat))
    },
    gradient = function(y, y_hat) {
      eps <- 1e-15
      y_hat <- pmax(pmin(y_hat, 1 - eps), eps)
      -(y / y_hat - (1 - y) / (1 - y_hat))
    },
    family = "binomial"
  )
}

# Poisson loss
#' @export
loss_poisson <- function() {
  loss_custom(
    loss = function(y, y_hat) {
      y_hat <- pmax(y_hat, 1e-15)
      y_hat - y * log(y_hat)
    },
    gradient = function(y, y_hat) {
      y_hat <- pmax(y_hat, 1e-15)
      1 - y / y_hat
    },
    family = "poisson"
  )
}

# Gamma loss
#' @export
loss_gamma <- function() {
  loss_custom(
    loss = function(y, y_hat) {
      y_hat <- pmax(y_hat, 1e-15)
      y / y_hat + log(y_hat)
    },
    gradient = function(y, y_hat) {
      y_hat <- pmax(y_hat, 1e-15)
      -y / y_hat^2 + 1 / y_hat
    },
    family = "gamma"
  )
}
