library(Rfast)

sim_data <- function(n = 200, p = 1000, type = "normal") {

  x <- matrix(rnorm(n * p), ncol = p)
  x <- apply(x, 2, function(x) x + rnorm(1))
  betas <- rexp(p, 10) * sample(c(-0.9, 1.1), p, replace = TRUE)

  if (type == "normal") y <- rnorm(n, 0.3 + (x %*% betas))
  if (type == "binomial") y <- rbinom(n, 1, plogis(0.3 + (x %*% betas)))

  return(data.frame(y, x))

}

loss_list <- list(

  gaussian = list(
    link = function(mu) mu,
    inv_link = function(eta) eta,
    gradient = function(y, f) f - y,
    hessian  = function(y, f) rep(1, length(y))  # constant
  ),

  binomial = list(
    link = function(mu) log(mu / (1 - mu)),
    inv_link = function(eta) 1 / (1 + exp(-eta)),
    gradient = function(y, f) plogis(f) - y,
    hessian  = function(y, f) plogis(f) * (1 - plogis(f))
  ),

  poisson = list(
    link = function(mu) log(mu),
    inv_link = function(eta) exp(eta),
    gradient = function(y, f) exp(f) - y,
    hessian  = function(y, f) exp(f)
  )
)

# Now some test data
full_data <- sim_data(type = "binomial")
x <- as.matrix(full_data[,-which(colnames(full_data) == "y")])
current_gradient <- loss_list$binomial$gradient(full_data$y, rep(qlogis(mean(full_data$y)), nrow(x)))
current_hessian <- loss_list$binomial$hessian(full_data$y, rep(qlogis(mean(full_data$y)), nrow(x)))

# Some additional parameters, alpha being L1-penalty and gamma L0-penalty
n <- nrow(x)
alpha <- 0.01
gamma <- 0.01
max_interaction <- 3
max_children <- 3
subsample_row <- 1
subsample_column <- 1
omit_zeroes <- TRUE


do_one_iteration <- function(x,
                             current_gradient, current_hessian,
                             alpha, gamma,
                             max_interaction, max_children,
                             subsample_col, subsample_row,
                             omit_zeroes) {

  # Initialize iteration with an intercept
  candidates <- matrix(rep(1, nrow(x)), ncol = 1)

  # Track candidates that are still admissible
  admissible_candidates <- c(1)

  # Now iterate
  # Later this should be while (TRUE) with extensive stopping

  for (i in 1:200) {

    # For each admissible candidate, pick the loss minimizer after multiplying by the intercept
    for (k in 1:length(admissible_candidates)) {

      # Calculate working outcome
      working_y <- - (current_gradient / current_hessian)

      # Get current candidate
      cur_cand <- candidates[,admissible_candidates[[k]]]

      # Get sum of Hessians (for centering)
      sum_hess <- sum(current_hessian)

      # Easiest to pre-calculate weight-centered and candidate-multiplied matrix
      wc_m <- Rfast::eachrow(x * cur_cand,
                             drop((crossprod(x, current_hessian * cur_cand) / sum_hess)),
                             "-")

      # Do the same for working outcome
      wc_h <- working_y - (sum(working_y * current_hessian) / sum_hess)

      # Now do the weighted least squares calculation across all columns for the current admissible candidate
      # Do this to identify the minimum across columns

      get_minimizer <- working_y - ((crossprod(wc_m, cur_cand * current_hessian) * colSums(wc_m)) / (colSums(wc_m^2)))

      part_1 <- crossprod(wc_m, cur_cand * current_hessian * working_y)
      part_2 <- crossprod(wc_m^2, cur_cand^2 * current_hessian)


    }

  }



}

