
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

