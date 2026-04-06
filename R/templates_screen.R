
#' @export
scr_correlation <- function(name, cutoff = 0.1, abs = TRUE) {
  make_learner_factory(
    fit = function(x, y) {
      get_cors <- apply(x, 2, function(clmn) cor(clmn, y))
      if (abs) get_cors <- abs(get_cors)

      return(which(get_cors > cutoff))

    }, preds = function(object, data) {
      return(data[,object])
    },
    cutoff,
    abs,
    is_metalearner = FALSE
  )(name = name, cutoff = cutoff, abs = abs)
}

#' @export
scr_lasso <- function(name, alpha) {
  return(NULL)
}

#' @export
scr_ranger <- function(name, n_vars) {
  return(NULL)
}

#' @export
scr_svd <- function(name, n_vars) {
  return(NULL)
}
