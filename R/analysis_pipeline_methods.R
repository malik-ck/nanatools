
#' @export
summary.FWB_TMLE <- function(object, ...) {

  object$results

}

#' @export
print.FWB_TMLE <- function(object, ...) {

  result_vec <- object$results[,2]
  names(result_vec) <- object$results[,1]

  cat("Point estimates:\n\n")

  print(result_vec)

  cat("\nBased on ", object$bootstrap_samples, " fractional weighted bootstrap samples\n\n", sep = "")

}



#' @export
summary.FWB_TMLE_MI <- function(object, ...) {

  object$results

}

#' @export
print.FWB_TMLE_MI <- function(object, ...) {

  result_vec <- object$results[,2]
  names(result_vec) <- object$results[,1]

  cat("Point estimates:\n\n")

  print(result_vec)

  cat("\nBased on ", object$objects[[1]]$bootstrap_samples, " fractional weighted bootstrap samples per imputed data set\n", sep = "")
  cat("With a total of ", length(object$objects), " imputed data sets\n\n", sep = "")

}
