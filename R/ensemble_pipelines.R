

# Learner lister
# Not for export, only internal
make_learner_list <- function(...) {
  dots <- list(...)
  is_learner <- function(x) inherits(x, "SL_Learner") || inherits(x, "SL_Metalearner")

  res <- lapply(dots, function(x) {
    if (is_learner(x)) {
      return(list(x)) # Pack into a list
    } else {
      return(x) # Is already a list
    }
  })

  # Flatten one level to get a single list of learners, check that all are now individual learners
  collected_learners <- do.call(c, res)

  if (!all(unlist(lapply(collected_learners, function(x) inherits(x, "SL_Learner") || inherits(x, "SL_Metalearner"))))) {
    stop("'collect_learners' can only handle individual learners of class 'SL_Leaerner' or 'SL_Metalearner' and lists of them.")
  }
  collected_learners
}

#' Make a pipeline for use in ensembles.
#'
#' @param ... Learners or lists of learners in the order they are to be executed.
#' See \code{details} to understand how to set up a valid piepline.
#' @return An object of class \code{SL_Pipeline} to hand into \code{collect_learners}
#' @details
#' \code{make_pipeline} admits both individual learners (class \code{SL_Learner}), and lists of
#' learners. However, it is imperative that the first argument is an \code{SL_Learner}.
#' The pipeline starts by training it and handing its predictions into the learner or learners
#' in the next argument. The output of training on a pipeline are the learner(s) specified in its last argument.
#' Note that, if the next argument is a list of learners, all current learners are handed into each next learner, so the output grows quickly without care.
#' Pipeline objects can be handed into \code{collect_learners}. Multiple pipelines can be specified,
#' also alongside individual \code{SL_Learner} objects.
#'
#' @export
make_pipeline <- function(...) {
  return(NULL)
}

#' Collect individual learners and pipelines in a \code{train_ensemble}-friendly format.
#'
#' @param ... Individual learners and pipelines to train. Lists of learners can be specified
#' (say outputs of \code{lrn_grid}) and are flattened into individual learners.
#' @return An object of class \code{SL_Learner_Collection} to hand into \code{setup_ensemble}
#'
#' @export
collect_learners <- function(...) {
  return(NULL)
}

### ALSO: fit() and predict() methods for pipelines. Should lead to somewhat-seamless integration into the current architecture then.
#' @export
fit.SL_Pipeline <- function(object, x, y, ...) {

}

#' @export
predict.SL_Pipeline <- function(object, x, y, ...) {

}
