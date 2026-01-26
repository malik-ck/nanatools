

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
lrn_glm <- function(name, family, offset = NULL) {

  if (missing(family)) stop("Please explicitly specify a family object for glm.")

  force(name)
  force(family)
  force(offset)

  get_list <- list(
    name = name,
    fit = function(x, y) {

      if (is.data.frame(x)) {
        instr_list <- create_instruction_list(x)
        x <- make_safe_matrix(x, instr_list)
      } else {
        instr_list <- "skip"
      }

      fd <- data.frame(y = y, x)

      # Formula, maybe with offset
      if (!is.null(offset)) ofs_char <- paste("offset(",offset , ")", sep = "") else ofs_char <- character(0)
      offset_col <- which(colnames(fd) == offset)
      get_frm <- paste0("y ~ ", paste(c(ofs_char, colnames(fd)[-c(1, offset_col)]), collapse = " + "))

      get_model <- stats::glm(get_frm, family = family, data = fd)

      # Return explicitly with instruction list if needed
      return(list(
        model = get_model,
        instructions = instr_list
      ))

    },
    preds = function(object, data) {

      # Apply instruction list if necessary
      if (!identical(object[["instructions"]], "skip")) {
        data <- make_safe_matrix(data, object[["instructions"]])
      }

      # Then need to coerce to data frame either way
      data <- data.frame(data)

      # Then output
      return(as.vector(predict(object[["model"]], newdata = data, type = "response")))

    }
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}


# Templates for GAMs
#' @export
#' @import mgcv
lrn_gam <- function(name, family, offset = NULL, k = 10, method = "GCV.Cp", frm = NULL, smoother = "tp") {

  if (missing(family)) stop("Please explicitly specify a family object for glm.")

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(frm)
  force(family)
  force(method)
  force(smoother)

  fit <- function(x, y) {

    # Make a safe matrix if necessary and no formula provided
    if (is.data.frame(x) & is.null(frm)) {
      instr_list <- create_instruction_list(x)
      x <- make_safe_matrix(x, instr_list)
    } else {
      instr_list <- "skip"
    }

    # Need a data frame for GAMs
    fd <- data.frame(y = y, x)

    # Now need to construct formula
    # If provided, just use provided formula
    if (!is.null(frm)) {

      use_frm <- frm

    } else {

      # Get numeric columns with sufficient length
      filter_numeric <- which(unlist(lapply(fd[,-1], function(x) is.numeric(x) & length(unique(x)) > 2)))

      # For those of sufficient length, get the number of unique values (capped at k)
      unique_vals <- unlist(lapply(fd[,(filter_numeric + 1), drop = FALSE], function(x) ifelse(length(unique(x)) <= k, length(unique(x)), k)))

      # Now can construct formula
      if (!is.null(offset)) offset_part <- paste("offset(",offset , ")", sep = "") else offset_part <- character(0)
      offset_col <- which(colnames(fd) == offset)

      numeric_part_vars <- colnames(fd)[(filter_numeric + 1)]
      indicator_part_vars <- colnames(fd[-c(1, filter_numeric + 1)])

      # If there is an offset, remove it from numeric and indicator parts
      if (!is.null(offset)) {
        if (any(numeric_part_vars == offset)) {
          unique_vals <- unique_vals[-which(numeric_part_vars == offset)]
          numeric_part_vars <- numeric_part_vars[-which(numeric_part_vars == offset)]
        }
        if (any(indicator_part_vars == offset)) indicator_part_vars <- indicator_part_vars[-which(indicator_part_vars == offset)]
      }

      numeric_part <- paste0(
        "s(", numeric_part_vars, ", k = ", unique_vals, ", bs = \"", smoother, "\")"
      )

      indicator_part <- paste0(
        indicator_part_vars
      )

      # Combine all and remove last two of string (since that is an overhang +)
      use_frm <- paste("y ~ ", paste0(c(offset_part, numeric_part, indicator_part), collapse = " + "), collapse = "")
      use_frm <- formula(use_frm)

    }

    # Can now fit!
    return(list(
      model = mgcv::gam(use_frm, family = family, data = fd, method = method),
      instructions = instr_list
    ))

  }

  preds <- function(object, data) {

    # Apply instruction list if necessary
    if (!identical(object[["instructions"]], "skip")) {
      data <- make_safe_matrix(data, object[["instructions"]])
    }

    # Then need to coerce to data frame either way
    data <- data.frame(data)

    # Then output
    return(as.vector(predict(object[["model"]], newdata = data, type = "response")))

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
lrn_mboost <- function(name, family, offset = NULL, mstop = 100, nu = 0.1, frm = NULL, max_df = 5, knots = 20, df_factor = 0.99) {

  if (missing(family)) stop("Please explicitly specify a family object for glm.")

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(family)
  force(offset)
  force(frm)
  force(mstop)
  force(nu)
  force(max_df)
  force(df_factor)
  force(knots)

  fit <- function(x, y) {

    # Create safe matrix if necessary
    if (is.data.frame(x) & is.null(frm)) {
      instr_list <- create_instruction_list(x)
      x <- make_safe_matrix(x, instr_list)
    } else {
      instr_list <- "skip"
    }

    # Need a data frame for Mboost
    fd <- data.frame(y = y, intr = 1, x)

    # Now need to construct formula
    # If provided, just use provided formula
    if (!is.null(frm)) {

      use_frm <- frm

    } else {

      # Get numeric columns with sufficient length
      filter_numeric <- which(unlist(lapply(fd[,-1, drop = FALSE], function(x) is.numeric(x) & length(unique(x)) > 2)))

      # Also get if too few unique values
      filter_tiny <- which(unlist(lapply(fd[,-1, drop = FALSE], function(x) length(unique(x)) == 1)))

      # For those of sufficient length, get the number of unique values (capped at k)
      get_n_knot <- unlist(lapply(fd[,(filter_numeric + 1), drop = FALSE], function(x) ifelse(length(unique(x)) <= knots + 2, length(unique(x)), knots + 2)))
      get_df <- unlist(lapply(fd[,(filter_numeric + 1), drop = FALSE], function(x) ifelse(length(unique(x)) <= max_df, length(unique(x)) - 1, max_df)))

      # Get variable names and remove offset if it exists
      numeric_part_vars <- colnames(fd)[(filter_numeric + 1)]
      indicator_part_vars <- colnames(fd)[-c(1, filter_tiny + 1)]

      if (!is.null(offset)) offset_part <- paste("offset(",offset , ")", sep = "") else offset_part <- character(0)
      offset_col <- which(colnames(fd) == offset)

      if (!is.null(offset)) {
        if (any(numeric_part_vars == offset)) {
          get_n_knot <- get_n_knot[-which(numeric_part_vars == offset)]
          get_df <- get_df[-which(numeric_part_vars == offset)]
          numeric_part_vars <- numeric_part_vars[-which(numeric_part_vars == offset)]
        }
        if (any(indicator_part_vars == offset)) indicator_part_vars <- indicator_part_vars[-which(indicator_part_vars == offset)]
      }

      # Now can construct formula
      numeric_part <- paste0(
        "bbs(", numeric_part_vars, ", df = ", get_df * df_factor, ", center = TRUE, knots = ", get_n_knot, ")"
      )

      indicator_part <- paste0(
        "bols(", indicator_part_vars, ", intercept = FALSE, df = ", df_factor, ")"
      )

      # Combine all and remove last two of string (since that is an overhang +)
      use_frm <- formula(paste("y ~ ", paste0(c(numeric_part, indicator_part), collapse = " + "), collapse = ""))

    }

    # Can now fit!
    return(list(
      model = mboost::mboost(use_frm, data = fd, family = family, control = boost_control(mstop = mstop, nu = nu)),
      instructions = instr_list
    ))

  }

  preds <- function(object, data) {

    # Apply instruction list if necessary
    if (!identical(object[["instructions"]], "skip")) {
      data <- make_safe_matrix(data, object[["instructions"]])
    }

    # Then need to coerce to data frame either way
    data <- data.frame(data)

    # Then output
    return(as.vector(predict(object[["model"]], newdata = data, type = "response")))

  }

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}


# Templates for hal9001
#' @export
#' @import hal9001
lrn_hal <- function(name, family, offset = NULL, frm = NULL, max_degree = 2,
                    smoothness_orders = 1, num_knots = 50) {

  if (missing(family)) stop("Please explicitly specify a family object for glmnet.")

  # Some checks ensuring there are integers where necessary
  integer_checker(max_degree, "the interaction degree.")
  integer_checker(smoothness_orders, "the smoothness.")

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(frm)
  force(offset)
  force(family)
  force(max_degree)
  force(smoothness_orders)

  fit <- function(x, y) {

    # If there is an offset, retrieve as variable and remove from data
    if (!is.null(offset)) {
      get_offset <- x[,offset]
      if (!is.numeric(get_offset)) stop("Called from lrn_cv_glmnet: please ensure that the offset you provide is numeric.")

      x <- x[,-which(colnames(x) == offset)]
    } else {
      get_offset <- NULL
    }

    # Make a safe matrix if necessary and no formula provided
    if (is.data.frame(x) & is.null(frm)) {

      instr_list <- create_instruction_list(x)
      x <- make_safe_matrix(x, instr_list)

    } else if (is.data.frame(x) & !is.null(frm)) { # If frm provided, use in model.matrix call

      instr_list <- list(coerce_df = FALSE, frm = frm)
      x <- model.matrix(frm, data = x)[,-1, drop = FALSE]

    } else if (is.matrix(x) & is.null(frm)) { # Simplest case: take x as is

      instr_list <- "skip"

    } else if (is.matrix(x) & !is.null(frm)) { # If formula provided for matrix, coerce to df and warn

      instr_list <- list(coerce_df = TRUE, frm = frm)
      x <- model.matrix(frm, data = data.frame(x))[,-1, drop = FALSE]

      warning("Formula provided for matrix in glmnet.\nCoerced to data frame for call to model.matrix(); can be (silently) very unsafe!")

    } else stop("Unexpected data input in glmnet! Should not happen.")

    # Normalize...
    flex_list <- list()

    get_sds <- apply(x, 2, sd)
    flex_list[["rescale"]] <- list(means = apply(x, 2, mean), sds = ifelse(get_sds == 0, 1, get_sds))
    x <- sweep(x, 2, flex_list[["rescale"]][["means"]], "-")
    x <- sweep(x, 2, flex_list[["rescale"]][["sds"]], "/")

    # Can now fit!
    get_model <- hal9001::fit_hal(x, y, family = family, offset = get_offset,
                                  max_degree = max_degree, smoothness_orders = smoothness_orders,
                                  num_knots = num_knots)

    return(list(
      model = get_model,
      instructions = instr_list,
      flexibility = flex_list
    ))

  }

  preds <- function(object, data) {

    # If there is an offset, retrieve as variable and remove from data
    if (!is.null(offset)) {
      get_offset <- data[,offset]
      if (!is.numeric(get_offset)) stop("Called from lrn_cv_glmnet: please ensure that the offset you provide is numeric.")

      data <- data[,-which(colnames(data) == offset)]
    } else {
      get_offset <- NULL
    }

    # Apply instruction list if necessary
    if (!(identical(object[["instructions"]], "skip"))) { # Only need logic if data not already right

      # If matrix, need to coerce to df again for model matrix call
      if (is.matrix(data)) {
        data <- model.matrix(object[["instructions"]][["frm"]], data = data.frame(data))[,-1, drop = FALSE]
      } else if (!is.null(object[["instructions"]][["frm"]])) {
        data <- model.matrix(object[["instructions"]][["frm"]], data = data)[,-1, drop = FALSE]
      } else { # Should be the last option here: df without formula
        data <- make_safe_matrix(data, object[["instructions"]])
      }

    }

    # Now rescale
    get_means <- object[["flexibility"]][["rescale"]][["means"]]
    get_sds <- object[["flexibility"]][["rescale"]][["sds"]]

    data <- sweep(data, 2, get_means, "-")
    data <- sweep(data, 2, get_sds, "/")


    # Can now output
    return(as.vector(predict(object[["model"]], new_data = data, type = "response", newoffset = get_offset)))

  }

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}


# Templates for CV Elastic Nets, optionally with added flexibility
### Could thin this down, extracting only the relevant parameter vector and lambda,
### rather than the entire object. Could save good memory in high-dimensional settings.
#' @export
#' @import glmnet
lrn_cv_glmnet <- function(name, family, offset = NULL, frm = NULL, alpha = 1, nfolds = 10, nlambda = 100,
                          type_lambda = "lambda.min", tpb_knots = NULL, create_interactions = NULL) {

  if (missing(family)) stop("Please explicitly specify a family object for glmnet.")

  # Some checks ensuring there are integers where necessary
  integer_checker(nfolds, "the number of folds.")
  integer_checker(nlambda, "nlambda")
  integer_checker(tpb_knots, "the number of spline knots.")
  integer_checker(create_interactions, "the interaction depth.")

  # Small type check for the type of lambda
  if (!(identical(type_lambda, "lambda.min") | identical(type_lambda, "lambda.1se"))) {
    stop("Please make sure that type_lambda is a character saying either lambda.min or lambda.1se.")
  }

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(frm)
  force(offset)
  force(family)
  force(alpha)
  force(nfolds)
  force(nlambda)
  force(tpb_knots)
  force(create_interactions)
  force(type_lambda)

  fit <- function(x, y) {

    # If there is an offset, retrieve as variable and remove from data
    if (!is.null(offset)) {
      get_offset <- x[,offset]
      if (!is.numeric(get_offset)) stop("Called from lrn_cv_glmnet: please ensure that the offset you provide is numeric.")

      x <- x[,-which(colnames(x) == offset)]
    } else {
      get_offset <- NULL
    }

    # Make a safe matrix if necessary and no formula provided
    if (is.data.frame(x) & is.null(frm)) {

      instr_list <- create_instruction_list(x)
      x <- make_safe_matrix(x, instr_list)

    } else if (is.data.frame(x) & !is.null(frm)) { # If frm provided, use in model.matrix call

      instr_list <- list(coerce_df = FALSE, frm = frm)
      x <- model.matrix(frm, data = x)[,-1, drop = FALSE]

    } else if (is.matrix(x) & is.null(frm)) { # Simplest case: take x as is

      instr_list <- "skip"

    } else if (is.matrix(x) & !is.null(frm)) { # If formula provided for matrix, coerce to df and warn

      instr_list <- list(coerce_df = TRUE, frm = frm)
      x <- model.matrix(frm, data = data.frame(x))[,-1, drop = FALSE]

      warning("Formula provided for matrix in glmnet.\nCoerced to data frame for call to model.matrix(); can be (silently) very unsafe!")

    } else stop("Unexpected data input in glmnet! Should not happen.")

    # Now have the design matrix in each case
    # Create splines and interactions if necessary and specified
    # For splines, choose up to num_knots knots at quantiles, unless length(x) < 3

    flex_list <- list()

    # Start by normalizing
    # For constant columns set sd to 1; otherwise messes things up
    get_sds <- apply(x, 2, sd)
    flex_list[["rescale"]] <- list(means = apply(x, 2, mean), sds = ifelse(get_sds == 0, 1, get_sds))
    x <- sweep(x, 2, flex_list[["rescale"]][["means"]], "-")
    x <- sweep(x, 2, flex_list[["rescale"]][["sds"]], "/")

    if (!is.null(tpb_knots)) {

      x_lengths <- apply(x, 2, function(vrb) length(unique(vrb)))
      get_spline_knots <- pmax(pmin(x_lengths - 2, tpb_knots), 0)

      append_smooth <- matrix(nrow = nrow(x), ncol = (sum(get_spline_knots > 0) * 2) + sum(get_spline_knots))

      # Append non-linear parts with pre-specified knots
      counter <- 1

      flex_list[["spline_instructions"]] <- vector("list", ncol(x))

      for (i in 1:ncol(x)) {

        if (get_spline_knots[[i]] == 0) {

          flex_list[["spline_instructions"]][[i]] <- "skip"
          next

        } else {

          get_smooth <- tps(x[,i], get_spline_knots[[i]])
          flex_list[["spline_instructions"]][[i]] <- attr(get_smooth, "knots")
          append_smooth[,seq(counter, counter + get_spline_knots[[i]] + 1)] <- get_smooth[,-1]
          counter <- counter + get_spline_knots[[i]] + 2

        }

      }

      # Through with this: now just bind together
      x <- cbind(x, append_smooth)

    }

    # For interactions just iteratively create column-wise Kronecker products
    # Right now just first-order interactions. :(
    if (!is.null(create_interactions)) {

      if (create_interactions > 2) warning("Currently only interaction orders up to 2 supported.")
      flex_list[["interactions"]] <- 2
      x <- cbind(x, col_kronecker(x))

    } else {
      flex_list[["interactions"]] <- "skip"
    }

    # Can now fit!
    get_model <- glmnet::cv.glmnet(x, y, family = family, alpha = alpha, offset = get_offset,
                                   nfolds = nfolds, nlambda = nlambda, standardize = FALSE)

    return(list(
      model = get_model,
      instructions = instr_list,
      flexibility = flex_list,
      pred_lambda = type_lambda
    ))

  }

  preds <- function(object, data) {

    # If there is an offset, retrieve as variable and remove from data
    if (!is.null(offset)) {
      get_offset <- data[,offset]
      if (!is.numeric(get_offset)) stop("Called from lrn_cv_glmnet: please ensure that the offset you provide is numeric.")

      data <- data[,-which(colnames(data) == offset)]
    } else {
      get_offset <- NULL
    }

    # Apply instruction list if necessary
    ### Ensure that creation is proper if splines and interactions implemented!
    # Make a safe matrix if necessary and no formula provided
    if (!(identical(object[["instructions"]], "skip"))) { # Only need logic if data not already right

      # If matrix, need to coerce to df again for model matrix call
      if (is.matrix(data)) {
        data <- model.matrix(object[["instructions"]][["frm"]], data = data.frame(data))[,-1, drop = FALSE]
      } else if (!is.null(object[["instructions"]][["frm"]])) {
        data <- model.matrix(object[["instructions"]][["frm"]], data = data)[,-1, drop = FALSE]
      } else { # Should be the last option here: df without formula
        data <- make_safe_matrix(data, object[["instructions"]])
      }

    }

    # Now need to match flexibility, starting with scaling
    get_means <- object[["flexibility"]][["rescale"]][["means"]]
    get_sds <- object[["flexibility"]][["rescale"]][["sds"]]

    data <- sweep(data, 2, get_means, "-")
    data <- sweep(data, 2, get_sds, "/")

    # Now splines
    if (!is.null(object[["flexibility"]][["spline_instructions"]])) {

      get_spline_instructions <- object[["flexibility"]][["spline_instructions"]]
      count_bs <- 0

      for (i in 1:length(get_spline_instructions)) {
        if (!identical("skip", get_spline_instructions[[i]])) {
          count_bs <- count_bs + length(get_spline_instructions[[i]]) + 2
        }
      }

      append_mat <- matrix(ncol = count_bs, nrow = nrow(data))

      counter <- 1

      for (i in 1:ncol(data)) {

        if (identical(get_spline_instructions[[i]], "skip")) {
          next
        } else {

          append_mat[,seq(counter, counter + length(get_spline_instructions[[i]]) + 1)] <-
            tps(data[,i], knot_seq = get_spline_instructions[[i]], num_knots = NULL)[,-1]
          counter <- counter + length(get_spline_instructions[[i]]) + 2

        }

      }

      data <- cbind(data, append_mat)

    }

    # Lastly not leastly interactions
    if (!identical(object[["flexibility"]][["interactions"]], "skip")) {

      ### Here only implemented for one-way interactions still!
      data <- cbind(data, col_kronecker(data))

    }


    # Then output


    return(as.vector(predict(object[["model"]], newx = data, type = "response", s = object[["pred_lambda"]], newoffset = get_offset)))

  }

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}


# Templates for multivariate adaptive regression splines.
#' @export
#' @import earth
lrn_earth <- function(name, family = NULL, offset = NULL, degree = 2,
                      penalty = 3, nk = 100,
                      thresh = 0.01, pmethod = "backward", nfold = 0) {

  if (missing(family)) stop("Please explicitly specify a family object for earth.")

  # Some checks ensuring there are integers where necessary
  integer_checker(degree, "the interaction degree (1 = additive model).")
  integer_checker(penalty, "the GCV penalty.")
  integer_checker(nk, "the number of knots.")
  integer_checker(nfold, "the number of cv folds (0 = no cv).", require_positive = FALSE)

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(family)
  force(offset)
  force(degree)
  force(penalty)
  force(nk)
  force(thresh)
  force(pmethod)
  force(nfold)

  fit <- function(x, y) {

    if (is.null(family)) warning("No family specified for earth. Will estimate a (standard) normal model.")

    # Very similar to how I did it for GLMs, actually!
    if (is.data.frame(x)) {
      instr_list <- create_instruction_list(x)
      x <- make_safe_matrix(x, instr_list)
    } else {
      instr_list <- "skip"
    }

    fd <- data.frame(y = y, x)

    # Formula, maybe with offset
    if (!is.null(offset)) ofs_char <- paste("offset(",offset , ")", sep = "") else ofs_char <- character(0)
    offset_col <- which(colnames(fd) == offset)
    get_frm <- formula(paste0("y ~ ", paste(c(ofs_char, colnames(fd)[-c(1, offset_col)]), collapse = " + ")))

    # Can now fit!
    # Some logic to ensure that family is only handed in if necessary
    if (!is.null(family)) {
      get_fm <- list(family = family)
    } else {
      get_fm <- NULL
    }

    get_model <- earth(formula = get_frm, data = fd, pmethod = pmethod, degree = degree,
                       nfold = nfold, thresh = thresh, penalty = penalty, nk = nk,
                       glm = get_fm)

    return(list(
      model = get_model,
      instructions = instr_list
    ))

  }

  preds <- function(object, data) {

    # Apply instruction list if necessary
    if (!identical(object[["instructions"]], "skip")) {
      data <- make_safe_matrix(data, object[["instructions"]])
    }

    # Then need to coerce to data frame either way
    data <- data.frame(data)

    # Then output
    return(as.vector(predict(object[["model"]], newdata = data, type = "response")))

  }

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}


# Templates for GLMs estimated via glmnet, which are computationally more stable
#' @export
lrn_bigGlm <- function(name, family, offset = NULL, frm = NULL) {

  if (missing(family)) stop("Please explicitly specify a family object for bigGlm.")

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(frm)
  force(offset)
  force(family)

  fit <- function(x, y) {

    # If there is an offset, retrieve as variable and remove from data
    if (!is.null(offset)) {
      get_offset <- x[,offset]
      if (!is.numeric(get_offset)) stop("Called from lrn_bigGlm: please ensure that the offset you provide is numeric.")

      x <- x[,-which(colnames(x) == offset)]
    } else {
      get_offset <- NULL
    }

    # Make a safe matrix if necessary and no formula provided
    if (is.data.frame(x) & is.null(frm)) {

      instr_list <- create_instruction_list(x)
      x <- make_safe_matrix(x, instr_list)

    } else if (is.data.frame(x) & !is.null(frm)) { # If frm provided, use in model.matrix call

      instr_list <- list(coerce_df = FALSE, frm = frm)
      x <- model.matrix(frm, data = x)[,-1, drop = FALSE]

    } else if (is.matrix(x) & is.null(frm)) { # Simplest case: take x as is

      instr_list <- "skip"

    } else if (is.matrix(x) & !is.null(frm)) { # If formula provided for matrix, coerce to df and warn

      instr_list <- list(coerce_df = TRUE, frm = frm)
      x <- model.matrix(frm, data = data.frame(x))[,-1, drop = FALSE]

      warning("Formula provided for matrix in bigGlm.\nCoerced to data frame for call to model.matrix(); can be (silently) very unsafe!")

    } else stop("Unexpected data input in bigGlm! Should not happen.")

    # Now have the design matrix in each case
    # Create splines and interactions if necessary and specified
    # For splines, choose up to num_knots knots at quantiles, unless length(x) < 3

    flex_list <- list()

    # Start by normalizing
    # For constant columns set sd to 1; otherwise messes things up
    get_sds <- apply(x, 2, sd)
    flex_list[["rescale"]] <- list(means = apply(x, 2, mean), sds = ifelse(get_sds == 0, 1, get_sds))
    x <- sweep(x, 2, flex_list[["rescale"]][["means"]], "-")
    x <- sweep(x, 2, flex_list[["rescale"]][["sds"]], "/")

    # Can now fit!
    get_model <- glmnet::bigGlm(x, y, family = family, offset = get_offset, standardize = FALSE)

    return(list(
      model = get_model,
      instructions = instr_list,
      flexibility = flex_list
    ))

  }

  preds <- function(object, data) {

    # If there is an offset, retrieve as variable and remove from data
    if (!is.null(offset)) {
      get_offset <- data[,offset]
      if (!is.numeric(get_offset)) stop("Called from lrn_bigGlm: please ensure that the offset you provide is numeric.")

      data <- data[,-which(colnames(data) == offset)]
    } else {
      get_offset <- NULL
    }

    # Apply instruction list if necessary
    ### Ensure that creation is proper if splines and interactions implemented!
    # Make a safe matrix if necessary and no formula provided
    if (!(identical(object[["instructions"]], "skip"))) { # Only need logic if data not already right

      # If matrix, need to coerce to df again for model matrix call
      if (is.matrix(data)) {
        data <- model.matrix(object[["instructions"]][["frm"]], data = data.frame(data))[,-1, drop = FALSE]
      } else if (!is.null(object[["instructions"]][["frm"]])) {
        data <- model.matrix(object[["instructions"]][["frm"]], data = data)[,-1, drop = FALSE]
      } else { # Should be the last option here: df without formula
        data <- make_safe_matrix(data, object[["instructions"]])
      }

    }

    # Now need to match scaling
    get_means <- object[["flexibility"]][["rescale"]][["means"]]
    get_sds <- object[["flexibility"]][["rescale"]][["sds"]]

    data <- sweep(data, 2, get_means, "-")
    data <- sweep(data, 2, get_sds, "/")

    return(as.vector(predict(object[["model"]], newx = data, type = "response", newoffset = get_offset)))

  }

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}


# Templates for random forests via ranger
#' @export
#' @import ranger
lrn_ranger <- function(name, select_vars = NULL, num.trees = 500, mtry = NULL, probability = FALSE,
                       min.node.size = NULL, min.bucket = NULL, max.depth = NULL, splitrule = NULL) {

  # Force evaluation of things to ensure they are available later...
  force(name)
  force(num.trees)
  force(mtry)
  force(probability)
  force(min.node.size)
  force(min.bucket)
  force(max.depth)
  force(splitrule)
  force(select_vars)

  fit <- function(x, y) {

    # First, if select_vars is provided, subset x
    if (!is.null(select_vars)) {
      x <- x[,select_vars]
    }

    # Make a safe matrix if necessary
    if (is.data.frame(x)) {
      instr_list <- create_instruction_list(x)
      x <- make_safe_matrix(x, instr_list)
    } else {
      instr_list <- "skip"
    }

    # Need a data frame for ranger
    x_df <- data.frame(x)

    # Can now fit!
    return(list(
      model = ranger::ranger(x = x_df, y = y, num.trees = num.trees, mtry = mtry,
                             probability = probability, min.node.size = min.node.size,
                             min.bucket = min.bucket, max.depth = max.depth,
                             splitrule = splitrule),
      instructions = instr_list
    ))

  }

  preds <- function(object, data) {

    # Apply instruction list if necessary
    if (!identical(object[["instructions"]], "skip")) {
      data <- make_safe_matrix(data, object[["instructions"]])
    }

    # Then need to coerce to data frame either way
    data <- data.frame(data)

    # Then output
    return(as.vector(predict(object[["model"]], data = data, type = "response")$predictions))

  }

  get_list <- list(
    name = name,
    fit = fit,
    preds = preds
  )

  class(get_list) <- "SL_Learner"

  return(get_list)

}

