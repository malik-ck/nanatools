

# Draw from a uniform dirichlet distribution
r_unif_dirichlet <- function(n, k) {

  if (n <= 0 || k <= 1) {
    stop("n must be positive and k must be at least 2")
  }

  # Generate n x k matrix of gamma(1,1) i.e. exp(1) random variables
  gamma_samples <- matrix(rexp(n * k, rate = 1), nrow = k, ncol = n)

  # Normalize each row to sum to 1
  return(gamma_samples / (colSums(gamma_samples) / k))

}

r_unif_dirichlet(1, 1000)


tabulate(sample(1:1000, 1000, replace = TRUE))


# Write a transform-type function that applies to variable names
quick_transform <- function(data, name, replacement) {

  data[,name] <- replacement
  return(data)

}


# Some typechecking for fwb_tmle_bin
typecheck_lazy_sl_tmle <- function(model_obj, metalearner_type) {

  if ("LazySL" %in% class(model_obj)) {

    if (is.null(metalearner_type) & model_obj$was_cv_ensemble == FALSE & model_obj$metalearner_count > 1) {

      stop("A LazySL-object with unspecified metalearner detected.\nIf your ensemble is not cross-validated and you have multiple metalearners,\nyou need to specify a metalearner name in metalearner_treatment.")

    }

    if (is.null(metalearner_type) & model_obj$was_cv_ensemble == TRUE & model_obj$metalearner_count > 1) {

      warning("A LazySL-object with multiple metalearners detected, but none specified. Defaults to the best as determined via out-of-sample loss.")

    }


  }

  if (!is.null(model_obj) & sum(class(model_obj) %in% c("LazySL", "glm", "weightit")) == 0) {

    stop("Please ensure that model objects are of class LazySL, glm, or weightit.")

  }

  return(NULL)

}

typechecks_fwb_tmle_bin <- function(treatment_model, or_full, treatment_name,
                                    metalearner_treatment, metalearner_outcome, trim_ipw, n_bstrap) {

  if (is.null(treatment_name)) stop("Please provide a treatment name.")
  if (is.null(or_full)) stop("Please provide an outcome model.")

}

# Column applier
apply_cols <- function(X, FUN, ...) {

  if (is.data.frame(X)) {
    lapply(X, FUN, ...)
  } else if (is.matrix(X)) {
    apply(X, 2, FUN, ...)
  } else {
    stop("Input must be a data frame or matrix")
  }

}

# Instruction list creator for the use with GLMs and the likes that handles factors well
create_instruction_list <- function(data) {

  # First get column indices
  potential_vars <- 1:ncol(data)

  # Start by identifying columns with zero variance
  identify_constants <- which(unlist(apply_cols(data, function(x) length(unique(x)) <= 1)))

  # Now iterate over all others and, if not numeric, explicitly enumerate their levels
  instruction_list <- vector("list", length(potential_vars))

  for (i in potential_vars) {

    # Ensure that the current class is valid
    if (!inherits(
      data[,i],
      c("numeric", "logical", "POSIXct", "POSIXt", "difftime", "Date", "integer",
        "character", "factor", "ordered")
    )) {

      get_class <- class(data[,i])

      stop(
        paste0("Invalid column class detected: ", paste(get_class, collapse = ", "), ". Please ensure each predictor inherits one of the following classes:\nnumeric, logical, POSIXct, POSIXt, difftime, Date, integer, character, factor, ordered.")
      )

    }

    # First, set instructions to NULL if constant (i.e., not part of design matrix)
    if (i %in% identify_constants) {
      instruction_list[[i]] <- "ignore"
    } else if (inherits(data[,i], "numeric")) { # If numeric, just return the vector
      instruction_list[[i]] <- "identity"
    } else if (inherits(data[,i], c("logical", "POSIXct", "POSIXt", "difftime", "Date", "integer"))) { # If one of these, coerce to numeric and return
      instruction_list[[i]] <- "coerce_numeric"
    } else if (inherits(data[,i], c("character", "factor", "ordered"))) {

      # Most complex handling here: Enumerate all levels in data and dummy code, taking as reference the class with most 1's
      get_lvls <- unique(data[,i])

      # Get the largest class...
      get_class_sizes <- rep(NA, length(get_lvls))
      get_current_predictor <- data[,i]

      get_class_sizes <- tabulate(match(get_current_predictor, get_lvls))

      # Remove the largest level from dummy variable creation to make it the reference, also coerce to character for it to lose unnecessary attributes
      get_reference <- get_lvls[which.max(get_class_sizes)]
      to_indicate <- as.character(setdiff(get_lvls, get_reference))

      # Put that character vector into the instruction list
      instruction_list[[i]] <- list(search_lvls = to_indicate)

    }

  }

  names(instruction_list) <- colnames(data)

  return(instruction_list)

}

# Safe matrix creator given x and an instruction list
make_safe_matrix <- function(data, instr_list) {

  # First, determine how many columns we will need
  track_col_n <- 0

  for (i in 1:length(instr_list)) {

    if (instr_list[[i]] == "ignore") {
      track_col_n <- track_col_n # Do not add anything here
    } else if (class(instr_list[[i]]) != "list") {
      track_col_n <- track_col_n + 1 # Single column added whenever not a list
    } else {
      track_col_n <- track_col_n + length(instr_list[[i]][[1]]) # Add number of categories if list
    }

  }

  # Create empty matrix
  built_mat <- matrix(ncol = track_col_n, nrow = nrow(data))

  # Now build matrix, track column names and filled columns along the way
  current_col <- 1
  assigned_names <- rep(NA, track_col_n)

  for (i in 1:length(instr_list)) {

    if (instr_list[[i]] == "ignore") {
      next
    } else if (instr_list[[i]] == "identity") {
      built_mat[,current_col] <- data[,i]
      assigned_names[[current_col]] <- colnames(data)[[i]]
      current_col <- current_col + 1
    } else if (instr_list[[i]] == "coerce_numeric") {
      built_mat[,current_col] <- as.numeric(data[,i])
      assigned_names[[current_col]] <- colnames(data)[[i]]
      current_col <- current_col + 1
    } else if (class(instr_list[[i]]) == "list") {

      add_levels <- instr_list[[i]][[1]]

      # Quick matrix creation for current dummies
      current_dummies <- matrix(ncol = length(add_levels), nrow = nrow(data))
      current_var <- data[,i]

      for (k in 1:length(add_levels)) {
        current_dummy <- ifelse(current_var == add_levels[[k]], 1, 0)
        built_mat[,current_col] <- current_dummy
        assigned_names[[current_col]] <- paste0(colnames(data)[[i]], add_levels[[k]])
        current_col <- current_col + 1
      }

    } else stop("Whoops, the instruction list contained an unexpected value!")

  }

  # Now have matrix filled, can assign names and return it
  colnames(built_mat) <- assigned_names

  return(built_mat)

}

# Function to check whether something is an integer, with some tolerance just in case
# Can alternatively return the rounded value to make sure
integer_checker <- function(x, object_name = NULL, require_positive = TRUE, return = FALSE) {

  if (!is.null(x)) {
    if ((abs(round(x) - x)) > 1e-16 | !is.numeric(x)) {

      if (is.null(object_name)) {
        stop("Non-whole number or integer detected where one was expected. Please check inputs.")
      } else {
        stop(paste("Please specify NULL, an integer or a whole number for", object_name, collapse = " "))
      }
    }

    # Just do this separately
    if (require_positive == TRUE & x < 1) {

      if (is.null(object_name)) {
        stop("Non-positive integer detected where one was expected. Please check inputs.")
      } else {
        stop(paste("Please specify NULL, a positive integer or a whole number for", object_name, collapse = " "))
      }
    }
  }

  if (return == FALSE) {
    return(NULL)
  } else {
    return(round(x))
  }

}

# Truncated power basis splines, useful in combination with ridge penalties.
tps <- function(x, num_knots = 20, knot_seq = NULL, degree = 3, intercept = FALSE) {

  integer_checker(degree, "the spline degree.", require_positive = FALSE)
  integer_checker(num_knots, "the number of knots.")
  if (degree < 0) stop("Degree needs to be non-negative.")

  # Some typechecks
  if (!is.null(num_knots) & !is.null(knot_seq)) {
    warning("Both num_knots and knot_seq are provided. knot_seq used for construction.")
    num_knots <- length(knot_seq)
  }

  if (is.null(num_knots) & is.null(knot_seq)) {
    warning("Both num_knots and knot_seq are NULL. Creating basis with knots at all unique values.")
    knot_seq <- unique(sort(x))[-c(1, length(unique(x)))]
    num_knots <- length(knot_seq)
  }

  # Keep the order of x for re-ordering later
  order_x <- order(x)
  sorted_x <- sort(x)

  # Get knot values if not provided (at quantiles defined via num_knots)
  if (is.null(knot_seq)) {
    knot_seq <- quantile(sorted_x, seq(0, 1, length.out = num_knots + 2))[-c(1, num_knots + 2)]
  } else {
    num_knots <- length(knot_seq)
  }

  # Build the degree-th polynomial, which is the start of the design matrix
  start_mat <- matrix(ncol = intercept + degree, nrow = length(x))

  if (intercept == TRUE) start_mat[,1] <- 1
  for (i in 1:degree) start_mat[,i + intercept] <- sorted_x^i

  # Project a sorted x into a matrix with all columns needed
  # Also adjust following columns by subtracting knot values
  # Cube at the end

  spline_mat <- `colnames<-`(
    cbind(start_mat,
          pmax(
            matrix(
              rep(sorted_x, times = num_knots), ncol = num_knots
            ) - matrix(
              rep(knot_seq, each = length(x)), ncol = num_knots
            ),
            0)^degree
    )[order(order_x),],
    NULL
  )

  # Now just set some attributes
  attr(spline_mat, "knots") <- knot_seq
  attr(spline_mat, "range") <- c(sorted_x[[1]], sorted_x[[length(sorted_x)]])
  attr(spline_mat, "has_intercept") <- intercept
  attr(spline_mat, "degree") <- degree

  # Set new class
  # Might be helpful for the future, if I want to reconstruct bases via generics
  class(spline_mat) <- c("TruncatedPowerSpline", class(spline_mat))

  return(spline_mat)

}

# Column-wise Kronecker products, useful for tensor product smooths.
# If mat2 is NULL, creates instead a full interaction basis for mat1.
col_kronecker <- function(mat1, mat2 = NULL) {

  if (!is.null(mat2)) {
    if(nrow(mat1) != nrow(mat2)) {
      stop("Please provide matrices with the same number of rows.")
    }
  }

  if (is.null(mat2)) {

    # In this case need to omit interactions among the same columns
    num_col <- ncol(mat1)

    index_1 <- unlist(lapply(seq(1, num_col), function(x) seq(x, num_col)))

    index_2 <- rep(0, (num_col * (num_col + 1)) / 2)
    index_2[cumsum(seq(num_col, 2)) + 1] <- 1
    index_2 <- cumsum(index_2) + 1

    # This gives the indeces we need; now multiply and return
    return(mat1[,index_1] * mat1[,index_2])

  } else {

    get_full_prod <- apply(mat2, 2, function(x) x * mat1)
    col_num <- ncol(mat1) * ncol(mat2)
    return(matrix(get_full_prod, ncol = col_num, nrow = nrow(mat1)))

  }

}
