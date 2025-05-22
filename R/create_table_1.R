

### To do:
# 1) Current handling of set_named_list_at_path horrible if a cat variable isn't already a group. Very important to fix!
# 2) Custom missingness handling (complete case across data, complete case for one variable,
# separate category with its own group)
# 3) Guesses on what type of summary a variable receives
# 4) Weights for table 1
# 5) Fix bug: stratifying by month in airquality-example broken.


#' Create a descriptive table as data frame and tinytable-object for LaTeX exports.
#'
#' @param data A data frame containing variables that are to be summarized.
#' @param treatment_name An optional character string containing the name of a factor variable to stratify descriptives by. Default: none.
#' @param sig_figs How many significant figures to print. Default: 2.
#' @param labels An optional character vector of labels to replace variable names in the table. Should be provided in the order provided to the order-argument.
#' @param order An optional character vector of variable names with the variable ordering desired for the table. Default: column order in the provided data frame.
#' @param summarize_mean An optional character vector specifying which variables are to be summarized using means and standard deviations.
#' @param summarize_median An optional character vector specifying which variables are to be summarized using medians and interquartile ranges.
#' @param summarize_binary An optional character vector specifying binary factor variables to be summarized using counts and percentages.
#' @param summarize_categorical An optional character vector specifying factor variables to be summarized using counts and percentages.
#' @param include_overall If the table is stratified, should an additional non-stratified column be added? Default is 'end', making this the last column. Other options are 'beginning' and 'none'.
#' @param groups A named list that can optionally be specified if some variables are supposed to be grouped under a header. Each list entry contains the name of the group, and a character vector with the corresponding variables.
#'
#' @return A list containing the descriptive table as a data frame, and a tinytable-object for exporting.
#' @import tinytable
#' @export
#'
#' @examples
#' data(iris)
#'
#' get_table1(
#' iris,
#' labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width", "Species"),
#' summarize_mean = c("Sepal.Length", "Sepal.Width"),
#' summarize_median = c("Petal.Length", "Petal.Width"),
#' summarize_categorical = "Species"
#' )
get_table1 <- function(data, treatment_name = NULL, sig_figs = 2, labels = NULL, order = NULL,
                       summarize_mean = NULL, summarize_median = NULL, summarize_binary = NULL,
                       summarize_categorical = NULL, include_overall = "end", groups = NULL) {

  # Ensure that a data set is handed in
  if (is.null(data)) stop("Please provide a data set.")

  # Ensure that input is a data frame
  if (!is.data.frame(data)) stop ("Please ensure that provided data is in a data frame.")

  # Ensure that, if handed in, labels, order, and summary variables all contain identical variables
  summary_set <- unique(c(summarize_mean, summarize_median, summarize_binary, summarize_categorical))

  if (
    (!is.null(labels) & (length(labels) != length(summary_set))) |
    ((!is.null(labels) & !is.null(order)) & (length(labels) != length(order))) |
    (!is.null(order) & !setequal(summary_set, order))
  ) stop("Please ensure that summary variables, and (if provided) the ordering and label vectors contain the same elements or are of the same length.")

  # Ensure that include_overall is at a viable level
  if (!include_overall %in% c("beginning", "end", "none")) {

    stop("Allowed levels for include_overall are 'beginning', 'end', and 'none'.")

  }

  if (!include_overall == "none") include_overall_bin <- TRUE else include_overall_bin <- FALSE

  # Ensure that all variables to be summarized are in data and none are duplicates
  all_var_names <- c(treatment_name, summarize_mean, summarize_median,
                     summarize_categorical, summarize_binary)

  if (length(all_var_names) != length(unique(all_var_names))) {
    stop("Please ensure that each variable is only input for one summary type")
  }

  if (sum(all_var_names %in% colnames(data)) != length(all_var_names)) {
    stop("Please ensure that all variables to be summarized are in data")
  }

  # Ensure that, if no treatment is provided, overall is set to TRUE
  if(is.null(treatment_name) & include_overall_bin == FALSE) {
    stop("If you do not provide a treatment name, please do not set include_overall to 'none'.")
  }

  # Remove variables not mentioned
  existing_set <- c(treatment_name, summarize_mean, summarize_median,
                    summarize_binary, summarize_categorical)

  data <- data[,existing_set]

  # Remove missing data, report that this was done if the case
  data <- na.omit(data)
  if (nrow(data) != nrow(na.omit(data))) {
    warning("Missing data detected. Incomplete cases removed for tabulation.")
  }

  # Remove treatment data from df, but save it and its level indices separately
  if (!is.null(treatment_name)) {

    tx_var <- data[,treatment_name]
    data <- data[, !colnames(data) %in% treatment_name]

    tx_levels <- levels(tx_var)

    tx_indeces <- lapply(tx_levels, function(levels, x) which((x == levels) == 1), x = tx_var)

    names(tx_indeces) <- tx_levels


  } else {
    tx_indeces <- list()
  }

  # If no order is provided, simply use colnames without the treatment variable.
  # If it is provided, do some checks to ensure everything is correct.
  if (is.null(order)) {

    table1_order <- colnames(data)

  } else { # Will need to do some checks for the else-option

    table1_order <- order

    if (identical(sort(table1_order), sort(colnames(data))) == FALSE) {

      stop("If you provide a custom variable order, please provide a character vector containing
            the names of all variables, excluding the treatment name.")

    }

    data <- data[,table1_order]

  }

  # Add list entry for overall summary if requested
  if (include_overall_bin == TRUE) {
    tx_indeces[[length(tx_indeces) + 1]] <- 1:nrow(data)
    names(tx_indeces)[[length(tx_indeces)]] <- "Overall"
  }

  # Get automatic labels if none provided
  if (is.null(labels)) labels <- table1_order

  # Check that groups (if provided!) are consecutive and non-overlapping
  check_non_duplicating(groups)
  check_consecutive(groups, table1_order)

  # Now recursively replace column names by indices, which we will need for later
  # Also add single groups for categorical variables here
  replace_names_by_inds <- function(grps, varnames) {

    if (is.null(grps)) return(list())

    lapply(
      grps, function(x) {
        if (is.list(x)) {
          replace_names_by_inds(x, varnames)
        } else {
          which(varnames %in% x)
        }
      }
    )

  }

  make_index_list <- replace_names_by_inds(groups, table1_order)

  # If a categorical variable shows up in groups, make it its own category

  if (!is.null(summarize_categorical)) {

    get_cat_inds <- sapply(
      summarize_categorical,
      function(cats, nms) which(nms %in% cats), nms = table1_order
    )

    # Function to identify a path
    find_path <- function(lst, target) {
      stack <- list(list(node = lst, path = list()))

      while (length(stack) > 0) {
        current <- stack[[length(stack)]]
        stack <- stack[-length(stack)]

        node <- current$node
        path <- current$path

        if (!is.list(node)) next

        for (i in seq_along(node)) {
          item <- node[[i]]
          new_path <- c(path, i)

          if (is.list(item)) {
            stack[[length(stack) + 1]] <- list(node = item, path = new_path)
          } else {
            if (identical(item, target)) {
              return(new_path)
            }
          }
        }
      }

      return(NULL)  # Not found
    }

    # Function to overwrite with a named list at a path
    set_named_list_at_path <- function(lst, path, name, value_list) {
      stopifnot(is.list(value_list))

      # Navigate to the parent of the target element
      current <- lst
      for (i in head(path, -1)) {
        current <- current[[i]]
      }

      idx <- tail(path, 1)
      current[[idx]] <- NULL  # Remove old item
      new_item <- setNames(list(value_list), name)

      # Replace in the original list structure
      parent <- lst
      ptr <- parent
      for (i in head(path, -1)) {
        ptr <- ptr[[i]]
      }
      ptr <- append(ptr, new_item, after = idx - 1)

      # Now reassign this modified sublist back up to the top
      assign_recursive <- function(lst, path, sub) {
        if (length(path) == 0) return(sub)
        lst[[path[1]]] <- assign_recursive(lst[[path[1]]], path[-1], sub)
        return(lst)
      }

      lst <- assign_recursive(lst, head(path, -1), ptr)
      return(lst)
    }

    for (i in seq_along(get_cat_inds)) {

      path <- find_path(make_index_list, get_cat_inds[[i]])

      if (is.null(path)) {

        make_index_list <- append(make_index_list, list(list(get_cat_inds[[i]])))
        names(make_index_list)[[length(make_index_list)]] <- table1_order[[get_cat_inds[[i]]]]


      } else {

        label <- labels[[get_cat_inds[[i]]]]
        make_index_list <- set_named_list_at_path(make_index_list, path, label, list(get_cat_inds[[i]]))
      }

    }

  }

  # Helper functions creating appropriate summaries
  style_mean <- function(x, sig_figs, var_label) {

    mean_x <- format(round(mean(x), sig_figs), nsmall = sig_figs)
    sd_x <- format(round(sd(x), sig_figs), nsmall = sig_figs)

    sum_string_x <- paste0(mean_x, " (", sd_x, ")")

    return_mat <- matrix(c(
      paste0(var_label, ", mean (SD)"), sum_string_x), ncol = 2
    )

    return(return_mat)

  }

  style_median <- function(x, sig_figs, var_label) {

    median_x <- format(round(median(x), sig_figs), nsmall = sig_figs)
    iqr_x <- format(round(quantile(x, c(0.25, 0.75)), sig_figs), nsmall = sig_figs)

    sum_string_x <- paste0(median_x, " [", iqr_x[[1]], ", ", iqr_x[[2]], "]")

    return_mat <- matrix(c(
      paste0(var_label, ", median [IQR]"), sum_string_x), ncol = 2
    )

    return(return_mat)

  }

  style_categorical <- function(x, sig_figs, var_label) {

    # Check that the variable is actually a factor
    if (!is.factor(x)) {
      stop("Please ensure that each variable formatted as categorical is a factor variable.")
    }

    full_length <- length(x)
    x_levels <- levels(x)

    match_cats <- sapply(x_levels, function(levels, x) length(x[x == levels]), x = x)

    match_mat <- matrix(rep(NA, 2*length(x_levels)), ncol = 2)

    for (i in 1:length(x_levels)) {

      match_mat[i, 1] <- paste0(x_levels[[i]], ", n (%)")
      match_mat[i, 2] <- paste0(match_cats[[i]], " (", round(match_cats[[i]]/full_length, sig_figs + 2)*100, "%)")

    }

    return(match_mat)

  }

  style_binary <- function(x, sig_figs, var_label) {

    # Check that the variable is actually a factor
    if (!is.factor(x)) {
      stop("Please ensure that each variable formatted as binary is a factor variable.")
    }

    full_length <- length(x)
    x_levels <- levels(x)

    match_cats <- sapply(
      sapply(x_levels, function(levels, x) x[x == levels], x = x),
      function(x) length(x)
    )

    match_mat <- matrix(rep(NA, 2*length(x_levels) - 2), ncol = 2)

    for (i in 1:(length(x_levels) - 1)) {

      match_mat[i, 1] <- paste0(x_levels[[i]], " (n, %)")
      match_mat[i, 2] <- paste0(match_cats[[i]], " (", round(match_cats[[i]]/full_length, sig_figs + 2)*100, "%)")

    }

    return(match_mat)

  }

  # Now we have variables, labels and formats and can continue with summarizing

  # Attach summary types to each variable in a matrix
  # Some weird logic, which seemed easiest to me here
  if (is.null(summarize_mean)) style_mean <- summarize_mean <- 0
  if (is.null(summarize_median)) style_median <- summarize_median <- 0
  if (is.null(summarize_categorical)) style_categorical <- summarize_categorical <- 0
  if (is.null(summarize_binary)) style_binary <- summarize_binary <- 0


  styles_attached <- rbind(
    cbind(style_mean, summarize_mean),
    cbind(style_median, summarize_median),
    cbind(style_categorical, summarize_categorical),
    cbind(style_binary, summarize_binary)
  )

  if (sum(unlist(lapply(styles_attached[,1], function(x) !is.function(x)))) != 0) {

    styles_attached <- styles_attached[-which(unlist(lapply(styles_attached[,1], function(x) !is.function(x)))),]

  }

  create_formatted_list <- function(data, stratum, styles_attached) {

    formatted_list <- vector("list", ncol(data))

    for (i in 1:length(formatted_list)) {

      get_col_index <- which(colnames(data) == styles_attached[i, 2])

      formatted_list[[get_col_index]] <- styles_attached[[i, 1]](
        x = data[stratum, get_col_index],
        sig_figs,
        var_label = labels[[get_col_index]])

    }

    return(formatted_list)

  }


  # Combine table into one
  all_table_lists <- lapply(tx_indeces, create_formatted_list, data = data,
                            styles_attached = styles_attached)

  if (length(make_index_list) > 0) {

    # Do some weird iteration over grouping structure here...
    all_elements <- list()
    get_current_lvl <- unlist(make_index_list, recursive = FALSE)
    current_depth <- 1

    while (TRUE) {

      temp_inds_1 <- unlist(lapply(get_current_lvl, function(x) !is.list(x)))

      get_non_lists <- get_current_lvl[which(temp_inds_1)]

      for (i in 1:length(get_non_lists)) {

        all_elements <- append(all_elements, list(list(depth = current_depth, index = get_non_lists[[i]])))

      }

      # Discard all non-lists now and unlist further
      temp_inds_2 <- unlist(lapply(get_current_lvl, function(x) is.list(x)))

      if (length(temp_inds_2) == 0) break

      get_lists <- get_current_lvl[which(temp_inds_2)]

      get_current_lvl <- unlist(get_lists, recursive = FALSE)

      current_depth <- current_depth + 1

    }

    # Need to do something similar for names
    temp_index_list <- make_index_list
    min_indices <- list()
    current_depth <- 0

    while (TRUE) {

      if (length(make_index_list) == 0) break

      for (i in 1:length(temp_index_list)) {

        min_indices <- append(min_indices, list(list(min_index = min(unlist(temp_index_list[[i]])), name = names(temp_index_list)[[i]], current_depth = current_depth)))

      }

      flattened <- unlist(temp_index_list, recursive = FALSE, use.names = FALSE)

      names(flattened) <- unlist(lapply(temp_index_list, function(x) {
        if (is.null(names(x))) rep("", length(x)) else names(x)
      }))

      temp_index_list <- flattened[which(unlist(lapply(flattened, is.list)))]

      if (length(temp_index_list) == 0) break

      current_depth <- current_depth + 1

    }

    # Get spacing and names where we need them in table lists
    for (n in 1:length(all_table_lists)) {

      for (i in 1:length(all_elements)) {

        space_amount <- strrep("  ", all_elements[[i]]$depth)
        all_table_lists[[n]][[all_elements[[i]]$index]][,1] <- paste0(space_amount, all_table_lists[[n]][[all_elements[[i]]$index]][,1])

      }

      for (i in length(min_indices):1) {

        space_amount <- strrep("  ", min_indices[[i]]$current_depth)

        all_table_lists[[1]][[min_indices[[i]]$min_index]] <- rbind(c(paste0(space_amount, min_indices[[i]]$name), ""),
                                                                    all_table_lists[[1]][[min_indices[[i]]$min_index]])

      }

    }

    unstructured_table <- cbind(
      do.call(rbind, all_table_lists[[1]])[,1],
      do.call(cbind,
              lapply(all_table_lists, function(x) do.call(rbind, x)[,2])
      )
    )

  } else {

    unstructured_table <- cbind(
      do.call(rbind, all_table_lists[[1]])[,1],
      do.call(cbind,
              lapply(all_table_lists, function(x) do.call(rbind, x)[,2])
      )
    )

  }

  updated_table <- unstructured_table


  colnames(updated_table)[[1]] <- "Variable"

  # If there is an overall column, it has automatically been set to the end.
  # Change it to the beginning if users request.
  if (include_overall == "beginning") {

    new_names <- c(colnames(updated_table)[[1]],
                   colnames(updated_table)[[ncol(updated_table)]],
                   colnames(updated_table)[seq(2, ncol(updated_table) - 1)])

    updated_table <- cbind(
      updated_table[,1],
      updated_table[,ncol(updated_table)],
      updated_table[,seq(2, ncol(updated_table) - 1)]
    )

    colnames(updated_table) <- new_names

  }

  df_for_printing <- data.frame(updated_table)

  # Sometimes we will want a group header provided by tinytable
  if (include_overall_bin == TRUE & !is.null(treatment_name)) {

    if (include_overall == "end") {

      group_together <- seq(2, ncol(df_for_printing) - 1)

    } else if (include_overall == "beginning") {

      group_together <- seq(3, ncol(df_for_printing))

    }

  } else group_together <- NULL


  # Now do the formatting...
  get_formatted <- tinytable::style_tt(
    tt(df_for_printing, width = 1, theme = "spacing"),
    i = 0:nrow(df_for_printing), j = 2:ncol(df_for_printing), align = "r"
  )

  if (!is.null(group_together)) {

    get_formatted <- tinytable::group_tt(get_formatted, j = list("Treatment" = group_together))

  }


  return(list(df_for_printing, get_formatted))

}

#' Create a descriptive missingness table as data frame and tinytable-object for LaTeX exports.
#'
#' @param data A data frame containing variables that are to be summarized.
#' @param subset Variables for which the missingness table is to be created. Usually a character vector; default 'missing_only' uses only variables with missings, and NULL uses all variables.
#' @param treatment_name An optional character string containing the name of a factor variable to stratify descriptives by. Default: none.
#' @param sig_figs How many significant figures to print. Default: 2.
#' @param labels An optional character vector of labels to replace variable names in the table. Should be provided in the order provided to the order-argument.
#' @param include_overall If the table is stratified, should an additional non-stratified column be added? Default is 'end', making this the last column. Other options are 'beginning' and 'none'.
#' @param groups A named list that can optionally be specified if some variables are supposed to be grouped under a header. Each list entry contains the name of the group, and a character vector with the corresponding variables.
#'
#' @return A list containing the descriptive table as a data frame, and a tinytable-object for exporting.
#' @import tinytable
#' @export
#'
#' @examples
#' data(airquality)
#'
#' get_missingness_table(
#'  airquality,
#'  subset = c("Ozone", "Solar.R", "Wind", "Temp"),
#'  labels = c("Ozone Level", "Solar", "Wind", "Temperature")
#' )
#'
#'
#'
get_missingness_table <- function(data, subset = "missing_only", treatment_name = NULL, sig_figs = 2,
                                  labels = NULL, include_overall = "end", groups = NULL) {

  # Ensure that data is handed in
  if (is.null(data)) stop("Please provide a data set.")

  # Ensure that input is a data frame
  if (!is.data.frame(data)) stop ("Please ensure that provided data is in a data frame.")

  # Quick if-else to get subsets of the data, if desired
  if (identical(subset, "missing_only")) {

    get_subset <- unique(c(names(which(lapply(lapply(data, is.na), sum) > 0)),
                           treatment_name))

  } else if (is.null(subset)) {

    get_subset <- colnames(data)

  } else if (is.character(subset)) {

    get_subset <- subset

  } else stop ("Please provide a valid argument for subset: missing_only (print only missing variables),\nNULL (print all variables), or a vector with variable names.")

  data <- data[,get_subset]

  # Table 1 ordering is simply based on the subset
  order <- colnames(data)

  if (is.null(labels)) labels <- order

  # Get treatment column, if available
  if (!is.null(treatment_name)) retain_tx <- data[,which(colnames(data) == treatment_name)]

  # Get data frames with TRUE and FALSE for missingness converted to 1 and 0
  na_df <- data.frame(lapply(data, function(x) ifelse(is.na(x), 1, 0)))

  skip_tx <- is.null(treatment_name)
  label_use_counter <- 1

  for (i in 1:ncol(na_df)) {

    # Code that skips the treatment variable in this df if we would be on it
    if (skip_tx == FALSE) {

      if (colnames(na_df)[[i]] == treatment_name) next

    }

    na_df[,i] <- factor(na_df[,i], levels = c(1, 0), labels = c(labels[[label_use_counter]], "Not Missing"))
    label_use_counter <- label_use_counter + 1

  }

  # Now retain treatment!
  if (!is.null(treatment_name)) na_df[,treatment_name] <- retain_tx

  # We summarize all using the binary summarizing option

  if (sum(colnames(na_df) == treatment_name) != 0) {

    summarize_binary <- colnames(na_df)[-(which(colnames(na_df) == treatment_name))]

  } else summarize_binary <- colnames(na_df)

  # Remove treatment variable from order and labels, if applicable
  order <- setdiff(order, treatment_name)
  labels <- setdiff(labels, treatment_name)

  # Can now apply the function
  miss_tab <- get_table1(data = na_df, treatment_name, sig_figs, labels, order,
                         summarize_binary = summarize_binary, include_overall = include_overall,
                         summarize_categorical = NULL, groups = NULL)

  return(miss_tab)

}
