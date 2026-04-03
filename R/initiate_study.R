#' Initiate a causal inference study task
#'
#' @param data A \code{data.frame} or \code{matrix} containing all study
#'   variables. Used only during construction; not stored in the returned task.
#' @param treatment <[`tidyselect`][tidyselect::language]> Treatment column(s).
#'   Also accepts a character vector of column names or an integer index vector.
#'   All treatment columns must be numeric or integer; binary treatments must
#'   already be coded as 0/1. Stored as a matrix or data frame matching the
#'   class of \code{data}.
#' @param confounders <[`tidyselect`][tidyselect::language]> Global adjustment
#'   set. Stored as a matrix or data frame.
#' @param outcomes A fully named \code{list}. Each element identifies one study
#'   outcome via a character vector of column names or an integer index vector.
#'   Single-column outcomes are stored as plain vectors; multi-column outcomes
#'   are stored as a matrix or data frame.
#' @param time <[`tidyselect`][tidyselect::language]> Optional time variable(s).
#' @param cluster <[`tidyselect`][tidyselect::language]> Optional cluster
#'   variable(s).
#' @param treatment_labels Optional character vector of display labels for
#'   treatment columns. Named: matched by column name. Unnamed: positional.
#' @param confounder_labels Optional character vector of display labels for
#'   confounder columns. Named or unnamed (positional).
#' @param outcome_labels Optional character vector of display labels for
#'   outcomes. Named: matched by outcome list name. Unnamed: positional.
#' @param time_labels Optional character vector of display labels for time
#'   columns. Named or unnamed (positional).
#' @param cluster_labels Optional character vector of display labels for
#'   cluster columns. Named or unnamed (positional).
#' @param adjustment_sets Optional named list (names must be a subset of
#'   \code{names(outcomes)}). Each element is either a character vector of
#'   confounder column names or an integer index vector into the columns of the
#'   stored confounder block. \code{NULL} entries (or omitted outcomes) inherit
#'   the full global confounder set.
#' @param extra_vars Optional named list of arbitrary objects to be carried in
#'   the task and made available to downstream functions (e.g. penalty matrices,
#'   offset vectors, external data).
#' @param verbose Logical. If \code{TRUE} (default), pipeline functions emit
#'   informational messages.
#'
#' @return An S3 object of class \code{nana_task}.
#' @export
initiate_study <- function(data,
                           treatment,
                           confounders,
                           outcomes,
                           time              = NULL,
                           cluster           = NULL,
                           treatment_labels  = NULL,
                           confounder_labels = NULL,
                           outcome_labels    = NULL,
                           time_labels       = NULL,
                           cluster_labels    = NULL,
                           adjustment_sets   = NULL,
                           extra_vars        = NULL,
                           verbose           = TRUE) {

  # ── 1. Input validation ────────────────────────────────────────────────────
  if (!is.data.frame(data) && !is.matrix(data))
    stop("`data` must be a data.frame or matrix.")
  if (!is.list(outcomes) || is.null(names(outcomes)) || any(names(outcomes) == ""))
    stop("`outcomes` must be a fully named list (every element needs a non-empty name).")
  if (!is.null(extra_vars) && (!is.list(extra_vars) || is.null(names(extra_vars)) || any(names(extra_vars) == "")))
    stop("`extra_vars` must be a fully named list.")
  if (!is.logical(verbose) || length(verbose) != 1L)
    stop("`verbose` must be a single logical value.")

  is_df <- is.data.frame(data)
  n_col <- ncol(data)
  n_obs <- nrow(data)

  if (is.null(colnames(data)))
    colnames(data) <- paste0("V", seq_len(n_col))

  # ── 2. Zero-row proxy for tidyselect ──────────────────────────────────────
  # tidyselect only needs column names and types — zero rows are sufficient
  # and avoids any data copying regardless of dataset size.
  df_proxy <- if (is_df) data[0L, , drop = FALSE] else
    as.data.frame(data[0L, , drop = FALSE])

  # ── 3. Selection helpers ───────────────────────────────────────────────────

  # Resolves a quosure → named integer vector (name = colname, value = position).
  resolve_cols <- function(quo, arg_name, optional = FALSE) {
    if (optional && rlang::quo_is_null(quo)) return(NULL)
    tryCatch(
      tidyselect::eval_select(quo, df_proxy),
      error = function(ts_err) {
        val <- tryCatch(
          rlang::eval_tidy(quo),
          error = function(e) stop(sprintf(
            "Cannot resolve `%s`. Use tidyselect syntax, a character vector of ",
            "column names, or an integer index vector.\nUnderlying error: %s",
            arg_name, conditionMessage(ts_err)
          ))
        )
        if (is.character(val)) {
          idx <- match(val, colnames(data))
          bad <- val[is.na(idx)]
          if (length(bad))
            stop(sprintf("In `%s`: column(s) not found in data: %s",
                         arg_name, paste(bad, collapse = ", ")))
          setNames(as.integer(idx), val)
        } else if (is.numeric(val) || is.integer(val)) {
          idx <- as.integer(val)
          oob <- idx[idx < 1L | idx > n_col]
          if (length(oob))
            stop(sprintf("In `%s`: index/indices out of range: %s",
                         arg_name, paste(oob, collapse = ", ")))
          setNames(idx, colnames(data)[idx])
        } else {
          stop(sprintf(
            "In `%s`: selection must resolve to column names (character) or ",
            "indices (integer).", arg_name
          ))
        }
      }
    )
  }

  # Extracts a column block, preserving the class of `data`.
  # Single-column result becomes a plain vector when as_vec = TRUE.
  extract_block <- function(idx, as_vec = FALSE) {
    if (as_vec && length(idx) == 1L)
      return(if (is_df) data[[idx[[1L]]]] else as.vector(data[, idx[[1L]]]))
    block <- data[, idx, drop = FALSE]
    if (is_df) as.data.frame(block) else as.matrix(block)
  }

  # Resolves a raw character/integer selection against `data` and extracts.
  # Used for outcomes (which live in lists, not quosures).
  extract_from_sel <- function(sel, nm_for_error) {
    if (is.character(sel)) {
      idx <- match(sel, colnames(data))
      bad <- sel[is.na(idx)]
      if (length(bad))
        stop(sprintf("'%s': column(s) not found: %s",
                     nm_for_error, paste(bad, collapse = ", ")))
    } else if (is.numeric(sel) || is.integer(sel)) {
      idx <- as.integer(sel)
      oob <- idx[idx < 1L | idx > n_col]
      if (length(oob))
        stop(sprintf("'%s': index/indices out of range: %s",
                     nm_for_error, paste(oob, collapse = ", ")))
    } else {
      stop(sprintf(
        "'%s': selection must be a character vector or integer indices.",
        nm_for_error
      ))
    }
    extract_block(idx, as_vec = TRUE)
  }

  # Resolves display labels. Accepts named (matched by name) or unnamed
  # (positional, must match length of col_names) character vectors.
  resolve_labels <- function(user_labels, col_names, arg_name) {
    out <- setNames(col_names, col_names)
    if (is.null(user_labels)) return(out)
    if (!is.character(user_labels))
      stop(sprintf("`%s` must be a character vector.", arg_name))
    if (!is.null(names(user_labels))) {
      hits <- intersect(names(user_labels), col_names)
      out[hits] <- user_labels[hits]
    } else {
      if (length(user_labels) != length(col_names))
        stop(sprintf(
          "`%s` has %d element(s) but there are %d variable(s) in that group.",
          arg_name, length(user_labels), length(col_names)
        ))
      out[] <- user_labels
    }
    out
  }

  # ── 4. Resolve primary selections ─────────────────────────────────────────
  treatment_idx   <- resolve_cols(rlang::enquo(treatment),   "treatment")
  confounders_idx <- resolve_cols(rlang::enquo(confounders), "confounders")
  time_idx        <- resolve_cols(rlang::enquo(time),    "time",    optional = TRUE)
  cluster_idx     <- resolve_cols(rlang::enquo(cluster), "cluster", optional = TRUE)

  # ── 5. Variable overlap checks ────────────────────────────────────────────
  outcome_idx_list <- lapply(names(outcomes), function(nm) {
    sel <- outcomes[[nm]]
    if (is.character(sel)) match(sel, colnames(data)) else as.integer(sel)
  })
  names(outcome_idx_list) <- names(outcomes)

  treat_out_overlap <- intersect(treatment_idx, unlist(outcome_idx_list))
  if (length(treat_out_overlap))
    stop(sprintf(
      "Column(s) appear in both `treatment` and `outcomes`, which is never valid: %s",
      paste(colnames(data)[treat_out_overlap], collapse = ", ")
    ))

  all_idx <- c(treatment_idx, confounders_idx, time_idx, cluster_idx,
               unlist(outcome_idx_list))
  duped <- unique(all_idx[duplicated(all_idx)])
  if (length(duped))
    warning(sprintf(
      "Column(s) appear in more than one variable group: %s",
      paste(colnames(data)[duped], collapse = ", ")
    ))

  # ── 6. Extract variable blocks ─────────────────────────────────────────────
  confounders_data <- extract_block(confounders_idx)
  time_data        <- if (!is.null(time_idx))    extract_block(time_idx)    else NULL
  cluster_data     <- if (!is.null(cluster_idx)) extract_block(cluster_idx) else NULL

  # ── 7. Extract outcome blocks ─────────────────────────────────────────────
  outcome_data <- mapply(
    extract_from_sel,
    sel          = outcomes,
    nm_for_error = paste0("outcomes$", names(outcomes)),
    SIMPLIFY     = FALSE
  )

  # ── 8. Validate and extract treatment block ────────────────────────────────
  treatment_raw   <- extract_block(treatment_idx)
  treat_col_names <- names(treatment_idx)

  classify_treatment <- function(col, col_name) {
    if (!is.numeric(col) && !is.integer(col))
      stop(sprintf(
        "Treatment '%s': must be numeric or integer; got class '%s'.\n%s",
        col_name, class(col)[1L],
        "Please recode to numeric before calling initiate_study()."
      ))
    uvals <- sort(unique(col[!is.na(col)]))
    if (length(uvals) <= 1L) {
      warning(sprintf(
        "Treatment '%s': column is constant (all values = %s). %s",
        col_name,
        if (length(uvals) == 0L) "NA" else as.character(uvals),
        "This will cause problems in estimation."
      ))
      list(type = "binary", label_info = col_name)
    } else if (length(uvals) == 2L) {
      if (!identical(uvals, c(0, 1)) && !identical(uvals, c(0L, 1L)))
        warning(sprintf(
          "Treatment '%s': binary variable with values {%s, %s} is not coded as 0/1. %s",
          col_name, uvals[1L], uvals[2L],
          "Consider recoding before estimation."
        ))
      list(type = "binary", label_info = col_name)
    } else {
      list(type = "numerical", label_info = col_name)
    }
  }

  treat_results <- if (is_df) {
    lapply(treat_col_names,
           function(nm) classify_treatment(treatment_raw[[nm]], nm))
  } else {
    lapply(seq_len(ncol(treatment_raw)),
           function(i) classify_treatment(treatment_raw[, i], treat_col_names[[i]]))
  }
  names(treat_results) <- treat_col_names

  treatment_meta      <- lapply(treat_results, function(x)
    list(type = x$type, label_info = x$label_info))
  treatment_processed <- extract_block(treatment_idx)

  # ── 9. Resolve display labels ──────────────────────────────────────────────
  trt_auto <- setNames(
    vapply(treatment_meta, function(m) m$label_info, character(1L)),
    treat_col_names
  )
  trt_user <- resolve_labels(treatment_labels, treat_col_names, "treatment_labels")
  resolved_treatment_labels        <- ifelse(trt_user != treat_col_names, trt_user, trt_auto)
  names(resolved_treatment_labels) <- treat_col_names

  resolved_confounder_labels <- resolve_labels(
    confounder_labels, colnames(confounders_data), "confounder_labels"
  )
  resolved_outcome_labels <- resolve_labels(
    outcome_labels, names(outcomes), "outcome_labels"
  )
  resolved_time_labels <- if (!is.null(time_data))
    resolve_labels(time_labels, colnames(time_data), "time_labels") else NULL
  resolved_cluster_labels <- if (!is.null(cluster_data))
    resolve_labels(cluster_labels, colnames(cluster_data), "cluster_labels") else NULL

  # ── 10. Per-outcome adjustment sets ───────────────────────────────────────
  # Stored as integer indices into the *columns of confounders_data*.
  # NULL means "use all confounders"; resolved downstream via:
  #   adj <- task$adjustment_sets[[nm]]
  #   W   <- if (is.null(adj)) task$confounders else task$confounders[, adj, drop = FALSE]
  n_conf      <- ncol(confounders_data)
  conf_names  <- colnames(confounders_data)

  resolve_adj_set <- function(sel, nm) {
    if (is.character(sel)) {
      idx <- match(sel, conf_names)
      bad <- sel[is.na(idx)]
      if (length(bad))
        stop(sprintf(
          "`adjustment_sets$%s`: column(s) not found in confounders: %s",
          nm, paste(bad, collapse = ", ")
        ))
      as.integer(idx)
    } else if (is.numeric(sel) || is.integer(sel)) {
      idx <- as.integer(sel)
      oob <- idx[idx < 1L | idx > n_conf]
      if (length(oob))
        stop(sprintf(
          "`adjustment_sets$%s`: index/indices out of range for confounders (ncol = %d): %s",
          nm, n_conf, paste(oob, collapse = ", ")
        ))
      idx
    } else {
      stop(sprintf(
        "`adjustment_sets$%s`: selection must be a character vector or integer indices.",
        nm
      ))
    }
  }

  resolved_adj_sets <- if (!is.null(adjustment_sets)) {
    unknown <- setdiff(names(adjustment_sets), names(outcomes))
    if (length(unknown))
      warning(sprintf(
        "`adjustment_sets` name(s) not matching any outcome and will be ignored: %s",
        paste(unknown, collapse = ", ")
      ))
    lapply(setNames(names(outcomes), names(outcomes)), function(nm) {
      if (nm %in% names(adjustment_sets))
        resolve_adj_set(adjustment_sets[[nm]], nm)
      else
        NULL
    })
  } else NULL

  # ── 11. Censoring: auto-detect from outcome NAs ───────────────────────────
  detect_censoring <- function(y, nm) {
    na_mask <- if (is.vector(y)) is.na(y) else apply(is.na(as.matrix(y)), 1L, any)
    if (!any(na_mask)) return(NULL)
    if (verbose) message(sprintf(
      "Outcome '%s': %d missing value(s) detected. %s",
      nm, sum(na_mask),
      "Censoring indicator created (1 = observed, 0 = censored)."
    ))
    as.integer(!na_mask)
  }

  censoring <- Filter(
    Negate(is.null),
    mapply(detect_censoring, outcome_data, names(outcome_data), SIMPLIFY = FALSE)
  )

  # ── 12. Assemble task — data goes out of scope here ───────────────────────
  structure(
    list(
      n_obs             = n_obs,

      treatment         = treatment_processed,
      treatment_meta    = treatment_meta,
      treatment_labels  = resolved_treatment_labels,

      confounders       = confounders_data,
      confounder_labels = resolved_confounder_labels,

      outcomes          = outcome_data,
      outcome_labels    = resolved_outcome_labels,

      time              = time_data,
      time_labels       = resolved_time_labels,
      cluster           = cluster_data,
      cluster_labels    = resolved_cluster_labels,

      adjustment_sets   = resolved_adj_sets,
      censoring         = censoring,
      extra_vars        = extra_vars,

      # Pipeline slots — populated by downstream functions
      interventions     = NULL,
      fold_store        = NULL,
      treatment_spec    = NULL,
      outcome_spec      = NULL,
      treatment_fit     = NULL,
      table_one         = NULL,
      outcome_fit       = NULL,
      tmle_result       = NULL,

      verbose           = verbose
    ),
    class = "nana_task"
  )
}


#' @export
print.nana_task <- function(x, ...) {
  cat(sprintf("── nana_task %s\n", paste(rep("\u2500", 38), collapse = "")))
  cat(sprintf("  Observations : %d\n\n", x$n_obs))

  # Treatments
  cat("  Treatments\n")
  type_tag <- function(type) switch(type,
                                    binary    = "[binary]",
                                    numerical = "[numerical]",
                                    sprintf("[%s]", type)
  )
  for (nm in names(x$treatment_meta)) {
    m <- x$treatment_meta[[nm]]
    cat(sprintf("    \u00b7 %-35s %s\n",
                x$treatment_labels[[nm]], type_tag(m$type)))
  }
  cat("\n")

  # Confounders
  cat(sprintf("  Confounders  : %d variable(s)\n\n", ncol(x$confounders)))

  # Outcomes
  cat("  Outcomes\n")
  for (nm in names(x$outcomes)) {
    y        <- x$outcomes[[nm]]
    # Only show dimensions for multi-column outcomes; single-column is obvious
    dim_str  <- if (!is.vector(y)) sprintf("  (%d columns)", ncol(y)) else ""
    cens_str <- if (nm %in% names(x$censoring))
      sprintf("  [%d censored]", sum(x$censoring[[nm]] == 0L)) else ""
    cat(sprintf("    \u00b7 %s%s%s\n",
                x$outcome_labels[[nm]], dim_str, cens_str))
  }

  # Optional structural variables
  optional_lines <- character(0)
  if (!is.null(x$time))
    optional_lines <- c(optional_lines,
                        sprintf("  Time         : %d variable(s)", ncol(x$time)))
  if (!is.null(x$cluster))
    optional_lines <- c(optional_lines,
                        sprintf("  Cluster      : %d variable(s)", ncol(x$cluster)))
  if (!is.null(x$adjustment_sets)) {
    custom_n <- sum(vapply(x$adjustment_sets, Negate(is.null), logical(1L)))
    optional_lines <- c(optional_lines,
                        sprintf("  Adj. sets    : custom for %d / %d outcome(s)",
                                custom_n, length(x$outcomes)))
  }
  if (!is.null(x$extra_vars))
    optional_lines <- c(optional_lines,
                        sprintf("  Extra vars   : %s",
                                paste(names(x$extra_vars), collapse = ", ")))
  if (length(optional_lines))
    cat("\n", paste(optional_lines, collapse = "\n"), "\n", sep = "")

  # Pipeline status
  pipeline_slots <- c("estimands", "cv_folds", "treatment_spec", "outcome_spec",
                      "treatment_fit", "outcome_fit", "tmle_result")
  filled <- pipeline_slots[
    !vapply(pipeline_slots, function(s) is.null(x[[s]]), logical(1L))
  ]
  if (length(filled))
    cat(sprintf("\n  Pipeline     : %s\n", paste(filled, collapse = ", ")))

  cat(paste(rep("\u2500", 50), collapse = ""), "\n")
  invisible(x)
}
