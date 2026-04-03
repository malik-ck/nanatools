#' Create a Table 1 summary of baseline characteristics
#'
#' @param task A \code{nana_task} object.
#' @param vars <[`tidyselect`][tidyselect::language]> Optional subset of
#'   confounder columns to include. If \code{NULL} (default), all confounders
#'   are included.
#' @param stratify Optional character vector naming one or more treatment
#'   columns to stratify by. If more than one is provided, a combined factor
#'   is constructed from all unique combinations of the stratification
#'   variables, with each level labelled as a slash-separated concatenation
#'   of per-variable labels.
#' @param categorize Optional named list for categorizing treatment variables
#'   before stratification. Names must match treatment column names. Each
#'   element is either a numeric vector of cut points (passed to
#'   \code{cut(..., include.lowest = TRUE)}) or a function returning a factor.
#' @param use_weights Logical. If \code{TRUE}, inverse probability weights
#'   stored in \code{task$ipw_weights[[stratify]]} are applied. Only supported
#'   when a single stratification variable is provided.
#' @param few_unique_threshold Integer. Numeric variables with at most this
#'   many unique non-missing values are summarised as median [IQR]; those with
#'   more as mean (SD). Defaults to \code{10L}.
#' @param max_strata Integer. Maximum unique combinations permitted in the
#'   (combined) stratification variable. Defaults to \code{10L}.
#' @param ... Additional arguments passed to \code{gtsummary::tbl_summary()}
#'   or \code{gtsummary::tbl_svysummary()}.
#'
#' @return A \code{gtsummary} table object.
#' @export
create_table_one <- function(task,
                             vars                 = NULL,
                             stratify             = NULL,
                             categorize           = NULL,
                             use_weights          = FALSE,
                             few_unique_threshold = 10L,
                             max_strata           = 10L,
                             ...) {

  if (!inherits(task, "nana_task"))
    stop("`task` must be a nana_task object.")
  if (!requireNamespace("gtsummary", quietly = TRUE))
    stop("Package 'gtsummary' is required. Install with install.packages('gtsummary').")
  if (!is.logical(use_weights) || length(use_weights) != 1L)
    stop("`use_weights` must be a single logical value.")
  if (!is.null(stratify) && !is.character(stratify))
    stop("`stratify` must be a character vector of treatment column names.")
  if (use_weights && length(stratify) != 1L)
    stop("IPW weights are only supported with a single stratification variable.")
  if (!is.numeric(few_unique_threshold) || few_unique_threshold < 2L)
    stop("`few_unique_threshold` must be a single integer >= 2.")
  if (!is.numeric(max_strata) || max_strata < 2L)
    stop("`max_strata` must be a single integer >= 2.")

  few_unique_threshold <- as.integer(few_unique_threshold)
  max_strata           <- as.integer(max_strata)

  # ── 1. Resolve confounder block ───────────────────────────────────────────
  conf <- if (is.data.frame(task$confounders)) task$confounders else
    as.data.frame(task$confounders)

  # ── 2. Optionally subset confounders via tidyselect ───────────────────────
  vars_quo <- rlang::enquo(vars)
  if (!rlang::quo_is_null(vars_quo)) {
    proxy <- conf[0L, , drop = FALSE]
    sel <- tryCatch(
      tidyselect::eval_select(vars_quo, proxy),
      error = function(e) {
        val <- tryCatch(rlang::eval_tidy(vars_quo), error = function(e2)
          stop(sprintf("Cannot resolve `vars`.\nUnderlying error: %s",
                       conditionMessage(e))))
        if (is.character(val)) {
          idx <- match(val, colnames(conf))
          bad <- val[is.na(idx)]
          if (length(bad))
            stop(sprintf("`vars`: column(s) not found in confounders: %s",
                         paste(bad, collapse = ", ")))
          setNames(as.integer(idx), val)
        } else if (is.numeric(val)) {
          idx <- as.integer(val)
          oob <- idx[idx < 1L | idx > ncol(conf)]
          if (length(oob))
            stop(sprintf("`vars`: index/indices out of range: %s",
                         paste(oob, collapse = ", ")))
          setNames(idx, colnames(conf)[idx])
        } else stop("`vars` must resolve to column names or integer indices.")
      }
    )
    conf <- conf[, sel, drop = FALSE]
  }

  conf_labels <- as.list(setNames(
    vapply(colnames(conf), function(nm)
      task$confounder_labels[[nm]] %||% nm, character(1L)),
    colnames(conf)
  ))

  # ── 3. Recode a single treatment vector for display ───────────────────────
  # Binary 0/1 numeric → "No" (0) / "Yes" (1).
  # Continuous → binned via categorize spec; levels carry the cut description.
  # Factor/character → left as-is.
  recode_strat_var <- function(col, col_name) {

    # Apply categorization if requested
    if (!is.null(categorize) && col_name %in% names(categorize)) {
      spec <- categorize[[col_name]]
      if (is.numeric(spec)) {
        col <- cut(col, breaks = spec, include.lowest = TRUE)
      } else if (is.function(spec)) {
        col <- spec(col)
        if (!is.factor(col))
          stop(sprintf("`categorize$%s`: function must return a factor.",
                       col_name))
      } else {
        stop(sprintf(
          "`categorize$%s`: must be a numeric vector of cut points or a function.",
          col_name))
      }
      return(col)   # already a labelled factor from cut()
    }

    # Binary 0/1 numeric → Yes/No factor
    if (is.numeric(col) || is.integer(col)) {
      uvals <- sort(unique(col[!is.na(col)]))
      if (length(uvals) <= 2L &&
          all(uvals %in% c(0L, 1L, 0, 1))) {
        return(factor(ifelse(col == 1, "Yes", "No"),
                      levels = c("No", "Yes")))
      }
    }

    # Factor or character: convert to factor, keep existing levels
    if (is.factor(col)) return(col)
    if (is.character(col)) return(factor(col))

    # Numeric with more than 2 values but no categorize spec — error clearly
    stop(sprintf(
      paste0("Treatment '%s' is continuous with more than 2 unique values ",
             "and no `categorize` spec was provided.\n",
             "Supply a cut-point vector or function in `categorize` to bin ",
             "it before stratification."),
      col_name
    ))
  }

  # ── 4. Build per-variable labelled factor and combined strata factor ───────
  strat_vec   <- NULL
  strat_label <- NULL

  if (!is.null(stratify)) {
    trt <- if (is.data.frame(task$treatment)) task$treatment else
      as.data.frame(task$treatment)

    bad_names <- setdiff(stratify, colnames(trt))
    if (length(bad_names))
      stop(sprintf(
        "`stratify`: column(s) not found in treatment: %s\nAvailable: %s",
        paste(bad_names, collapse = ", "),
        paste(colnames(trt), collapse = ", ")
      ))

    # Recode each stratification variable and build a prefix label per level
    # Format: "Var label: level value"
    recoded_list <- lapply(stratify, function(nm) {
      col     <- trt[[nm]]
      recoded <- recode_strat_var(col, nm)
      lbl     <- task$treatment_labels[[nm]] %||% nm
      # Prefix each factor level with the variable label
      new_levels <- paste0(lbl, ": ", levels(recoded))
      factor(paste0(lbl, ": ", as.character(recoded)), levels = new_levels)
    })
    names(recoded_list) <- stratify

    if (length(stratify) == 1L) {
      strat_vec   <- recoded_list[[1L]]
      strat_label <- task$treatment_labels[[stratify]] %||% stratify
    } else {
      # Combine into a single factor of "/" separated level combinations
      combo_strings <- do.call(paste, c(
        lapply(recoded_list, as.character),
        list(sep = " / ")
      ))

      # All level combinations (preserves ordering)
      level_grid <- do.call(expand.grid, c(
        lapply(recoded_list, levels),
        list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
      ))
      all_levels <- apply(level_grid, 1L, paste, collapse = " / ")

      strat_vec   <- factor(combo_strings, levels = all_levels)
      strat_label <- paste(
        vapply(stratify, function(nm)
          task$treatment_labels[[nm]] %||% nm, character(1L)),
        collapse = " / "
      )
    }

    # Drop unused combinations and check strata count
    strat_vec <- droplevels(strat_vec)
    n_strata  <- nlevels(strat_vec)
    if (n_strata > max_strata)
      stop(sprintf(
        paste0("Combined stratification variable has %d unique level(s), ",
               "exceeding max_strata = %d.\n",
               "Use `categorize` to reduce the number of levels, or increase ",
               "`max_strata`."),
        n_strata, max_strata
      ))
  }

  # ── 5. Classify confounders for summary statistic selection ───────────────
  classify_var <- function(col) {
    if (is.factor(col) || is.character(col)) return("categorical")
    n_unique <- length(unique(col[!is.na(col)]))
    if (n_unique <= 2L)                   return("categorical")
    if (n_unique <= few_unique_threshold) return("continuous_few")
    "continuous_many"
  }

  var_classes <- vapply(conf, classify_var, character(1L))

  type_list <- as.list(ifelse(var_classes == "categorical",
                              "categorical", "continuous"))
  names(type_list) <- names(var_classes)

  statistic_list <- setNames(lapply(var_classes, function(vc) switch(vc,
                                                                     categorical     = "{n} ({p}%)",
                                                                     continuous_few  = "{median} [{p25}, {p75}]",
                                                                     continuous_many = "{mean} ({sd})"
  )), names(var_classes))

  # ── 6. Assemble table data ────────────────────────────────────────────────
  tbl_data <- conf
  if (!is.null(strat_vec))
    tbl_data[[".strata"]] <- strat_vec

  by_var <- if (!is.null(strat_vec)) ".strata" else NULL

  # ── 7. Build gtsummary table ──────────────────────────────────────────────
  if (use_weights) {
    if (!requireNamespace("survey", quietly = TRUE))
      stop("Package 'survey' is required for weighted tables.")
    if (is.null(task$ipw_weights))
      stop("No IPW weights found in task.")
    if (!stratify %in% names(task$ipw_weights))
      stop(sprintf("No IPW weights for '%s'. Available: %s",
                   stratify, paste(names(task$ipw_weights), collapse = ", ")))

    wts <- task$ipw_weights[[stratify]]
    if (length(wts) != nrow(tbl_data))
      stop(sprintf("IPW weights have length %d but data has %d rows.",
                   length(wts), nrow(tbl_data)))

    svy_des <- survey::svydesign(ids = ~1, data = tbl_data, weights = wts)
    tbl <- gtsummary::tbl_svysummary(
      data      = svy_des,
      by        = by_var,
      include   = names(conf),
      type      = type_list,
      statistic = statistic_list,
      label     = conf_labels,
      ...
    )
  } else {
    tbl <- gtsummary::tbl_summary(
      data      = tbl_data,
      by        = by_var,
      include   = names(conf),
      type      = type_list,
      statistic = statistic_list,
      label     = conf_labels,
      ...
    )
  }

  # ── 8. Post-processing ────────────────────────────────────────────────────
  if (!is.null(strat_vec)) {
    tbl <- gtsummary::add_overall(tbl)
    tbl <- gtsummary::modify_spanning_header(
      tbl,
      gtsummary::all_stat_cols() ~ sprintf("**%s**", strat_label)
    )
  }

  tbl <- gtsummary::modify_caption(
    tbl, "**Table 1. Baseline characteristics**"
  )

  task$table_one <- tbl
  task
}
