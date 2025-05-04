

check_non_duplicating <- function(grps) {

  all_vars <- unlist(grps)

  if (length(all_vars) != length(unique(all_vars))) {

    stop("Please ensure that each variable is only represented up to once in groups.")

  }

}

check_consecutive <- function(grps, check_order) {

  flag <- FALSE

  # Extract all lists from current level
  extracted <- grps[which(unlist(lapply(grps, is.list)))]

  while (TRUE) {

    # Have to do this iteratively:

    # Now ensure that, for each nested list, groups are consecutive in the order implied by groups
    for (i in 1:length(extracted)) {

      sub_extract <- extracted[[i]]

      match_inds <- which(check_order %in% unlist(sub_extract))

      if (suppressWarnings(!all(seq(min(match_inds), max(match_inds)) == sort(match_inds)))) {

        stop("Please ensure that grouped variables are consecutive in the provided order.")

      }

    }

    if (length(which(unlist(lapply(unlist(extracted, recursive = FALSE), is.list)))) == 0) flag <- TRUE

    if (flag == TRUE) break

    extracted <- unlist(extracted, recursive = FALSE)[which(unlist(lapply(unlist(extracted, recursive = FALSE), is.list)))]

  }

}
