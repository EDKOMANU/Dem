#' Title Flag Outliers
#' @description
#' This function flags outliers in a dataset based on user defined z-scores or
#' cut-off points on a normally distributed values.
#'
#' @param data data to be passed to the function
#' @param varname varialble to be analysed
#' @param item the first grouping variable, or the item variable if no other grouping is allowed
#' @param weight the weight variable, if any
#' @param over the other grouping variable, if any
#' @param z the z score or the cut of point
#' @param min_n the minimum number of observations in a group
#' @param verbose if TRUE, print out the summary of flagged outliers and group-level statistics
#'
#' @returns a data frame with the original data and additional columns for flagged outliers
#' @export
#'
#' @examples
#'
#' result <- flagout(
#' data = mum,
#' varname = "logvalue",
#' item = "hh_i02",
#' z = 3.5,
#' verbose = TRUE,
#' over = c("reside", "region"),
#' weight = "hh_wgt"
#' )
#'
flagout <- function(data, varname, item, weight = NULL,
                    over = NULL, z = 3, min_n = 20, verbose = FALSE) {
  library(dplyr)

  # Step 1: Check input columns
  if (!all(c(varname, item) %in% colnames(data))) {
    stop("Variable or item column not found in the data.")
  }
  if (!is.null(weight) && !weight %in% colnames(data)) {
    stop("Weight variable not found in the data.")
  }

  # Step 2: Define grouping variables
  group_vars <- unique(c(item, over))

  # Step 3: Compute stats for each group
  stats_all <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      p10 = quantile(.data[[varname]], 0.1, na.rm = TRUE),
      p25 = quantile(.data[[varname]], 0.25, na.rm = TRUE),
      center = median(.data[[varname]], na.rm = TRUE),
      p75 = quantile(.data[[varname]], 0.75, na.rm = TRUE),
      p90 = quantile(.data[[varname]], 0.9, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )

  # Step 4: Identify dropped groups
  dropped_groups <- stats_all %>%
    filter(n < min_n)

  if (nrow(dropped_groups) > 0) {
    warning("The following group(s) were dropped due to having fewer than ", min_n, " observations:\n",
            paste0(capture.output(print(dropped_groups[, c(group_vars, "n")], row.names = FALSE)), collapse = "\n"))
  }

  # Step 5: Keep only valid groups
  stats <- stats_all %>%
    filter(n >= min_n) %>%
    mutate(
      scale = ifelse((p75 - p25) == 0, (p90 - p10) / 2.56, (p75 - p25) / 1.35)
    )

  # Step 6: Merge statistics back
  data <- data %>%
    left_join(stats, by = group_vars)

  # Step 7: Flag outliers
  data <- data %>%
    mutate(
      min = center - z * scale,
      max = center + z * scale,
      flag = case_when(
        is.na(scale) | is.na(center) ~ NA_integer_,
        .data[[varname]] < min ~ -1L,
        .data[[varname]] > max ~ 1L,
        TRUE ~ 0L
      )
    )

  # Step 8: Verbose output
  if (verbose) {
    message("Summary of flagged outliers:")
    print(table(data$flag, useNA = "ifany"))
    message("Group-level statistics (n ≥ ", min_n, "):")
    print(stats)
  }

  return(data)
}
