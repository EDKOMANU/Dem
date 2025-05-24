#' @name DemographicModelBase
#' @title Base Class for Demographic Models
#' @description An R6 class that provides common functionality for demographic models.
#' This class is intended to be inherited by specific demographic models like
#' FertilityModel, MortalityModel, and MigrationModel.
#' @export
DemographicModelBase <- R6::R6Class(
  "DemographicModelBase",

  public = list(
    #' @field data data.table: The input dataset stored during initialization.
    data = NULL,
    #' @field var_map list: A list mapping standard variable names
    #' (e.g., "time", "age", "population") to the actual column names in the original `data`.
    var_map = NULL,

    #' @description
    #' Initialize the DemographicModelBase object.
    #' @param data A data frame or data.table containing the demographic data.
    #' @param variable_mapping A list that maps standard demographic variable names
    #' to the actual column names in the `data`. For example:
    #' `list(time = "Year", age = "AgeGroup", population = "PopCount")`.
    initialize = function(data, variable_mapping) {
      if (is.null(data)) {
        # Using logger::log_error if logger is consistently available, otherwise stop/warning
        # For R6 classes, simple stop() is often clear enough for critical issues.
        stop("Input data cannot be NULL.")
      }
      if (is.null(variable_mapping) || !is.list(variable_mapping) || length(variable_mapping) == 0) {
        stop("Variable mapping must be a non-empty list.")
      }

      self$data <- data.table::as.data.table(data)
      self$var_map <- variable_mapping
      
      # Initial validation: Check if all columns specified in var_map values exist in the input data
      # This ensures that the mapping refers to actual columns.
      mapped_cols_in_data <- unlist(variable_mapping)
      missing_in_input <- setdiff(mapped_cols_in_data, names(self$data))
      if (length(missing_in_input) > 0) {
          msg <- paste("The following columns specified in variable_mapping are not found in the input data:",
                       paste(missing_in_input, collapse = ", "))
          stop(msg)
      }
      # logger::log_info("DemographicModelBase initialized.") # Assuming logger is setup
    },

    #' @description
    #' Retrieve the variable mapping.
    #' @return A list containing the variable mapping.
    get_mapping = function() {
      return(self$var_map)
    },

    #' @description
    #' Prepare data for modeling. This involves selecting relevant columns based
    #' on `variable_mapping` (from original data names), renaming them to 
    #' standard names (keys of `variable_mapping`), and creating standardized 
    #' variables like `year_std` or factor versions of categorical variables.
    #' @param newdata Optional. A new data frame or data.table to prepare.
    #' If NULL, the data provided during initialization (`self$data`) is used.
    #' The structure (column names) of `newdata` should match `self$data`
    #' if `variable_mapping` is to be applied correctly.
    #' @return A data.table with selected, renamed, and potentially transformed columns
    #' using standard names.
    prepare_model_data = function(newdata = NULL) {
      if (is.null(self$var_map)) {
        stop("Variable mapping (var_map) is not set. Cannot prepare model data.")
      }

      current_data_to_process <- NULL
      if (!is.null(newdata)) {
        current_data_to_process <- data.table::as.data.table(newdata)
      } else {
        if (is.null(self$data)) {
          stop("No data available to prepare. Provide newdata or initialize with data.")
        }
        current_data_to_process <- data.table::copy(self$data) # Use a copy
      }

      # Check if all original column names specified in var_map values exist in current_data_to_process
      source_columns_needed <- unlist(self$var_map)
      missing_source_cols <- setdiff(source_columns_needed, names(current_data_to_process))
      if (length(missing_source_cols) > 0) {
        msg <- paste("The following required source columns (from var_map values) are missing from the data to be prepared:",
                     paste(missing_source_cols, collapse = ", "))
        stop(msg)
      }
      
      # Create the new prepared_data table by selecting and renaming
      prepared_data <- data.table::data.table()
      for (std_name in names(self$var_map)) {
        original_col_name <- self$var_map[[std_name]]
        # This check should be redundant due to missing_source_cols check above, but good for safety
        if (original_col_name %in% names(current_data_to_process)) { 
          prepared_data[, (std_name) := current_data_to_process[[original_col_name]]]
        } else { 
          # Should not happen if previous check is thorough
          stop(paste0("Critical error: Column '", original_col_name, "' not found during renaming to '", std_name, "'."))
        }
      }
      
      # Standardize 'year' if 'time' variable is now in prepared_data (i.e., was mapped)
      if ("time" %in% names(prepared_data)) {
        if (is.numeric(prepared_data$time)) {
          if (nrow(prepared_data) > 0 && length(unique(prepared_data$time)) > 1) {
            mean_year <- mean(prepared_data$time, na.rm = TRUE)
            sd_year <- stats::sd(prepared_data$time, na.rm = TRUE)
            if (!is.na(sd_year) && sd_year > 0) {
              prepared_data[, year_std := (time - mean_year) / sd_year]
            } else { # sd_year is 0 or NA (e.g. only one unique value after NAs removed, or all values NA)
              prepared_data[, year_std := (time - mean_year)] # Fallback: center only
              # logger::log_warn("'time' variable has zero standard deviation or is NA after NAs removed. 'year_std' centered but not scaled.")
            }
          } else if (nrow(prepared_data) > 0 && length(unique(prepared_data$time)) == 1) {
            prepared_data[, year_std := (time - mean(prepared_data$time, na.rm = TRUE))] # Center only
            # logger::log_warn("Only one unique value in 'time' variable. 'year_std' centered but not scaled.")
          } else if (nrow(prepared_data) == 0) {
            prepared_data[, year_std := numeric(0)] # Empty data, empty column
          } else { # All time values might be NA
            prepared_data[, year_std := NA_real_]
            # logger::log_warn("Could not standardize 'time'; it may contain all NAs or be empty.")
          }
        } else {
          # logger::log_warn("'time' variable is not numeric. Cannot create 'year_std'.")
          prepared_data[, year_std := NA_real_] # Add column as NA if time is not numeric
        }
      }

      # Create factor versions for common categorical variables if they are in prepared_data
      if ("region" %in% names(prepared_data)) {
        prepared_data[, region_id := as.factor(region)]
      }
      if ("district" %in% names(prepared_data)) {
        prepared_data[, district_id := as.factor(district)]
      }
      if ("age" %in% names(prepared_data)) {
        prepared_data[, age_factor := factor(age, levels = unique(age), ordered = FALSE)]
      }
      
      # logger::log_info("Model data prepared successfully.")
      return(prepared_data)
    }
  )
)
```
