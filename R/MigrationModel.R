#' @name MigrationModel
#' @title Placeholder Migration Model for Shiny App
#' @description R6 Class for a placeholder migration model. Inherits from DemographicModelBase.
#' This version has model fitting disabled and uses a heuristic for predictions.
#' @export
MigrationModel <- R6::R6Class(
  "MigrationModel",
  inherit = DemographicModelBase,

  public = list(
    #' @field in_model Placeholder for a fitted in-migration model object. NULL in this version.
    in_model = NULL,
    #' @field out_model Placeholder for a fitted out-migration model object. NULL in this version.
    out_model = NULL,
    #' @field mcmc_settings An instance of McmcSettings.
    mcmc_settings = NULL,

    #' @description
    #' Initialize the MigrationModel object.
    #' @param data A data frame or data.table containing demographic data.
    #' @param variable_mapping A list mapping standard variable names to actual column names.
    #' Must include mappings for 'in_migration' and 'out_migration'.
    #' @param mcmc_settings Optional. An existing McmcSettings object. If NULL, a new one
    #' with default settings is created.
    initialize = function(data, variable_mapping, mcmc_settings = NULL) {
      super$initialize(data = data, variable_mapping = variable_mapping)

      if (is.null(mcmc_settings)) {
        self$mcmc_settings <- McmcSettings$new()
      } else {
        if (!inherits(mcmc_settings, "McmcSettings")) {
          stop("Provided mcmc_settings is not an McmcSettings object.")
        }
        self$mcmc_settings <- mcmc_settings
      }

      private$validate_migration_data()

      private$init_timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      private$init_user <- Sys.getenv("USER", unset = "UnknownUser")

      # logger::log_info("MigrationModel (Placeholder) initialized by {private$init_user} at {private$init_timestamp}.")
    },

    #' @description
    #' Placeholder for model fitting. Fitting is disabled in this version.
    #' @param in_formula Optional. Formula for in-migration model (ignored).
    #' @param out_formula Optional. Formula for out-migration model (ignored).
    fit = function(in_formula = NULL, out_formula = NULL) {
      message("Model fitting is disabled for this placeholder MigrationModel version.")
      # logger::log_warn("Attempted to call fit() on a placeholder MigrationModel. Fitting is disabled.")
      invisible(NULL)
    },

    #' @description
    #' Predict in-migration and out-migration counts using a heuristic approach.
    #' This method does not use a statistical model for prediction.
    #' @param newdata A data frame or data.table with new data for prediction.
    #' @return A list containing two named numeric vectors:
    #'         `in_migration`: plausible predicted in-migration counts.
    #'         `out_migration`: plausible predicted out-migration counts.
    #'         Each vector corresponds to the rows in `newdata`.
    predict_migration = function(newdata) {
      if (is.null(newdata)) {
        stop("newdata must be provided for prediction.")
      }

      prepared_nd <- private$prepare_migration_data(newdata)
      n_rows <- nrow(prepared_nd)

      if (n_rows == 0) {
        return(list(in_migration = numeric(0), out_migration = numeric(0)))
      }

      predicted_in_migration <- numeric(n_rows)
      predicted_out_migration <- numeric(n_rows)

      # Heuristic:
      # If 'population' is available and numeric, use a small percentage for migration counts.
      # Otherwise, generate small random plausible numbers.

      if ("population" %in% names(prepared_nd) && is.numeric(prepared_nd$population) && all(!is.na(prepared_nd$population))) {
        # Generate in-migration as a small percentage of population (e.g., 0.1% to 2%)
        in_rate_per_row <- runif(n_rows, min = 0.001, max = 0.020) # 0.1% to 2.0%
        predicted_in_migration <- prepared_nd$population * in_rate_per_row

        # Generate out-migration similarly
        out_rate_per_row <- runif(n_rows, min = 0.001, max = 0.020)
        predicted_out_migration <- prepared_nd$population * out_rate_per_row

        # logger::log_info("Generated {n_rows} migration count predictions using population-based heuristic.")
      } else {
        # Fallback if population data is not usable or not present
        max_random_migrants <- 10 # Max random migrants per group
        predicted_in_migration <- runif(n_rows, min = 0, max = max_random_migrants)
        predicted_out_migration <- runif(n_rows, min = 0, max = max_random_migrants)
        # logger::log_warn("Population data not found, not numeric, or contains NAs in newdata for predict_migration. Using random fallback for {n_rows} predictions.")
      }

      # Ensure non-negativity. Projector handles rounding and ensuring out_migration <= population.
      predicted_in_migration <- pmax(0, predicted_in_migration)
      predicted_out_migration <- pmax(0, predicted_out_migration)

      return(list(
        in_migration = as.numeric(predicted_in_migration),
        out_migration = as.numeric(predicted_out_migration)
      ))
    }
  ),

  private = list(
    init_timestamp = NULL,
    init_user = NULL,

    #' @description
    #' Validate migration-specific aspects of the input data.
    #' Checks for 'in_migration' and 'out_migration' columns as defined in var_map.
    validate_migration_data = function() {
      required_components <- c("in_migration", "out_migration")
      for (comp in required_components) {
        if (is.null(self$var_map[[comp]])) {
          stop(paste0("Variable mapping for '", comp, "' is missing."))
        }
        if (!(self$var_map[[comp]] %in% names(self$data))) {
          stop(paste0("The '", comp, "' column '", self$var_map[[comp]],
                      "' (as specified in variable_mapping) is not found in the input data."))
        }
        # Optional: Check if columns are numeric and non-negative in self$data
        # if (!is.numeric(self$data[[self$var_map[[comp]]]]) || any(self$data[[self$var_map[[comp]]]] < 0, na.rm = TRUE)) {
        #   logger::log_warn(paste0("The '", comp, "' column in the input data contains non-numeric or negative values."))
        # }
      }
    },

    #' @description
    #' Prepare migration data using the base class method.
    #' Can be extended for migration-specific transformations if needed.
    #' @param data Optional. Data to prepare. If NULL, uses `self$data`.
    #' @return A data.table prepared for migration modeling/prediction.
    prepare_migration_data = function(data = NULL) {
      prepared_data <- super$prepare_model_data(data = data)

      # Ensure 'population' column is numeric if it exists, as the heuristic relies on it.
      if ("population" %in% names(prepared_data) && !is.numeric(prepared_data$population)) {
        # logger::log_warn("Population column was not numeric in prepare_migration_data, attempting coercion.")
        original_pop_class <- class(prepared_data$population)
        prepared_data[, population := as.numeric(as.character(population))] # as.character for factors
        if(any(is.na(prepared_data$population)) && !all(is.na(original_pop_class))) {
            # logger::log_warn("NAs introduced in population column after coercion from ", original_pop_class ," to numeric.")
        }
      }
      return(prepared_data)
    }
  )
)

