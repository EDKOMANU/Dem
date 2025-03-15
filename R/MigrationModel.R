#' Migration Model Class
#' @description R6 Class for modeling both in and out migration using Bayesian methods
#' @export
MigrationModel <- R6::R6Class(
  "MigrationModel",
  inherit = DemographicModelBase,

  public = list(
    #' @field in_model Fitted brms model for in-migration
    in_model = NULL,
    #' @field out_model Fitted brms model for out-migration
    out_model = NULL,
    #' @field mcmc_settings MCMC settings for model fitting
    mcmc_settings = NULL,

    #' @description Initialize the migration model
    initialize = function(data, variable_mapping, mcmc_settings = NULL) {
      super$initialize(data = data, variable_mapping = variable_mapping)
      self$mcmc_settings <- if (is.null(mcmc_settings)) McmcSettings$new() else mcmc_settings
      private$validate_migration_data()
      private$init_timestamp <- "2025-03-11 18:50:14"
      private$init_user <- "EDKOMANU"
      log_info("Migration model initialized at: {private$init_timestamp} by {private$init_user}")
    },

    #' @description Fit both in and out migration models
    fit = function(in_formula = NULL, out_formula = NULL) {
      log_info("Fitting migration models...")

      # Default formulas if not provided
      if (is.null(in_formula)) {
        in_formula <- bf(
          in_migration ~ 1 + year_std + age_factor + (1 | region_id) + (1 | district_id),
          family = negbinomial()
        )
      }

      if (is.null(out_formula)) {
        out_formula <- bf(
          out_migration ~ 1 + year_std + age_factor + (1 | region_id) + (1 | district_id),
          family = negbinomial()
        )
      }

      model_data <- private$prepare_migration_data()
      settings <- self$mcmc_settings$get_brms_settings()

      # Fit in-migration model
      tryCatch({
        log_info("Fitting in-migration model...")
        self$in_model <- do.call(brm,
          c(list(
            formula = in_formula,
            data = model_data,
            backend = "cmdstanr"
          ), settings)
        )
        private$in_model_timestamp <- "2025-03-11 18:50:14"
        log_info("In-migration model fitted successfully.")
        private$log_convergence_diagnostics(self$in_model, "in-migration")
      }, error = function(e) {
        log_error("In-migration model error: {conditionMessage(e)}")
        stop(e)
      })

      # Fit out-migration model with different seed
      tryCatch({
        log_info("Fitting out-migration model...")
        out_settings <- settings
        out_settings$seed <- settings$seed + 1

        self$out_model <- do.call(brm,
          c(list(
            formula = out_formula,
            data = model_data,
            backend = "cmdstanr"
          ), out_settings)
        )
        private$out_model_timestamp <- "2025-03-11 18:50:14"
        log_info("Out-migration model fitted successfully.")
        private$log_convergence_diagnostics(self$out_model, "out-migration")
      }, error = function(e) {
        log_error("Out-migration model error: {conditionMessage(e)}")
        stop(e)
      })
    },

    #' @description Predict migration flows
    #' @param newdata New data for prediction
    #' @param type Type of prediction ("in", "out", or "both")
    predict_migration = function(newdata, type = "both") {
      if (!type %in% c("in", "out", "both")) {
        stop("Type must be one of: 'in', 'out', 'both'")
      }

      if ((type %in% c("in", "both") && is.null(self$in_model)) ||
          (type %in% c("out", "both") && is.null(self$out_model))) {
        stop("Required models must be fitted before prediction")
      }

      pred_data <- private$prepare_migration_data(newdata)
      result <- list()

      if (type %in% c("in", "both")) {
        in_predictions <- posterior_predict(self$in_model, newdata = pred_data, draws = 1)
        result$in_migration <- pmin(round(in_predictions[1, ]),
                                  pred_data[[self$var_map$population]] * 0.1)
      }

      if (type %in% c("out", "both")) {
        out_predictions <- posterior_predict(self$out_model, newdata = pred_data, draws = 1)
        result$out_migration <- pmin(round(out_predictions[1, ]),
                                   pred_data[[self$var_map$population]] * 0.1)
      }

      return(result)
    },

    #' @description Calculate net migration
    calculate_net_migration = function(predictions) {
      if (!all(c("in_migration", "out_migration") %in% names(predictions))) {
        stop("Both in and out migration predictions required")
      }
      return(predictions$in_migration - predictions$out_migration)
    },

    #' @description Print model summaries
    print_summaries = function() {
      cat("\nMigration Models Summary\n")
      cat("=====================\n")
      cat(sprintf("Initialized: %s by %s\n", private$init_timestamp, private$init_user))

      if (!is.null(self$in_model)) {
        cat("\nIn-Migration Model Summary:\n")
        cat(sprintf("Fitted: %s\n", private$in_model_timestamp))
        print(summary(self$in_model))
      }

      if (!is.null(self$out_model)) {
        cat("\nOut-Migration Model Summary:\n")
        cat(sprintf("Fitted: %s\n", private$out_model_timestamp))
        print(summary(self$out_model))
      }
    }
  ),

  private = list(
    init_timestamp = NULL,
    init_user = NULL,
    in_model_timestamp = NULL,
    out_model_timestamp = NULL,

    validate_migration_data = function() {
      required_vars <- c("in_migration", "out_migration")
      missing_vars <- setdiff(required_vars, names(self$data))
      if (length(missing_vars) > 0) {
        stop(sprintf("Missing required migration variables: %s",
                    paste(missing_vars, collapse = ", ")))
      }

      # Validate migration values
      if (any(self$data$in_migration < 0) || any(self$data$out_migration < 0)) {
        stop("Migration values cannot be negative")
      }

      # Check for logical consistency
      if (any(self$data$out_migration > self$data[[self$var_map$population]])) {
        log_warn("Some out-migration values exceed population size")
      }
    },

    prepare_migration_data = function(data = NULL) {
      model_data <- super$prepare_model_data(data)

      model_data[, `:=`(
        migration_rate_in = in_migration / get(self$var_map$population),
        migration_rate_out = out_migration / get(self$var_map$population)
      )]

      return(model_data)
    },

    log_convergence_diagnostics = function(model, type) {
      if (is.null(model)) return()

      rhat <- rhat(model)
      neff <- neff_ratio(model)

      log_info("{type} Model Diagnostics:")
      log_info("- Rhat range: [{min(rhat)}, {max(rhat)}]")
      log_info("- Effective sample size ratios: [{min(neff)}, {max(neff)}]")

      if (any(rhat > 1.05)) {
        log_warn("Some {type} parameters show convergence issues (Rhat > 1.05)")
      }
      if (any(neff < 0.1)) {
        log_warn("Low effective sample sizes in {type} model")
      }
    }
  )
)
