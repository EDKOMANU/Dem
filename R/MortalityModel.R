#' Mortality Model Class
#' @description R6 Class for modeling mortality rates using Bayesian methods
#' @export
MortalityModel <- R6::R6Class(
  "MortalityModel",
  inherit = DemographicModelBase,

  public = list(
    #' @field model Fitted brms model for mortality
    model = NULL,
    #' @field mcmc_settings MCMC settings for model fitting
    mcmc_settings = NULL,

    #' @description Initialize the mortality model
    initialize = function(data, variable_mapping, mcmc_settings = NULL) {
      super$initialize(data = data, variable_mapping = variable_mapping)
      self$mcmc_settings <- if (is.null(mcmc_settings)) McmcSettings$new() else mcmc_settings
      private$validate_mortality_data()
      private$init_timestamp <- "2025-03-11 18:48:47"
      private$init_user <- "EDKOMANU"
      log_info("Mortality model initialized at: {private$init_timestamp} by {private$init_user}")
    },

    #' @description Fit the mortality model
    fit = function(formula = NULL) {
      log_info("Fitting mortality model...")

      if (is.null(formula)) {
        formula <- bf(
          log_mortality ~ 1 + year_std + age_factor + (1 | region_id) + (1 | district_id),
          family = gaussian()
        )
      }

      model_data <- private$prepare_mortality_data()

      tryCatch({
        settings <- self$mcmc_settings$get_brms_settings()

        priors <- c(
          prior(normal(0, 10), class = "b"),
          prior(normal(0, 2), class = "Intercept"),
          prior(exponential(1), class = "sd"),
          prior(exponential(1), class = "sigma")
        )

        self$model <- do.call(brm,
          c(list(
            formula = formula,
            data = model_data,
            backend = "cmdstanr",
            prior = priors
          ), settings)
        )

        private$model_timestamp <- "2025-03-11 18:48:47"
        log_info("Mortality model fitted successfully.")
        private$log_convergence_diagnostics()
      }, error = function(e) {
        log_error("Mortality model error: {conditionMessage(e)}")
        stop(e)
      })
    },

    #' @description Predict mortality rates
    predict_mortality = function(newdata) {
      if (is.null(self$model)) {
        stop("Model must be fitted before prediction")
      }

      pred_data <- private$prepare_mortality_data(newdata)
      predictions <- posterior_predict(self$model, newdata = pred_data, draws = 1)
      return(exp(predictions[1, ]))
    }
  ),

  private = list(
    init_timestamp = NULL,
    init_user = NULL,
    model_timestamp = NULL,

    validate_mortality_data = function() {
      if (!self$var_map$deaths %in% names(self$data)) {
        stop("Deaths variable must be present in the data")
      }
    },

    prepare_mortality_data = function(data = NULL) {
      model_data <- super$prepare_model_data(data)

      model_data[, `:=`(
        mortality_rate = get(self$var_map$deaths) / get(self$var_map$population),
        log_mortality = log((get(self$var_map$deaths) + 1) / get(self$var_map$population))
      )]

      return(model_data)
    },

    log_convergence_diagnostics = function() {
      if (is.null(self$model)) return()

      rhat <- rhat(self$model)
      neff <- neff_ratio(self$model)

      log_info("Mortality Model Diagnostics:")
      log_info("- Rhat range: [{min(rhat)}, {max(rhat)}]")
      log_info("- Effective sample size ratios: [{min(neff)}, {max(neff)}]")

      if (any(rhat > 1.05)) {
        log_warn("Some mortality parameters show convergence issues (Rhat > 1.05)")
      }
      if (any(neff < 0.1)) {
        log_warn("Low effective sample sizes in mortality model")
      }
    }
  )
)
