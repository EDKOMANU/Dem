#' @name  FertilityModel
#' @title Fertility Model
#' @description R6 Class for modeling fertility rates using Bayesian methods
#' @details This class is used to fit a Bayesian model for fertility rates using the brms package.
#' The model is fitted to a dataset containing births and population data, and can be used to predict future fertility rates.
#' @param data Data frame containing births and population data for different regions and age groups
#' @examples
#' # example code
#' var_mapping <- list(
  #'   region = "region",
  #'   district = "district",
  #'   time = "year",
  #'   age = "age_group",
  #'   sex = "sex",
  #'   population = "population",
  #'   deaths = "deaths",
  #'   in_migration= "in_migration" ,
  #'   out_migration = "out_migration"
  #' )
  #'
  #' # Create MCMC settings
#' mcmc_settings <- McmcSettings$new(
#'  iterations = 1000,
#'  warmup = 500,
#'  chains = 4,
#'  cores = 4,
#'  adapt_delta = 0.99,
#'  max_treedepth = 20,
#'  seed = 42
#' )
#'
#' #initialise the model
#' fertility_model <- FertilityModel$new(data = testdata, variable_mapping = var_mapping, mcmc_settings = mcmc_settings)
#'
#' #fit the model using 
#' #fertility_model$fit()
#'
#' @export
FertilityModel <- R6::R6Class(
  "FertilityModel",
  inherit = DemographicModelBase,

  public = list(
    #' @field model Fitted brms model for fertility
    model = NULL,
    #' @field mcmc_settings markov chain monte carlo settings for model fitting and adjusting model performance
    mcmc_settings = NULL,

    #' @description Initialize the fertility model
    #' @param data Data frame containing births and population data
    #' @param variable_mapping Variable mapping for model variables
    #' @param mcmc_settings MCMC settings for model fitting
    initialize = function(data, variable_mapping, mcmc_settings = NULL) {
      super$initialize(data = data, variable_mapping = variable_mapping)
      self$mcmc_settings <- if (is.null(mcmc_settings)) McmcSettings$new() else mcmc_settings
      private$validate_fertility_data()
      private$init_timestamp <- "2025-03-11 18:48:47"
      private$init_user <- "EDKOMANU"
      logger::log_info("Fertility model initialized at: {private$init_timestamp} by {private$init_user}")
    },

    #' @description Fit the fertility model
    #' @param formula Beysian Formula for the model
    fit = function(formula = NULL) {
      logger::log_info("Fitting fertility model...")

      if (is.null(formula)) {
        formula <- brms::bf(
          births ~ 1 + year_std + age_id + (1 | region_id) + (1 | district_id) +
            offset(log_population),
          family = poisson()
        )
      }

      model_data <- private$prepare_fertility_data()

      tryCatch({
        settings <- self$mcmc_settings$get_brms_settings()

        self$model <- do.call(brms::brm,
          c(list(
            formula = formula,
            data = model_data,
            backend = "cmdstanr"
          ), settings)
        )

        private$model_timestamp <- "2025-03-11 18:48:47"
        logger::log_info("Fertility model fitted successfully.")
        private$log_convergence_diagnostics()
      }, error = function(e) {
        logger::log_error("Fertility model error: {conditionMessage(e)}")
        stop(e)
      })
    },

    #' @description Predict fertility rates
    #' @param newdata Data frame containing new data for prediction
    predict_fertility = function(newdata) {
      if (is.null(self$model)) {
        stop("Model must be fitted before prediction")
      }

      pred_data <- private$prepare_fertility_data(newdata)
      predictions <- brms::posterior_predict(self$model, newdata = pred_data, draws = 1)
      return(predictions[1, ])
    }
  ),

  private = list(
    init_timestamp = NULL,
    init_user = NULL,
    model_timestamp = NULL,

    validate_fertility_data = function() {
      if (!"births" %in% names(self$data)) {
        stop("Births variable must be present in the data")
      }
    },

    prepare_fertility_data = function(data = NULL) {
      model_data <- super$prepare_model_data(data)

      model_data[, `:=`(
        age_id = as.numeric(age_factor),
        log_population = log(get(self$var_map$population))
      )]

      return(model_data)
    },

    log_convergence_diagnostics = function() {
      if (is.null(self$model)) return()

      rhat <- brms::rhat(self$model)
      neff <- brms::neff_ratio(self$model)

      logger::log_info("Fertility Model Diagnostics:")
      logger::log_info("- Rhat range: [{min(rhat)}, {max(rhat)}]")
      logger::log_info("- Effective sample size ratios: [{min(neff)}, {max(neff)}]")

      if (any(rhat > 1.05)) {
        logger::log_warn("Some fertility parameters show convergence issues (Rhat > 1.05)")
      }
      if (any(neff < 0.1)) {
        logger::log_warn("Low effective sample sizes in fertility model")
      }
    }
  )
)
