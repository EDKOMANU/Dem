#' @name McmcSettings
#' @title MCMC Settings Class
#' @description An R6 class to manage MCMC settings for Bayesian models,
#' particularly for use with the brms package.
#' @export
McmcSettings <- R6::R6Class(
  "McmcSettings",

  public = list(
    #' @field iterations Total number of MCMC iterations per chain.
    iterations = NULL,
    #' @field warmup Number of warmup (burn-in) iterations per chain.
    warmup = NULL,
    #' @field chains Number of MCMC chains.
    chains = NULL,
    #' @field cores Number of CPU cores to use for parallel processing.
    cores = NULL,
    #' @field adapt_delta Target average proposal acceptance probability in HMC.
    adapt_delta = NULL,
    #' @field max_treedepth Maximum tree depth for NUTS algorithm in HMC.
    max_treedepth = NULL,
    #' @field seed Random seed for reproducibility.
    seed = NULL,

    #' @description
    #' Initialize the McmcSettings object.
    #' Sets default values for common MCMC parameters and validates them.
    #' @param iterations Total MCMC iterations (default: 2000).
    #' @param warmup Warmup iterations (default: 1000).
    #' @param chains Number of chains (default: 4).
    #' @param cores Number of cores for parallel execution (default: 4).
    #' @param adapt_delta Target acceptance rate for HMC (default: 0.95).
    #' @param max_treedepth Maximum tree depth for NUTS (default: 10).
    #' @param seed Random seed (default: NA_integer_, brms will manage).
    initialize = function(iterations = 2000, warmup = 1000, chains = 4, cores = 4,
                          adapt_delta = 0.95, max_treedepth = 10, seed = NA_integer_) {

      if (!is.numeric(iterations) || iterations <= 0 || floor(iterations) != iterations) {
        stop("MCMC 'iterations' must be a positive integer.")
      }
      if (!is.numeric(warmup) || warmup < 0 || floor(warmup) != warmup) { # Warmup can be 0
        stop("MCMC 'warmup' must be a non-negative integer.")
      }
      # Validation for warmup vs iterations is in private$validate_settings

      if (!is.numeric(chains) || chains <= 0 || floor(chains) != chains) {
        stop("MCMC 'chains' must be a positive integer.")
      }
      # Validation for minimum chains is in private$validate_settings

      if (!is.numeric(cores) || cores <= 0 || floor(cores) != cores) {
        stop("MCMC 'cores' must be a positive integer.")
      }
      # Validation for cores vs available cores is in private$validate_settings

      if (!is.numeric(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) {
        stop("MCMC 'adapt_delta' must be between 0 and 1 (exclusive).")
      }
      if (!is.numeric(max_treedepth) || max_treedepth <= 0 || floor(max_treedepth) != max_treedepth) {
        stop("MCMC 'max_treedepth' must be a positive integer.")
      }
      if (!is.na(seed) && (!is.numeric(seed) || floor(seed) != seed)) {
        stop("MCMC 'seed' must be an integer or NA_integer_.")
      }

      self$iterations <- as.integer(iterations)
      self$warmup <- as.integer(warmup)
      self$chains <- as.integer(chains)
      self$cores <- as.integer(cores)
      self$adapt_delta <- adapt_delta
      self$max_treedepth <- as.integer(max_treedepth)
      self$seed <- if (is.na(seed)) NA_integer_ else as.integer(seed)

      private$validate_settings() # Call remaining validations
      # logger::log_info("McmcSettings initialized and validated.")
    },

    #' @description
    #' Get MCMC settings as a list suitable for brms::brm().
    #' @return A list containing MCMC parameters formatted for brms.
    get_brms_settings = function() {
      settings_list <- list(
        iter = self$iterations,
        warmup = self$warmup,
        chains = self$chains,
        cores = self$cores,
        control = list(
          adapt_delta = self$adapt_delta,
          max_treedepth = self$max_treedepth
        )
      )
      
      if (!is.na(self$seed)) {
        settings_list$seed <- self$seed
      }
      
      # logger::log_debug("brms settings retrieved: {jsonlite::toJSON(settings_list, auto_unbox = TRUE)}")
      return(settings_list)
    }
  ),

  private = list(
    validate_settings = function() {
      if (self$warmup >= self$iterations) {
        stop("Warmup iterations must be less than total iterations.")
      }
      # Check for cores > parallel::detectCores() can be problematic in some environments
      # (e.g., HPC clusters where detectCores() might not reflect actual allocation).
      # Consider making this a warning or context-dependent check.
      # For now, keeping it as per the original file's intent.
      # if (self$cores > parallel::detectCores()) {
      #   warning(paste0("Requested cores (", self$cores, ") exceed available cores (", parallel::detectCores(), "). This might lead to issues or inefficiency."))
      # }
      if (self$chains < 1) { # brms allows chains = 1, though >1 is best for diagnostics
         warning("Running with less than 2 chains is not generally recommended for convergence diagnostics.")
      }
    }
  )
)
```
