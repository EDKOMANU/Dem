#' MCMC Settings Class
#' @description Helper class to manage MCMC parameters consistently across models
#' @export
McmcSettings <- R6::R6Class(
  "McmcSettings",

  public = list(
    #' @field iterations Number of total iterations per chain
    iterations = 2000,
    #' @field warmup Number of warmup iterations
    warmup = 1000,
    #' @field chains Number of Markov chains
    chains = 4,
    #' @field cores Number of cores for parallel processing
    cores = 4,
    #' @field adapt_delta Target acceptance rate
    adapt_delta = 0.999,
    #' @field max_treedepth Maximum tree depth for NUTS sampler
    max_treedepth = 15,
    #' @field seed Random seed for reproducibility
    seed = 123,

    #' @description Initialize MCMC settings
    initialize = function(iterations = 2000, warmup = 1000, chains = 4, cores = 4,
                        adapt_delta = 0.999, max_treedepth = 15, seed = 123) {
      self$iterations <- iterations
      self$warmup <- warmup
      self$chains <- chains
      self$cores <- cores
      self$adapt_delta <- adapt_delta
      self$max_treedepth <- max_treedepth
      self$seed <- seed
      private$validate_settings()
    },

    #' @description Get settings as list for brms
    get_brms_settings = function() {
      list(
        iter = self$iterations,
        warmup = self$warmup,
        chains = self$chains,
        cores = self$cores,
        control = list(
          adapt_delta = self$adapt_delta,
          max_treedepth = self$max_treedepth
        ),
        seed = self$seed
      )
    }
  ),

  private = list(
    validate_settings = function() {
      if (self$warmup >= self$iterations)
        stop("Warmup must be less than total iterations")
      if (self$cores > parallel::detectCores())
        stop("Requested cores exceed available cores")
      if (self$chains < 2)
        stop("At least 2 chains recommended for convergence diagnostics")
    }
  )
)
