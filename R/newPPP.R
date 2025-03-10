#' @name project_population
#' @title Basic Probabilistic Population Projection
#'
#' @description This function projects population trajectories for multiple administrative regions.
#'
#' Project Population for Multiple Regions Using a Bayesian-Inspired Simulation Approach
#'
#' This function projects population trajectories for multiple administrative regions
#' using a Bayesian-inspired simulation approach (without full Bayesian integration).
#' The function takes a data frame with demographic and migration parameters and
#' simulates multiple projection scenarios based on randomly drawn coefficients
#' from prior distributions and an exponential growth model.
#' @param data A data frame containing projection parameters.
#' @param future_year Numeric. The target year for the projection.
#' @param base_year Numeric. The base year for the projection.
#' @param region_var Character. Column name for the region.
#' @param subregion_var Character. Column name for the subregion.
#' @param base_pop_var Character. Column name for the base population.
#' @param TFR_var Character. Column name for the Total Fertility Rate (TFR).
#' @param death_rate_var Character. Column name for the death rate.
#' @param net_migration_var Character. Column name for net migration.
#' @param num_samples Integer. Number of simulation samples (default: 2000).
#' @param random_seed Integer. Random seed for reproducibility (default: 42).
#'
#' @return A data frame containing the projected population summary statistics for each region,
#' including the 25th percentile (lower), mean, median, and 75th percentile (higher).
#'
#' @examples
#' sample_data <- data.frame(
#'   region = c("Northland", "Southshire", "Eastvale"),
#'   subregion = c("District A", "District B", "District C"),
#'   base_pop = c(50000, 75000, 60000),
#'   TFR = c(2.4, 2.3, 2.5),
#'   death_rate = c(0.012, 0.011, 0.013),
#'   net_migration = c(200, 300, 250)
#' )
#'
#' results <- project_population(
#'   data = sample_data,
#'   future_year = 2032,
#'   base_year = 2020,
#'   region_var = "region",
#'   subregion_var = "subregion",
#'   base_pop_var = "base_pop",
#'   TFR_var = "TFR",
#'   death_rate_var = "death_rate",
#'   net_migration_var = "net_migration",
#'   num_samples = 10000,
#'   random_seed = 42
#' )
#'
#' print(results)
#'
#' @export
project_population <- function(
    data, future_year, base_year, region_var = "region", subregion_var = "subregion",
    base_pop_var = "base_pop", TFR_var = "TFR", death_rate_var = "death_rate",
    net_migration_var = "net_migration", num_samples = 2000, random_seed = 42
) {
  set.seed(random_seed)

  years_ahead <- future_year - base_year
  results_list <- list()

  for (i in 1:nrow(data)) {
    row <- data[i, ]

    region <- as.character(row[[region_var]])
    subregion <- as.character(row[[subregion_var]])
    base_pop <- as.numeric(row[[base_pop_var]])
    TFR <- as.numeric(row[[TFR_var]])
    death_rate <- as.numeric(row[[death_rate_var]])
    net_migration <- as.numeric(row[[net_migration_var]])

    migration_effect <- net_migration / base_pop

    beta0 <- rnorm(num_samples, mean = 0, sd = 0.02)
    beta_TFR <- rnorm(num_samples, mean = 0.01, sd = 0.01)
    beta_death <- rnorm(num_samples, mean = 0.02, sd = 0.01)
    beta_mig <- rnorm(num_samples, mean = 0.05, sd = 0.02)

    growth_rate <- beta0 + beta_TFR * TFR - beta_death * death_rate + beta_mig * migration_effect
    sigma <- abs(rnorm(num_samples, mean = 0, sd = 0.02))
    noise <- rnorm(num_samples, mean = 0, sd = sigma)

    proj_population <- base_pop * exp(growth_rate * years_ahead + noise)

    lower <- quantile(proj_population, 0.25)
    mean_val <- mean(proj_population)
    median_val <- median(proj_population)
    higher <- quantile(proj_population, 0.75)

    results_list[[i]] <- data.frame(
      region = region,
      subregion = subregion,
      base_pop = base_pop,
      lower = lower,
      mean = mean_val,
      median = median_val,
      higher = higher
    )
  }

  results_df <- do.call(rbind, results_list)
  return(results_df)
}
