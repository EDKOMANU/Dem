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
#' project_population(region2000 ,base_year = 2000,
#' TFR_var = "TFR",base_pop_var = "base_pop",
#' region_var = "Country", death_rate_var = "death_rate",
#' net_migration_var = "net_migration",,
#' subregion_var = "Region", future_year = 2009,
#' num_samples = 5000)
#'
#'
#' @export

project_population <- function(
    data, future_year, base_year,
    region_var = "region", subregion_var = "subregion",
    base_pop_var = "base_pop", TFR_var = "TFR",
    death_rate_var = "death_rate", net_migration_var = "net_migration",
    num_samples = 2000, random_seed = 42
) {
  set.seed(random_seed)

  num_years <- future_year - base_year
  # List to store simulation results for each region per year.
  # We will append data frames with columns: region, subregion, year, median_population
  results_list <- list()

  for (i in 1:nrow(data)) {
    row_data <- data[i, ]
    region <- as.character(row_data[[region_var]])
    subregion <- as.character(row_data[[subregion_var]])

    # starting population is provided in the data
    current_pop <- as.numeric(row_data[[base_pop_var]])

    # Other parameters that are assumed constant across years in this model
    TFR <- as.numeric(row_data[[TFR_var]])
    death_rate <- as.numeric(row_data[[death_rate_var]])
    net_migration <- as.numeric(row_data[[net_migration_var]])

    # Calculate migration effect based on the initial population (could be updated each year if desired)
    migration_effect <- net_migration / current_pop

    # Create a temporary data frame to store yearly medians for this region
    region_results <- data.frame(
      region = character(),
      subregion = character(),
      year = integer(),
      median_population = double(),
      stringsAsFactors = FALSE
    )

    # Add the base year as the starting point
    region_results <- rbind(region_results, data.frame(
      region = region,
      subregion = subregion,
      year = base_year,
      median_population = current_pop
    ))

    # Simulate year by year
    for (j in 1:num_years) {
      # For each year we simulate growth based on random parameters.
      # You may also add yearly variability to TFR, death_rate, or migration_effect if needed.
      beta0 <- rnorm(num_samples, mean = 0, sd = 0.01) # adjusted for a more realistic baseline
      beta_TFR <- rnorm(num_samples, mean = 0.006, sd = 0.02) # adjusted TFR impact
      beta_death <- rnorm(num_samples, mean = 0.01, sd = 0.2) # adjusted death rate impact
      beta_mig <- rnorm(num_samples, mean = 0.001, sd = 0.2) # adjusted migration impact

      # Calculate annual growth rate for this year
      growth_rate <- beta0 + beta_TFR * TFR - beta_death * death_rate + beta_mig * migration_effect

      # Additional noise to simulate variability
      noise <- rnorm(num_samples, mean = 0, sd = 0.02)
      annual_growth <- growth_rate + noise

      # Update the population for each simulation path using discrete exponential growth over one year
      simulation_population <- current_pop * exp(annual_growth)

      # Get the median population for this year simulation
      median_pop <- median(simulation_population)

      # For next year's simulation, we update current_pop to be the median of the current simulation.
      current_pop <- median_pop

      # Optionally, you can update migration_effect relative to new current_pop:
      migration_effect <- net_migration / current_pop

      # Year corresponding to this simulation step
      current_year <- base_year + j

      # Store the results for the current year
      region_results <- rbind(region_results, data.frame(
        region = region,
        subregion = subregion,
        year = current_year,
        median_population = median_pop
      ))
    }

    results_list[[i]] <- region_results
  }

  # Combine all region results
  results_df <- do.call(rbind, results_list)
  return(results_df)
}
