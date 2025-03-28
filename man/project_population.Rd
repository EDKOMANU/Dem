% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newPPP.R
\name{project_population}
\alias{project_population}
\title{Basic Probabilistic Population Projection}
\usage{
project_population(
  data,
  future_year,
  base_year,
  region_var = "region",
  subregion_var = "subregion",
  base_pop_var = "base_pop",
  TFR_var = "TFR",
  death_rate_var = "death_rate",
  net_migration_var = "net_migration",
  num_samples = 2000,
  random_seed = 42
)
}
\arguments{
\item{data}{A data frame containing projection parameters.}

\item{future_year}{Numeric. The target year for the projection.}

\item{base_year}{Numeric. The base year for the projection.}

\item{region_var}{Character. Column name for the region.}

\item{subregion_var}{Character. Column name for the subregion.}

\item{base_pop_var}{Character. Column name for the base population.}

\item{TFR_var}{Character. Column name for the Total Fertility Rate (TFR).}

\item{death_rate_var}{Character. Column name for the death rate.}

\item{net_migration_var}{Character. Column name for net migration.}

\item{num_samples}{Integer. Number of simulation samples (default: 2000).}

\item{random_seed}{Integer. Random seed for reproducibility (default: 42).}
}
\value{
A data frame containing the projected population summary statistics for each region,
including the 25th percentile (lower), mean, median, and 75th percentile (higher).
}
\description{
This function projects population trajectories for multiple administrative regions but the output is the median of the trajectories.

Project Population for Multiple Regions Using a Bayesian-Inspired Simulation Approach

This function projects population trajectories for multiple administrative regions
using a Bayesian-inspired simulation approach (without full Bayesian integration).
The function takes a data frame with demographic and migration parameters and
simulates multiple projection scenarios based on randomly drawn coefficients
from prior distributions and an exponential growth model.
}
\examples{
project_population(region2000 ,base_year = 2000,
TFR_var = "TFR",base_pop_var = "base_pop",
region_var = "Country", death_rate_var = "death_rate",
net_migration_var = "net_migration",,
subregion_var = "Region", future_year = 2009,
num_samples = 5000)


}
