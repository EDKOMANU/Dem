#' Calculate CDR and ASDR
#'
#' This function calculates the Crude Death Rate (CDR) and Age-Specific Death Rates (ASDR).
#'
#' @param data A data frame containing demographic data.
#' @param type A string or vector specifying the type(s) of death rate calculation.
#'   Options: "CDR", "ASDR". Use "all" to calculate both.
#' @param age_col The column name for age groups (used for "ASDR").
#' @param population_col The column name for total population.
#' @param deaths_col The column name for total deaths.
#'
#' @return A list of named outputs for the requested death rate calculations.
#' @examples
#' data <- data.frame(
#'   age = c("0-4", "5-9", "10-14"),
#'   population = c(10000, 12000, 11000),
#'   deaths = c(100, 50, 30)
#' )
#' dem.cdr(data, type = "all", age_col = "age",
#'                     population_col = "population", deaths_col = "deaths")
#' @export
dem.cdr <- function(data, type, age_col = NULL, population_col, deaths_col) {
  # Validate inputs
  if (!is.data.frame(data)) stop("Input 'data' must be a dataframe.")
  if (is.character(type) && type == "all") type <- c("CDR", "ASDR")
  if (!all(type %in% c("CDR", "ASDR"))) stop("Invalid 'type'. Choose from 'CDR' or 'ASDR'.")
  if (!all(c(population_col, deaths_col) %in% colnames(data))) stop("Specified columns not found in the dataframe.")
  if (any(type == "ASDR") && is.null(age_col)) stop("'age_col' must be specified for ASDR.")

  # Initialize result list
  results <- list()

  # Calculate CDR
  if ("CDR" %in% type) {
    cdr <- (sum(data[[deaths_col]]) / sum(data[[population_col]])) * 1000
    results$CDR <- cdr
    cat("Crude Death Rate (CDR):\n")
    print(cdr)
  }

  # Calculate ASDR
  if ("ASDR" %in% type) {
    asdr <- (data[[deaths_col]] / data[[population_col]]) * 1000
    data$ASDR <- asdr  # Add ASDR to the dataframe
    results$ASDR <- asdr
    cat("Age-Specific Death Rate (ASDR):\n")
    print(asdr)
  }

  return(list(results = results, modified_data = data))
}



