#' Calculate Fertility Metrics
#'
#' This function calculates various fertility metrics (CBR, GFR, ASFR, TFR) based on user input
#' and a dataframe with demographic data.
#'
#' @param data A dataframe containing demographic data.
#' @param type A string or vector specifying the type(s) of fertility calculation.
#'   Options: "CBR", "GFR", "ASFR", "TFR". Use "all" to calculate all metrics.
#' @param age_col The column name for age groups (used for "ASFR" and "TFR").
#' @param population_col The column name for total population.
#' @param women_col The column name for number of women (used for "GFR", "ASFR", and "TFR").
#' @param births_col The column name for live births.
#' @return A list of named outputs for the requested fertility calculations and the modified dataframe with ASFR.
#' @examples
#' demo_data <- data.frame(
#'   age = c("15-19", "20-24", "25-29"),
#'   population = c(10000, 12000, 11000),
#'   women = c(5000, 6000, 5500),
#'   live_births = c(200, 400, 300)
#' )
#
#' # Calculate ASFR
#' dm.fert(demo_data, type = "ASFR", age_col = "age",
#'                     population_col = "population", women_col = "women", births_col = "live_births")
#' # Calculate all metrics
#' dem.fert(demo_data, type = "all", age_col = "age",
#'                     population_col = "population", women_col = "women", births_col = "live_births")
#' @export
dm.fert <- function(data, type, age_col = NULL, population_col, women_col = NULL, births_col) {
  # Validate inputs
  if (!is.data.frame(data)) stop("Input 'data' must be a dataframe.")
  if (is.character(type) && type == "all") type <- c("CBR", "GFR", "ASFR", "TFR")
  if (!all(type %in% c("CBR", "GFR", "ASFR", "TFR"))) stop("Invalid 'type'. Choose from 'CBR', 'GFR', 'ASFR', or 'TFR'.")
  if (!all(c(population_col, births_col) %in% colnames(data))) stop("Specified columns not found in the dataframe.")
  if (any(type %in% c("GFR", "ASFR", "TFR")) && is.null(women_col)) stop("'women_col' must be specified for GFR, ASFR, or TFR.")
  if (any(type %in% c("ASFR", "TFR")) && is.null(age_col)) stop("'age_col' must be specified for ASFR or TFR.")

  # Initialize result list
  results <- list()

  # Calculate metrics based on type
  if ("CBR" %in% type) {
    cbr <- (sum(data[[births_col]]) / sum(data[[population_col]])) * 1000
    results$CBR <- cbr
    cat("Crude Birth Rate (CBR):\n")
    print(cbr)
  }
  if ("GFR" %in% type) {
    gfr <- (sum(data[[births_col]]) / sum(data[[women_col]])) * 1000
    results$GFR <- gfr
    cat("General Fertility Rate (GFR):\n")
    print(gfr)
  }
  if ("ASFR" %in% type || "TFR" %in% type) {
    asfr <- (data[[births_col]] / data[[women_col]]) * 1000
    data$ASFR <- asfr  # Add ASFR to the dataframe
    results$ASFR <- asfr
    cat("Age-Specific Fertility Rate (ASFR):\n")
    print(asfr)
  }
  if ("TFR" %in% type) {
    # TFR is the sum of ASFRs, assuming age intervals of 5 years
    if (!"ASFR" %in% names(results)) {
      asfr <- (data[[births_col]] / data[[women_col]]) * 1000
      results$ASFR <- asfr
    }
    tfr <- sum(results$ASFR, na.rm = TRUE) * 5 / 1000
    results$TFR <- tfr
    cat("Total Fertility Rate (TFR):\n")
    print(tfr)
  }

  return(list(results = results, modified_data = data))
}
