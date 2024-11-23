#' Calculate Maternal Mortality Metrics
#'
#' This function calculates the Maternal Mortality Rate (MMR) and Maternal Mortality Ratio (MMR).
#'
#' @param data A dataframe containing demographic data.
#' @param deaths_col The column name for maternal deaths.
#' @param births_col The column name for live births.
#' @param pop_women The column containing the number of women in the reporductive age
#'
#' @return A list with Maternal Mortality Rate (MMR) and Maternal Mortality Ratio (MMR).
#' @examples
#' demo_data <- data.frame(
#'   age=c("15-24", "25-34", "35-44"),
#'   maternal_deaths = c(5, 10, 15),
#'   live_births = c(5000, 7000, 6000),
#'   women =c(20000, 22000, 32000)
#' )
#' dem.mmr(demo_data, deaths_col = "maternal_deaths", births_col = "live_births", pop_women = "women")
#' @export
dem.mmr <- function(data, deaths_col, births_col, pop_women) {
  # Validate inputs
  if (!is.data.frame(data)) stop("Input 'data' must be a dataframe.")
  if (!all(c(deaths_col, births_col, pop_women) %in% colnames(data))) stop("Specified columns not found in the dataframe.")

  # Calculate Maternal Mortality Rate (MMR)
  data$mm_rate <- (data[[deaths_col]] / data[[pop_women]]) * 100000
  mm_rate <- (sum(data[[deaths_col]]) / sum(data[[pop_women]])) * 100000
  # Calculate Maternal Mortality Ratio (MMR)
  data$mm_ratio <- (data[[deaths_col]] / data[[births_col]]) * 100000
  mm_ratio <- sum(data[[deaths_col]]) / sum(data[[births_col]]) * 100000
  results <- list(MMR = mm_rate, MMR_Ratio = mm_ratio)

  cat("Maternal Mortality Rate (MMR)  per 100,000 women:\n")
  print(mm_rate)

  cat("Maternal Mortality Ratio (MMR)  per 100,000 live births:\n")
  print(mm_ratio)
  print(data)

}

