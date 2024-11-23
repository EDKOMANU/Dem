#' Calculate Child, Infant, and Neonatal Mortality
#'
#' This function calculates Child Mortality Rate, Infant Mortality Rate, and Neonatal Mortality Rate.
#'
#' @param data A dataframe containing demographic data.
#' @param age_col The column name for age groups.
#' @param deaths_col The column name for total deaths.
#' @param live_births_col The column name for live births.
#' @param infant_deaths_col The column name for infant deaths.
#' @param neonatal_deaths_col The column name for neonatal deaths.
#' @return A list with Child Mortality, Infant Mortality, and Neonatal Mortality.
#' @examples
#' demo_data <- data.frame(
#'   age = c("0-4", "5-9", "10-14"),
#'   infant_deaths = c(20, 15, 10),
#'   neonatal_deaths = c(5, 4, 3),
#'   total_deaths = c(100, 50, 30),
#'   live_births = c(2000, 2500, 2400)
#' )
#' dem.chm(demo_data, age_col = "age", deaths_col = "total_deaths",
#'                           live_births_col = "live_births",
#'                           infant_deaths_col = "infant_deaths",
#'                           neonatal_deaths_col = "neonatal_deaths")
#' @export
dem.chm <- function(data, age_col, deaths_col, live_births_col,
                                      infant_deaths_col, neonatal_deaths_col) {
  # Validate inputs
  if (!is.data.frame(data)) stop("Input 'data' must be a dataframe.")
  if (!all(c(deaths_col, live_births_col, infant_deaths_col, neonatal_deaths_col) %in% colnames(data))) {
    stop("Specified columns not found in the dataframe.")
  }

  # Calculate Child Mortality Rate (under 5 years)
  child_mortality <- (sum(data[[deaths_col]]) / sum(data[[live_births_col]])) * 1000
  # Calculate Infant Mortality Rate (under 1 year)
  infant_mortality <- (sum(data[[infant_deaths_col]]) / sum(data[[live_births_col]])) * 1000
  # Calculate Neonatal Mortality Rate (under 28 days)
  neonatal_mortality <- (sum(data[[neonatal_deaths_col]]) / sum(data[[live_births_col]])) * 1000

  results <- list(
    Child_Mortality = child_mortality,
    Infant_Mortality = infant_mortality,
    Neonatal_Mortality = neonatal_mortality
  )

  cat("Child Mortality Rate (under 5 years):\n")
  print(child_mortality)

  cat("Infant Mortality Rate (under 1 year):\n")
  print(infant_mortality)

  cat("Neonatal Mortality Rate (under 28 days):\n")
  print(neonatal_mortality)

  return(results)
}
