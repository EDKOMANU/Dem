#' Compute Life Table Using Age and nqx
#'
#' This function computes a life table from a data frame containing age groups and
#' probabilities of dying (\code{nqx}). It calculates key life table components
#' such as \code{lx}, \code{dx}, \code{Lx}, \code{Tx}, and \code{ex}.
#'
#' @param data A data frame containing age and nqx columns.
#' @param age A string specifying the column name for age groups.
#' @param nqx A string specifying the column name for probabilities of dying (\code{nqx}).
#' @param l0 Initial population at age 0 (default = 100,000).
#'
#' @return A data frame with the computed life table, including:
#'   \describe{
#'     \item{\code{Age}}{Age groups.}
#'     \item{\code{nqx}}{Probability of dying between age \code{x} and \code{x+n}.}
#'     \item{\code{lx}}{Number of survivors at the beginning of each age group.}
#'     \item{\code{dx}}{Number of deaths within each age group.}
#'     \item{\code{Lx}}{Person-years lived within each age group.}
#'     \item{\code{Tx}}{Cumulative person-years lived above a specific age group.}
#'     \item{\code{ex}}{Life expectancy at the beginning of each age group.}
#'   }
#'
#' @examples
#' # Example data
#' data <- data.frame(
#'   age = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
#'   nqx = c(0.05, 0.01, 0.005, 0.002, 0.003, 0.004, 0.005, 0.007, 0.010, 0.015,
#'           0.020, 0.030, 0.050, 0.070, 0.100, 0.150, 0.200, 0.300, 1.000)
#' )
#' life_table <- lifetable_nqx(data, age = "age", nqx = "nqx")
#' print(life_table)
#'
#' @export
lifetable_nqx <- function(data, age, nqx, l0 = 100000) {
  # Validate input
  if (!is.data.frame(data)) stop("Input must be a data frame.")
  if (!age %in% colnames(data) || !nqx %in% colnames(data)) {
    stop("Specified columns for age and nqx must exist in the data frame.")
  }

  # Extract age and nqx
  age <- data[[age]]
  nqx <- data[[nqx]]

  if (!is.numeric(age) || !is.numeric(nqx)) stop("Specified age and nqx columns must be numeric.")
  if (any(nqx < 0 | nqx > 1)) stop("nqx values must be between 0 and 1.")

  # Initialize life table components
  n <- length(age)
  lx <- numeric(n)
  dx <- numeric(n)
  Lx <- numeric(n)
  Tx <- numeric(n)
  ex <- numeric(n)

  # Compute lx and dx
  lx[1] <- l0
  for (i in 1:(n-1)) {
    dx[i] <- lx[i] * nqx[i]
    lx[i + 1] <- lx[i] - dx[i]
  }
  dx[n] <- lx[n] # Deaths in the open-ended last age group

  # Compute Lx
  for (i in 1:(n-1)) {
    Lx[i] <- lx[i] - (dx[i] / 2) # Midpoint approximation
  }
  Lx[n] <- lx[n] * log10(lx[n]) # Open-ended group using lx * log10(lx)

  # Compute Tx
  Tx[n] <- Lx[n]
  for (i in (n-1):1) {
    Tx[i] <- Tx[i + 1] + Lx[i]
  }

  # Compute ex
  ex <- Tx / lx

  # Return a data frame
  life_table <- data.frame(
    Age = age,
    nqx = nqx,
    lx = lx,
    dx = dx,
    Lx = Lx,
    Tx = Tx,
    ex = ex
  )

  return(life_table)
}
