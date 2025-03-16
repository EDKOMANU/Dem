#'@title lifetable_basic
#' @name Life Table Computations
#'
#' @description
#' This function computes a complete life table based on given mortality rates or raw data.
#' It takes a dataframe with the necessary inputs and computes standard life table metrics.
#' 
#' @param data A data frame containing age-specific mortality probabilities.
#' @param age Numeric vector: age groups to be presented in point format, e.g., 0, 1, 5, 10, ...
#' @param nMx Numeric vector (optional): the observed mortality rates in the population. If not provided, `pop` and `Dx` must be given to compute it.
#' @param pop Numeric vector (optional): total population in each age group. Used to compute `nMx` if not given.
#' @param Dx Numeric vector (optional): deaths occurred in each age group. Used to compute `nMx` if not given.
#'
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{`metrics`}{A list of the life expectncy at birth and the total deaths recorded}
#'   \item{`lifetable`}{A dataframe of the complete life table.}
#' }
#'
#' @examples
#'#Example without moratlity rate
#'data(gphc2010)


# Call the function
#'lifetable_basic(data=gphc2010, age = "Age", pop = "Pop", Dx = "Deaths")

#'
#'

#' @export
lifetable_basic <- function(data,
                           age = "Age",
                           nMx = NULL,
                           pop = NULL,
                           Dx = NULL
                           ) {
  # Check if required columns are present
  if (is.null(nMx) && (is.null(pop) || is.null(Dx))) {
    stop("If nMx is not provided, both pop and Dx must be specified.")
  }

  # Extract columns
  age <- data[[age]]
  nMx <- if (!is.null(nMx)) {
    data[[nMx]]
  } else {
    # Compute nMx if not provided
    data[[Dx]] / data[[pop]]
  }

  pop <- if (!is.null(pop)) data[[pop]] else NULL
  Dx <- if (!is.null(Dx)) data[[Dx]] else NULL

  # Initialize data frame for life table metrics
  lifetable <- data.frame(
    Age = age,
    nDx = Dx,
    nMx = nMx,
    nqx = NA,
    lx = NA,
    dx = NA,
    nax = NA,
    Lx = NA,
    Tx = NA,
    ex = NA
  )

  # Compute nax
  lifetable$nax <- ifelse(lifetable$Age == 0,
                          ifelse(lifetable$nMx < 0.107, 0.045 + 2.684 * lifetable$nMx, 0.33),
                          ifelse(lifetable$Age == 1,
                                 1.5,
                                 2.5)) # Placeholder for 5-year intervals
  # Compute nax for all other intervals (5-year intervals)
  #lifetable$nax[is.na(lifetable$nax)] <- 2 + (5 * lifetable$nMx[is.na(lifetable$nax)]) / 24

  # Compute nax for the open-ended interval
  lifetable$nax[nrow(lifetable)] <- 1 / lifetable$nMx[nrow(lifetable)]

  # Compute nqx
  lifetable$nqx <- ifelse(lifetable$Age == max(lifetable$Age),
                          1,
                          (5 * lifetable$nMx) / (1 + (5 - lifetable$nax) * lifetable$nMx))
  lifetable$nqx <- pmin(lifetable$nqx, 1) # Ensure nqx doesn't exceed 1

  # Compute lx and dx
  lifetable$lx[1] <- 100000 # Standard starting population
  for (i in 2:nrow(lifetable)) {
    lifetable$lx[i] <- lifetable$lx[i - 1] * (1 - lifetable$nqx[i - 1])
  }
  lifetable$dx <- lifetable$lx * lifetable$nqx

  # Compute nLx
  for (i in 1:(nrow(lifetable) - 1)) {
    lifetable$Lx[i] <- 5 * lifetable$lx[i + 1] + lifetable$nax[i] * lifetable$dx[i]
  }
  # Last age group
  lifetable$Lx[nrow(lifetable)] <- lifetable$lx[nrow(lifetable)] / lifetable$nMx[nrow(lifetable)]

  # Compute Tx and ex
  lifetable$Tx <- rev(cumsum(rev(lifetable$Lx)))
  lifetable$ex <- lifetable$Tx / lifetable$lx

  # Summary metrics
  metrics <- list(
    TotalDeaths = sum(lifetable$dx, na.rm = TRUE),
    LifeExpectancyAtBirth = lifetable$ex[1]
  )

  return(list(metrics = metrics, lifetable = lifetable))
}
