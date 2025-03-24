#' Smart Interpolation Function
#'
#' Performs interpolation on numeric data, supporting linear, quadratic,
#' Lagrange, and cubic spline methods. Can handle both vector and dataframe inputs.
#'
#' @param x Numeric vector of x-values.
#' @param y Numeric vector of y-values (ignored if using a dataframe).
#' @param x_new Numeric vector of new x-values to interpolate.
#' @param data Optional dataframe containing y-values to interpolate.
#' @param y_cols Character vector of column names in `data` containing y-values.
#' @param method Character. Interpolation method: `"auto"`, `"linear"`, `"quadratic"`, `"lagrange"`, or `"spline"`.
#' Default is `"auto"` (chooses the best method based on the number of points).
#'
#' @return If vectors `x` and `y` are provided, returns a named numeric vector of interpolated values.
#' If a dataframe is provided, returns the original dataframe with additional interpolated columns.
#'
#' @examples
#' # Example 1: Interpolating between points (vector input)
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(2, 4, 6, 8, 10)
#' x_new <- c(2.5, 3.5, 4.5)
#' interpolation(x, y, x_new, method = "linear")
#'
#' # Example 2: Interpolating for a dataframe
#' data <- data.frame(
#'   category = c("A", "B", "C"),
#'   y1 = c(2, 5, 9),
#'   y2 = c(3, 6, 10)
#' )
#' x_new <- c(2.5, 3.5)
#' interpolation(x = c(1, 2), data = data, y_cols = c("y1", "y2"), x_new = x_new, method = "spline")
#'
#' @export
interpolation <- function(x, y, x_new,
                                data = NULL, y_cols = NULL,
                                method = c("auto", "linear", "quadratic", "lagrange", "spline")) {
  method <- match.arg(method)  # Ensure method is valid

  # Validate input
  if (!is.null(data) && !is.null(y_cols)) {
    if (!all(y_cols %in% names(data))) stop("Some y_cols not found in data")
    mode <- "dataframe"
  } else if (is.numeric(x) && is.numeric(y)) {
    if (length(x) != length(y)) stop("x and y must have the same length")
    mode <- "vector"
  } else {
    stop("Invalid input: Provide either (x, y) vectors or (data, y_cols) with x")
  }

  if (!is.numeric(x) || !is.numeric(x_new)) stop("x and x_new must be numeric")
  if (any(duplicated(x))) stop("x contains duplicate values, which is not allowed")

  # Warn if extrapolating
  if (any(x_new < min(x)) || any(x_new > max(x))) {
    warning("Some x_new values are outside the range of x. Extrapolation may be unreliable.")
  }

  # Interpolation Methods
  linear_interpolate <- function(x0, x1, y0, y1, x) {
    y0 + (x - x0) * ((y1 - y0)/(x1 - x0))
  }

  quadratic_interpolate <- function(x0, x1, x2, y0, y1, y2, x) {
    L0 <- ((x - x1)*(x - x2))/((x0 - x1)*(x0 - x2))
    L1 <- ((x - x0)*(x - x2))/((x1 - x0)*(x1 - x2))
    L2 <- ((x - x0)*(x - x1))/((x2 - x0)*(x2 - x1))
    y0*L0 + y1*L1 + y2*L2
  }

  lagrange_interpolate <- function(x_points, y_points, x_val) {
    n <- length(x_points)
    y_new <- 0
    for(i in 1:n) {
      term <- y_points[i]
      for(j in 1:n) {
        if(i != j) {
          term <- term * (x_val - x_points[j])/(x_points[i] - x_points[j])
        }
      }
      y_new <- y_new + term
    }
    return(y_new)
  }

  # Auto-method selection
  interpolate_points <- function(x_points, y_points, x_val) {
    n_points <- length(x_points)

    if (method == "linear" || (method == "auto" && n_points == 2)) {
      return(approx(x_points, y_points, xout = x_val, rule = 2)$y) # rule=2 allows extrapolation
    } else if (method == "quadratic" || (method == "auto" && n_points == 3)) {
      return(quadratic_interpolate(x_points[1], x_points[2], x_points[3],
                                   y_points[1], y_points[2], y_points[3],
                                   x_val))
    } else if (method == "lagrange" || (method == "auto" && n_points > 3)) {
      return(lagrange_interpolate(x_points, y_points, x_val))
    } else if (method == "spline") {
      return(spline(x_points, y_points, xout = x_val, method = "natural")$y)
    }
  }

  # Handle different input modes
  if (mode == "vector") {
    results <- sapply(x_new, function(x_val) interpolate_points(x, y, x_val))
    names(results) <- paste0("y_", x_new)
    return(results)
  } else {
    # Dataframe interpolation (without dplyr)
    y_list <- lapply(x_new, function(x_val) {
      apply(data[y_cols], 1, function(y_vals) interpolate_points(x, as.numeric(y_vals), x_val))
    })

    y_df <- do.call(cbind, y_list)
    colnames(y_df) <- paste0("y_", x_new)

    return(cbind(data, y_df))
  }
}
