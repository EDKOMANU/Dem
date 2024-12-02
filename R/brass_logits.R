#' Brass Logit Model
#'
#' This function fits a Brass Logit Model based on user-specified standards.
#' The function uses the linear transformation property of the Brass Logit Model.
#'
#' @param data A data frame containing the observed probabilities (qx).
#' @param qx_col A string specifying the column name for the observed probabilities (qx).
#' @param age_col A string specifying the column name for age groups.
#' @param standard A string specifying the standard to use ("African" or "GeneralUN").
#' @param standards_data A data frame containing the standard logits (must include columns for "Age", standard probabilities, and logits).
#' @param ... Additional arguments passed to `lm()` for fitting the linear model.
#' @importFrom stats lm predict coef
#' @importFrom base log
#' @return A data frame containing the original data and the predicted qx values, with predictions for missing qx.
#'
#' @examples
#' # Example data with NA for some qx values
#' observed_data <- data.frame(
#'   age = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
#'   qx = c(0.1, 0.05, 0.02, NA, 0.005, NA, 0.002, 0.0015, NA, 0.0008, 0.0006, 0.0005)
#' )
#'
#' brass_logit_model(
#'   data = observed_data,
#'   qx_col = "qx",
#'   age_col = "age",
#'   standard = "African"
#' )
#'
#'
#' @export
brass_logit_model <- function(data, qx_col, age_col, standard, standards_data = NULL, ...) {
  # Validate inputs
  if (!is.data.frame(data)) stop("Input must be a data frame.")
  if (!qx_col %in% colnames(data) || !age_col %in% colnames(data)) {
    stop("Specified columns for qx and age must exist in the data frame.")
  }
  if (!standard %in% c("African", "GeneralUN")) {
    stop("Invalid standard specified. Choose 'African' or 'GeneralUN'.")
  }

  # Use the internal standards data if not provided
  if (is.null(standards_data)) {
    data("standards", package = "Dem")  # Load the internal standards data
    standards_data <- standards  # Assign the loaded dataset
  }

  # Ensure the standards data has the correct structure
  if (!is.data.frame(standards_data) || !all(c("Age", paste0(standard, "_logit")) %in% colnames(standards_data))) {
    stop("Standards data must be a data frame with columns 'Age' and the corresponding standard logits.")
  }

  # Extract observed probabilities and age
  qx <- data[[qx_col]]
  age <- data[[age_col]]

  if (any(qx <= 0 | qx >= 1, na.rm = TRUE)) stop("qx values must be between 0 and 1 (exclusive), excluding NAs.")

  # Logit transformation of observed qx
  logit_qx <- log(qx / (1 - qx))

  # Match age in the standards data
  standard_logits <- standards_data[[paste0(standard, "_logit")]][match(age, standards_data$Age)]
  if (any(is.na(standard_logits))) stop("Mismatch between provided age groups and standards data.")

  # Fit the linear model (only use rows where qx is not NA)
  lm_fit <- lm(logit_qx ~ standard_logits, data = data, subset = !is.na(qx), ...)

  # Predicted values for all ages
  predicted_logits <- predict(lm_fit, newdata = data)
  predicted_qx <- exp(predicted_logits) / (1 + exp(predicted_logits))

  # For rows where qx is NA, replace the missing qx with the predicted qx
  data$predicted_qx <- ifelse(is.na(qx), predicted_qx, qx)

  # Return the modified data frame with predicted qx values
  result <- list(
    model = lm_fit,
    coefficients = coef(lm_fit),
    data = data  # Return the modified data frame with predicted qx
  )

  return(result)
}
