#' Brass Logit Model
#'
#' @param data A data frame containing the observed probabilities (nqx) and ages.
#' @param qx_col Column name for nqx in the data frame.
#' @param age_col Column name for age in the data frame.
#' @param standard Standard to use ("African" or "GeneralUN").
#' @param standards_data Standards dataset (default: included "standards").
#' @return A list containing the model, coefficients, and modified data with predicted nqx.
#' @importFrom stats lm predict coef
#'
#' @examples
#' observed_data <- data.frame(
#'   age = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
#'   qx = c(0.1, 0.05, 0.02, NA, 0.005, NA, 0.002, 0.0015, NA, 0.0008, 0.0006, 0.0005)
#' )
#'
#' model<-brass_logit(
#'   data = observed_data,
#'   qx_col = "qx",
#'   age_col = "age",
#'   standard = "African"
#' )
#'
#' lifetable <- model$data
#'
#' lifetable
#'
#' lifetable_nqx(lifetable, age="age", nqx = "predicted_qx")

#' @export
brass_logit <- function(data, qx_col, age_col, standard, standards_data = NULL) {
  # Load the internal standards dataset if not provided
  if (is.null(standards_data)) {
    standards
    standards_data <- standards
  }

  # Validate inputs
  if (!is.data.frame(data)) stop("Input data must be a data frame.")
  if (!qx_col %in% colnames(data) || !age_col %in% colnames(data)) {
    stop("Specified columns for nqx and age must exist in the data frame.")
  }
  if (!standard %in% c("African", "GeneralUN")) {
    stop("Invalid standard. Choose 'African' or 'GeneralUN'.")
  }

  # Extract observed probabilities and age
  qx <- data[[qx_col]]
  age <- data[[age_col]]

  # Match age in standards data
  standard_logits <- standards_data[[paste0(standard, "_logit")]][match(age, standards_data$Age)]
  if (any(is.na(standard_logits))) {
    stop("Mismatch between age groups in the input data and standards data.")
  }

  # Logit transformation of observed qx (only where qx is not NA)
  logit_qx <- ifelse(!is.na(qx), 0.5*log(qx / (1 - qx)), NA)

  # Fit the model using non-NA values
  lm_fit <- lm(logit_qx ~ standard_logits, subset = !is.na(logit_qx))

  # Predict logits for all ages
  predicted_logits <- predict(lm_fit, newdata = data.frame(standard_logits = standard_logits))

  # Transform logits back to probabilities
  predicted_qx <- exp(predicted_logits) / (1 + exp(predicted_logits))

  # Replace NA values in qx with predicted values
  data$predicted_qx <-  predicted_qx

  # Return results
  return(list(
    model = lm_fit,
    coefficients = coef(lm_fit),
    data = data
  ))
}
