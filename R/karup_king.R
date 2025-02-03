#' Split Age Groups into Single-Year Ages
#'
#' This function takes grouped population data (e.g., 5-year age groups) and distributes the population into single-year age groups using a coefficient-based interpolation approach. The user can specify the column containing age groups, one or more population columns, and coefficient matrices for edge and middle groups.
#'
#' @param df A data frame containing grouped population data.
#' @param age_col A string specifying the column in `df` that contains age group data (e.g., "15-19").
#' @param pops A string or vector of strings specifying one or more columns in `df` that contain population data.
#' @param first_coef A matrix of coefficients for splitting the first age group. Each row corresponds to a single-year age within the group, and columns represent neighbouring group weights. Defaults to:
#'
#' ```
#' first_coef <- matrix(
#'   c(0.344, -0.208, 0.064,   # Year 1 (e.g., age 15 in 15-19)
#'     0.248, -0.056, 0.008,   # Year 2 (age 16)
#'     0.176, 0.048, -0.024,   # Year 3 (age 17)
#'     0.128, 0.104, -0.032,   # Year 4 (age 18)
#'     0.104, 0.112, -0.016),  # Year 5 (age 19)
#'   nrow = 5, byrow = TRUE
#' )
#' ```
#'
#' @param middle_coef A matrix of coefficients for splitting middle age groups. Each row corresponds to a single-year age within the group, and columns represent neighbouring group weights. Defaults to:
#'
#' ```
#' middle_coef <- matrix(
#'   c(0.064, 0.152, -0.016,   # Year 1 (e.g., age 20 in 20-24)
#'     0.008, 0.224, -0.032,   # Year 2 (age 21)
#'     -0.024, 0.248, -0.024,   # Year 3 (age 22)
#'     -0.032, 0.224, 0.008,   # Year 4 (age 23)
#'     -0.016, 0.152, 0.064),  # Year 5 (age 24)
#'   nrow = 5, byrow = TRUE
#' )
#' ```
#'
#' @param last_coef A matrix of coefficients for splitting the last age group. Each row corresponds to a single-year age within the group, and columns represent neighbouring group weights. Defaults to:
#'
#' ```
#' last_coef <- matrix(
#'   c(-0.016, 0.112, 0.104,   # Year 1 (e.g., age 35 in 35-39)
#'     -0.032, 0.104, 0.128,   # Year 2 (age 36)
#'     -0.024, 0.048, 0.176,   # Year 3 (age 37)
#'     0.008, -0.056, 0.248,   # Year 4 (age 38)
#'     0.064, -0.208, 0.344),  # Year 5 (age 39)
#'   nrow = 5, byrow = TRUE
#' )
#' ```
#'
#' Overflow ages refer to ages that fall outside the range of the initial grouped data. These are removed to ensure the resulting data frame contains only ages that are relevant and within the specified age group boundaries.
#'
#' @return A data frame with single-year ages and corresponding population estimates for each specified population column.
#' @examples
#'
#'
#' #first_coef <- matrix(
#' #c(0.344, -0.208, 0.064,   # Year 1 (e.g., age 15 in 15-19)
#' # 0.248, -0.056, 0.008,   # Year 2 (age 16)
#' # 0.176, 0.048, -0.024,   # Year 3 (age 17)
#'#  0.128, 0.104, -0.032,   # Year 4 (age 18)
#'# 0.104, 0.112, -0.016),  # Year 5 (age 19)
#'# nrow = 5, byrow = TRUE)

#' #Coefficients for MIDDLE groups (columns: [previous, current, next])
#' #middle_coef <- matrix(
#' # c(0.064, 0.152, -0.016,   # Year 1 (e.g., age 20 in 20-24)
#' #   0.008, 0.224, -0.032,   # Year 2 (age 21)
#' #   -0.024, 0.248, -0.024,   # Year 3 (age 22)
#' #   -0.032, 0.224, 0.008,   # Year 4 (age 23)
#' #   -0.016, 0.152, 0.064),  # Year 5 (age 24)
#' # nrow = 5, byrow = TRUE
#'#)

#' # Coefficients for LAST group (columns: [previous-1, previous, current])
#' #last_coef <- matrix(
#'  #c(-0.016, 0.112, 0.104,   # Year 1 (e.g., age 35 in 35-39)
#'   # -0.032, 0.104, 0.128,   # Year 2 (age 36)
#'   #-0.024, 0.048, 0.176,   # Year 3 (age 37)
#'   # 0.008, -0.056, 0.248,   # Year 4 (age 38)
#'   # 0.064, -0.208, 0.344),  # Year 5 (age 39)
#'  #nrow = 5, byrow = TRUE
#'  #)

#' # ------------------------------
#'# STEP 2: Prepare Sample Data
#'# ------------------------------
#'
#' karup_king(df = data,
#' age_col = "age_col",
#' pops = c("2021","2022","2023","2024","2025","2026","2027","2028", "2029","2030","2031","2032","2033","2034","2035"),
#' first_coef = first_coef,
#' middle_coef = middle_coef,
#' last_coef = last_coef
#' )
#'
#' @export



karup_king<- function(df, age_col = "age_group", pops = "population",
                      first_coef=first_coef, middle_coef = middle_coef, last_coef = last_coef) {
  # Split the specified age column into start and end ages
  df <- df |>
    tidyr::separate(col = {{age_col}}, into = c("start_age", "end_age"),
                    sep = "-", convert = TRUE) |>
    dplyr::arrange(start_age)

  single_ages <- list()

  for (i in 1:nrow(df)) {
    current_group <- df[i, ]

    # Determine coefficients and neighbors based on group position
    if (i == 1) {
      coef_matrix <- first_coef
      pop1 <- current_group[, pops, drop = FALSE]
      pop2 <- if (i+1 <= nrow(df)) df[i+1, pops, drop = FALSE] else 0 * pop1
      pop3 <- if (i+2 <= nrow(df)) df[i+2, pops, drop = FALSE] else 0 * pop1
    } else if (i == nrow(df)) {
      coef_matrix <- last_coef
      pop1 <- if (i-2 >= 1) df[i-2, pops, drop = FALSE] else 0 * current_group[, pops, drop = FALSE]
      pop2 <- if (i-1 >= 1) df[i-1, pops, drop = FALSE] else 0 * current_group[, pops, drop = FALSE]
      pop3 <- current_group[, pops, drop = FALSE]
    } else {
      coef_matrix <- middle_coef
      pop1 <- df[i-1, pops, drop = FALSE]
      pop2 <- current_group[, pops, drop = FALSE]
      pop3 <- df[i+1, pops, drop = FALSE]
    }

    # Calculate single-year populations for each population column
    for (year_idx in 1:5) {
      age <- current_group$start_age + (year_idx - 1)

      # Calculate population values using matrix operations
      pop_values <- round(coef_matrix[year_idx, 1] * pop1 +
        coef_matrix[year_idx, 2] * pop2 +
        coef_matrix[year_idx, 3] * pop3,0)

      single_ages[[length(single_ages) + 1]] <- dplyr::bind_cols(
        data.frame(age = age),
        pop_values
      )
    }
  }

  # Combine results and remove overflow ages
  result <- dplyr::bind_rows(single_ages) |>
    dplyr::filter(age <= max(df$end_age)) |>
    dplyr::mutate(age=as.character(age))

  return(result)
}
