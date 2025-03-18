#' Example Dataset: standards
#'
#' A dataset containing standard life table probabilities and logits.
#'
#' @format A data frame with 24 rows and 5 columns:
#' \describe{
#'   \item{Age}{Age groups expressed as single ages}
#'   \item{GeneralUN}{the Gerneral UN survival rates}
#'   \item{African}{African survival rates}
#'   \item{GeneralUN_logit}{General UN logit-transformed probabilities}
#'   \item{African_logit}{African logit-transformed probabilities}
#' }
#' @source UN Population Division
"standards"

#' Example Dataset: gphc2010
#'
#' This dataset contains Ghana's 2010 Population and Housing Census (GPHC) data.
#'
#' @format A data frame with 21 rows and 3 columns:
#' \describe{
#'   \item{Age}{Age groups expressed as single ages}
#'   \item{Pop}{population from the Ghana 2010 census}
#'   \item{Deaths}{ number of deaths from the Ghana 2010 census}
#' }
#' @source Ghana Statistical Service
"gphc2010"

#' Example Dataset: testdata for population projections
#'
#' Test dataset used for demonstration.
#' @format A tibble with 1,224 rows and 10 variables:
#' \describe{
#'   \item{region}{Character. Name of the region.}
#'   \item{district}{Character. Name of the district.}
#'   \item{year}{Numeric. Year of the data.}
#'   \item{age_group}{Character. Age group category.}
#'   \item{sex}{Character. Sex (Male/Female).}
#'   \item{population}{Numeric. Total population count.}
#'   \item{births}{Numeric. Number of births recorded.}
#'   \item{deaths}{Numeric. Number of deaths recorded.}
#'   \item{in_migration}{Numeric. Number of people migrating into the district.}
#'   \item{out_migration}{Numeric. Number of people migrating out of the district.}
#' }
#' @source Generated data
"testdata"

#' Karup-king coefficeints: first_coef
#'
#' These datasets contain coefficients for demographic computations.
#' @format A numeric matrix with 5 rows and 3 columns:
#' \describe{
#'   \item{Column 1}{First set of coefficients.}
#'   \item{Column 2}{Second set of coefficients.}
#'   \item{Column 3}{Third set of coefficients.}
#' }
#' @source Karup-king coefficients for age splitting
"first_coef"

#' Karup-king coefficeints: middle_coef
#'
#' These datasets contain coefficients for demographic computations.
#' @format A numeric matrix with 5 rows and 3 columns:
#' \describe{
#'   \item{Column 1}{First set of coefficients.}
#'   \item{Column 2}{Second set of coefficients.}
#'   \item{Column 3}{Third set of coefficients.}
#' }
#' @source Karup-king coefficients for age splitting
"middle_coef"

#' Karup-king coefficeints: last_coef
#'
#' These datasets contain coefficients for demographic computations.
#' @format A numeric matrix with 5 rows and 3 columns:
#' \describe{
#'   \item{Column 1}{First set of coefficients.}
#'   \item{Column 2}{Second set of coefficients.}
#'   \item{Column 3}{Third set of coefficients.}
#' }
#' @source Karup-king coefficients for age splitting
"last_coef"

#' testdata for karup-king age splitting
#'
#' This dataset contains  population counts for different age groups from 2021 to 2035.
#' The data is structured with age groups as categories and population estimates for each year.
#'
#' @format A tibble with 16 rows and 16 columns:
#' \describe{
#'   \item{age_col}{Character. Age group categories, e.g., `0-4`, `5-9`, etc.}
#'   \item{2021}{Numeric.  population count for the year 2021.}
#'   \item{2022}{Numeric.  population count for the year 2022.}
#'   \item{2023}{Numeric.  population count for the year 2023.}
#'   \item{2024}{Numeric.  population count for the year 2024.}
#'   \item{2025}{Numeric.  population count for the year 2025.}
#'   \item{2026}{Numeric.  population count for the year 2026.}
#'   \item{2027}{Numeric.  population count for the year 2027.}
#'   \item{2028}{Numeric.  population count for the year 2028.}
#'   \item{2029}{Numeric.  population count for the year 2029.}
#'   \item{2030}{Numeric.  population count for the year 2030.}
#'   \item{2031}{Numeric.  population count for the year 2031.}
#'   \item{2032}{Numeric.  population count for the year 2032.}
#'   \item{2033}{Numeric.  population count for the year 2033.}
#'   \item{2034}{Numeric.  population count for the year 2034.}
#'   \item{2035}{Numeric.  population count for the year 2035.}
#' }
#'
#' @usage data(data)
#'
#' @source Generated data for karup-king
#'
#' @examples
#' data(data)
#' head(data)
"data"

#' region2000  data for basic probabilitic population projections
#'
#' This dataset contains  population for regions in Ghana 2000 from the 2000 Census
#' The data is structured with region, TFR, 2000 pop, death rates, net migration.
#'
#' @format A tibble with 16 rows and 6 columns:
#' \describe{
#'   \item{Country}{Character. Age group categories, e.g., `0-4`, `5-9`, etc.}
#'   \item{Region}{Numeric.  population count for the year 2021.}
#'   \item{base_pop}{Numeric.  population count for the year 2022.}
#'   \item{TFR}{Numeric.  population count for the year 2023.}
#'   \item{death_rate}{Numeric.  population count for the year 2024.}
#'   \item{net_migration}{Numeric.  population count for the year 2025.}
#' }
#'
#' @usage data(region2000 )
#'
#' @source Ghana Statistical Service (2000)
#'
#' @examples
#' data(region2000 )
#' head(region2000 )
"region2000"
