#' @title Data Validation Utilities
#' @description This file contains utility functions, primarily for validating input data
#' for the demographic projection Shiny application.

#' @name validate_base_population
#' @description Validates the uploaded base population data frame.
#' @param df The data frame (or object coercible to data.frame) uploaded by the user.
#' @param required_cols A character vector of column names that must be present.
#'        These are assumed to be the standardized names the projection engine expects.
#' @param var_map (Currently Unused in this version) The variable mapping list.
#'        Future versions might use this to map user column names to standard names
#'        before validation against `required_cols`.
#' @return A list with two elements:
#'         `is_valid`: TRUE if all checks pass, FALSE otherwise.
#'         `message`: A string detailing validation errors or a success message.
validate_base_population <- function(df, required_cols, var_map = NULL) {
  errors <- character(0) # Initialize an empty character vector for error messages

  # 1. Check if df is a data frame
  if (!is.data.frame(df)) {
    if (data.table::is.data.table(df)) {
      # Convert to data.frame for consistent checks, though data.table is fine.
      # df <- as.data.frame(df) 
      # Or, ensure all checks below work with data.table syntax if df is kept as data.table
    } else {
      errors <- c(errors, "Uploaded data is not a data frame or data.table.")
      return(list(is_valid = FALSE, message = paste(errors, collapse = "\n")))
    }
  }
  
  if (nrow(df) == 0) {
      errors <- c(errors, "Uploaded data is empty (contains no rows).")
      return(list(is_valid = FALSE, message = paste(errors, collapse = "\n")))
  }

  # 2. Check for presence of all required columns
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste0("Missing required columns: ", paste(missing_cols, collapse = ", "), "."))
  }

  # Proceed with further checks only if all required columns are present
  if (length(errors) == 0) {
    # 3. Check for NA values in essential columns
    # Define essential columns for NA checks (subset of required_cols or all of them)
    # For the projector, core identifiers and population are critical.
    # Components like births/deaths in base year might be less critical if not used for projection start.
    essential_na_check_cols <- intersect(required_cols, 
                                         c("year", "age_group", "sex", "population", 
                                           "region", "district")) # region/district if they are used
    
    for (col_name in essential_na_check_cols) {
      if (any(is.na(df[[col_name]]))) {
        errors <- c(errors, paste0("Column '", col_name, "' contains NA values."))
      }
    }

    # 4. Check if relevant columns are numeric
    numeric_cols_to_check <- intersect(required_cols, 
                                       c("year", "population", "births", "deaths", 
                                         "in_migration", "out_migration"))
    for (col_name in numeric_cols_to_check) {
      if (!is.numeric(df[[col_name]])) {
        # Attempt coercion for columns that might be read as character but should be numeric
        original_class <- class(df[[col_name]])
        # Try to coerce, then check again. This is a bit lenient.
        # A stricter validation might just error if not already numeric.
        # For Shiny, providing this flexibility might be good.
        coerced_col <- suppressWarnings(as.numeric(as.character(df[[col_name]])))
        if (any(is.na(coerced_col)) && !all(is.na(df[[col_name]]))) { # Check if new NAs were introduced by coercion
             errors <- c(errors, paste0("Column '", col_name, "' (class: ", original_class, ") could not be fully coerced to numeric (contains non-numeric values)."))
        } else if (all(is.na(coerced_col)) && !all(is.na(df[[col_name]]))) { # All became NA
             errors <- c(errors, paste0("Column '", col_name, "' (class: ", original_class, ") contains non-numeric values and could not be coerced."))
        } else {
            # If coercion seems okay or column was already all NAs
            df[[col_name]] <- coerced_col # Replace with coerced version if successful for further checks
            if (!is.numeric(df[[col_name]])) { # Still not numeric (should not happen if coercion worked)
                 errors <- c(errors, paste0("Column '", col_name, "' is not numeric."))
            }
        }
      }
    }
    
    # Re-check after potential coercion for numeric_cols_to_check
    for (col_name in numeric_cols_to_check) {
        if (is.numeric(df[[col_name]])) { # Only proceed if column is now confirmed numeric
            # 5. Check for non-negative values in count/population columns
            non_negative_cols <- intersect(numeric_cols_to_check, 
                                           c("population", "births", "deaths", 
                                             "in_migration", "out_migration"))
            if (col_name %in% non_negative_cols) {
              if (any(df[[col_name]] < 0, na.rm = TRUE)) {
                errors <- c(errors, paste0("Column '", col_name, "' contains negative values."))
              }
            }
            # Year should be positive (specific check)
            if (col_name == "year") {
                if (any(df[[col_name]] <= 0, na.rm = TRUE)) {
                    errors <- c(errors, paste0("Column '", col_name, "' contains non-positive values."))
                }
            }
        }
    }


    # 6. Check for a single base year
    if ("year" %in% names(df) && is.numeric(df$year)) {
      unique_years <- unique(na.omit(df$year))
      if (length(unique_years) > 1) {
        errors <- c(errors, paste0("Data contains multiple base years: ", paste(unique_years, collapse = ", "), ". Please provide data for a single base year."))
      } else if (length(unique_years) == 0 && nrow(df) > 0 && !all(is.na(df$year))) {
        # This case should be caught by NA check if year is essential, but as a safeguard
        errors <- c(errors, "Year column contains only NA values or is problematic.")
      }
    } else if (!("year" %in% names(df))) {
      # This is already caught by missing_cols check if "year" is in required_cols
      # errors <- c(errors, "Year column is missing, cannot determine base year.")
    }


    # 7. Basic check for age_group format (optional but good)
    if ("age_group" %in% names(df)) {
      # Example: Check if all age groups match a simple regex like "X-Y" or "X+"
      # This is a very basic check. More robust would be to check against a known list of valid age groups.
      # Regex: matches "digits-digits" or "digits+"
      age_pattern <- "^(\\d{1,3}-\\d{1,3}|\\d{1,3}\\+)$" 
      # Allow for 3 digits for ages like 100+ or 100-104
      
      # Check only non-NA age groups
      valid_age_formats <- grepl(age_pattern, na.omit(df$age_group))
      if (!all(valid_age_formats)) {
        # Find first few problematic age groups to show user
        problematic_ages <- head(na.omit(df$age_group)[!valid_age_formats], 3)
        errors <- c(errors, paste0("Column 'age_group' contains values with unexpected formats (e.g., '", 
                                   paste(problematic_ages, collapse="', '"), 
                                   "'). Expected format like '0-4', '20-24', '85+'.")
                   )
      }
    }
    
    # Check for duplicate rows based on key identifiers
    # Assuming region, district, year, age_group, sex form a unique key for population count
    identifier_cols <- intersect(required_cols, c("region", "district", "year", "age_group", "sex"))
    if (length(identifier_cols) > 0) { # Only if these identifiers are present
        # Check for duplicated identifier sets only if all identifier_cols are present
        if (all(identifier_cols %in% names(df))) {
            if (any(duplicated(df[, identifier_cols, drop = FALSE]))) {
                errors <- c(errors, "Data contains duplicate rows for the same combination of identifying columns (e.g., region, district, year, age_group, sex).")
            }
        }
    }


  } # End of "if all required columns are present"

  if (length(errors) == 0) {
    return(list(is_valid = TRUE, message = "Data validation successful. Ready for projection.", data = df))
  } else {
    # Prepend a general error message
    full_message <- paste("Data validation failed:", paste(errors, collapse = "\n- "), sep = "\n- ")
    return(list(is_valid = FALSE, message = full_message, data = NULL))
  }
}

# Example Usage (Conceptual - not part of the file content for utils.R itself):
# if (FALSE) {
#   # Define required columns for the test
#   req_cols <- c("year", "age_group", "sex", "population", "births", "deaths", 
#                 "in_migration", "out_migration", "region", "district")
# 
#   # Test case 1: Valid data
#   valid_df <- data.frame(
#     year = rep(2020, 4),
#     age_group = c("0-4", "5-9", "0-4", "5-9"),
#     sex = c("Male", "Male", "Female", "Female"),
#     population = c(100, 110, 95, 105),
#     births = c(5, 0, 4, 0), # Assuming births are by mother's characteristics, can be 0 for male rows
#     deaths = c(1, 0, 1, 0),
#     in_migration = c(10, 10, 8, 8),
#     out_migration = c(5, 5, 4, 4),
#     region = rep("North", 4),
#     district = rep("A", 4)
#   )
#   print(validate_base_population(valid_df, req_cols))
# 
#   # Test case 2: Missing column
#   invalid_df_missing_col <- valid_df[, -which(names(valid_df) == "population")]
#   print(validate_base_population(invalid_df_missing_col, req_cols))
# 
#   # Test case 3: NA values
#   invalid_df_na <- valid_df
#   invalid_df_na$population[1] <- NA
#   print(validate_base_population(invalid_df_na, req_cols))
# 
#   # Test case 4: Non-numeric population
#   invalid_df_non_numeric <- valid_df
#   invalid_df_non_numeric$population <- as.character(invalid_df_non_numeric$population)
#   invalid_df_non_numeric$population[2] <- "abc" # Introduce non-coercible
#   print(validate_base_population(invalid_df_non_numeric, req_cols))
#   
#   # Test case 4b: Non-numeric population (but coercible)
#   invalid_df_non_numeric_coercible <- valid_df
#   invalid_df_non_numeric_coercible$population <- as.character(invalid_df_non_numeric_coercible$population)
#   validation_result_coercible <- validate_base_population(invalid_df_non_numeric_coercible, req_cols)
#   print(validation_result_coercible)
#   if(validation_result_coercible$is_valid) print(str(validation_result_coercible$data))
#
# 
#   # Test case 5: Negative population
#   invalid_df_negative_pop <- valid_df
#   invalid_df_negative_pop$population[1] <- -10
#   print(validate_base_population(invalid_df_negative_pop, req_cols))
# 
#   # Test case 6: Multiple base years
#   invalid_df_multi_year <- valid_df
#   invalid_df_multi_year$year[3:4] <- 2021
#   print(validate_base_population(invalid_df_multi_year, req_cols))
# 
#   # Test case 7: Invalid age_group format
#   invalid_df_age_format <- valid_df
#   invalid_df_age_format$age_group[1] <- "0to4"
#   invalid_df_age_format$age_group[2] <- "5_9"
#   print(validate_base_population(invalid_df_age_format, req_cols))
#   
#   # Test case 8: Empty data frame
#   empty_df <- data.frame()
#   # This will error earlier, but if columns were defined:
#   # empty_df_cols <- setNames(data.frame(matrix(ncol = length(req_cols), nrow = 0)), req_cols)
#   # print(validate_base_population(empty_df_cols, req_cols))
#   # The current check handles nrow(df) == 0
#   print(validate_base_population(data.frame(), req_cols))
#
#   # Test case 9: Duplicate rows
#   duplicate_df <- rbind(valid_df, valid_df[1,])
#   print(validate_base_population(duplicate_df, req_cols))
# }

```
