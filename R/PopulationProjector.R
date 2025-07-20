#' @name PopulationProjector
#' @title Population Projection Engine
#' @description R6 Class for projecting population dynamics using component demographic models.
#' This version is designed to work with placeholder models and includes scenario adjustment capabilities.
#' @export
PopulationProjector <- R6::R6Class(
  "PopulationProjector",

  public = list(
    #' @field fertility_model An instance of a fertility model (e.g., placeholder FertilityModel).
    fertility_model = NULL,
    #' @field mortality_model An instance of a mortality model (e.g., placeholder MortalityModel).
    mortality_model = NULL,
    #' @field migration_model An instance of a migration model (e.g., placeholder MigrationModel).
    migration_model = NULL,
    #' @field projections Stored projection results from the last run.
    projections = NULL,


    #' @description
    #' Initialize the PopulationProjector object.
    #' @param fertility_model An initialized fertility model object.
    #' @param mortality_model An initialized mortality model object.
    #' @param migration_model An initialized migration model object.
    initialize = function(fertility_model = NULL, mortality_model = NULL, migration_model = NULL) {
      if (is.null(fertility_model)) stop("Fertility model must be provided.")
      if (is.null(mortality_model)) stop("Mortality model must be provided.")
      if (is.null(migration_model)) stop("Migration model must be provided.")

      # Basic check for predict methods - can be enhanced with inherits() if class names are fixed
      if(!("predict_fertility" %in% names(fertility_model))) stop("fertility_model lacks predict_fertility method.")
      if(!("predict_mortality" %in% names(mortality_model))) stop("mortality_model lacks predict_mortality method.")
      if(!("predict_migration" %in% names(migration_model))) stop("migration_model lacks predict_migration method.")

      self$fertility_model <- fertility_model
      self$mortality_model <- mortality_model
      self$migration_model <- migration_model

      # logger::log_info("PopulationProjector initialized with component models.")
    },

    #' @description
    #' Project population based on component models and scenario parameters.
    #' @param base_population A data.table with the starting population structure.
    #' Expected columns: "year", "age_group", "sex", "population", and columns for
    #' births, deaths, in_migration, out_migration (can be 0 for base year values not used in projection).
    #' Column names are expected to be standardized as per the placeholder models.
    #' @param horizon_year The final year for the projection.
    #' @param scenario_params A list for scenario adjustments. Example:
    #' `list(fertility = list(births_multiplier = 1.1), mortality = list(rate_multiplier = 0.9), migration = list(in_migration_fixed_change = 10))`
    #' @return A data.table containing the projected population for each year up to `horizon_year`.
    project = function(base_population, horizon_year, scenario_params = list()) {
      if (!data.table::is.data.table(base_population)) {
        base_population <- data.table::as.data.table(base_population)
      }
      if (nrow(base_population) == 0) stop("Base population data is empty.")

      required_cols <- c("year", "age_group", "sex", "population",
                           "births", "deaths", "in_migration", "out_migration")
      missing_cols <- setdiff(required_cols, names(base_population))
      if (length(missing_cols) > 0) {
        stop(paste("Base population data is missing required columns:", paste(missing_cols, collapse = ", ")))
      }

      base_year <- max(as.numeric(base_population$year), na.rm = TRUE)
      if (is.infinite(base_year) || base_year >= horizon_year) {
        stop("Horizon year must be greater than the base year in base_population.")
      }

      projection_years <- seq(from = base_year + 1, to = horizon_year, by = 1)
      all_years_data_list <- list()
      # Ensure base_population itself has all required columns, fill with 0 if not present (except identifiers/pop)
      for(col in required_cols) {
          if (!col %in% names(base_population)) base_population[, (col) := 0]
      }
      all_years_data_list[[as.character(base_year)]] <- data.table::copy(base_population)

      current_pop_data <- data.table::copy(base_population)

      for (proj_year in projection_years) {
        # logger::log_info("Projecting year: {proj_year}")

        next_year_data <- data.table::copy(current_pop_data)
        next_year_data[, year := proj_year]
        component_cols <- c("births", "deaths", "in_migration", "out_migration")
        for(col in component_cols) next_year_data[, (col) := 0.0] # Use 0.0 for numeric type consistency initially

        # 1. Mortality
        raw_mortality_rates <- self$mortality_model$predict_mortality(next_year_data)
        adjusted_mortality_rates <- private$apply_scenario_adjustments(
          raw_values = raw_mortality_rates,
          component_type = "mortality",
          adjustment_type = "rate",
          scenario_params = scenario_params
        )
        projected_deaths_float <- next_year_data$population * adjusted_mortality_rates
        projected_deaths_final <- pmax(0, round(projected_deaths_float))
        projected_deaths_final <- pmin(projected_deaths_final, next_year_data$population)

        next_year_data[, deaths := projected_deaths_final]
        next_year_data[, population := population - deaths]
        next_year_data[population < 0, population := 0]

        # 2. Aging
        next_year_data <- private$age_population(next_year_data)

        # 3. Fertility
        raw_births_counts <- self$fertility_model$predict_fertility(next_year_data)
        adjusted_births_counts <- private$apply_scenario_adjustments(
          raw_values = raw_births_counts,
          component_type = "fertility",
          adjustment_type = "births",
          scenario_params = scenario_params
        )
        projected_births_for_groups <- pmax(0, round(adjusted_births_counts))
        next_year_data[, births := projected_births_for_groups]
        next_year_data <- private$distribute_births(next_year_data)

        # 4. Migration
        raw_migration_list <- self$migration_model$predict_migration(next_year_data)

        adjusted_in_migration <- private$apply_scenario_adjustments(
          raw_values = raw_migration_list$in_migration,
          component_type = "migration",
          adjustment_type = "in_migration",
          scenario_params = scenario_params
        )
        adjusted_out_migration <- private$apply_scenario_adjustments(
          raw_values = raw_migration_list$out_migration,
          component_type = "migration",
          adjustment_type = "out_migration",
          scenario_params = scenario_params
        )

        projected_in_migration_final <- pmax(0, round(adjusted_in_migration))
        projected_out_migration_float <- pmax(0, adjusted_out_migration) # Round after checking against pop

        current_pop_val <- next_year_data$population # Population before any migration in this step
        pop_after_in_mig <- current_pop_val + projected_in_migration_final

        # Out-migration cannot exceed population after in-migration
        projected_out_migration_final <- pmin(round(projected_out_migration_float), pop_after_in_mig)

        next_year_data[, in_migration := projected_in_migration_final]
        next_year_data[, out_migration := projected_out_migration_final]
        next_year_data[, population := population + in_migration - out_migration]
        next_year_data[population < 0, population := 0]

        all_years_data_list[[as.character(proj_year)]] <- data.table::copy(next_year_data)
        current_pop_data <- next_year_data
      }

      self$projections <- data.table::rbindlist(all_years_data_list, use.names = TRUE, fill = TRUE)
      # logger::log_info("Population projection completed.")
      return(self$projections)
    }
  ),

  private = list(
    apply_scenario_adjustments = function(raw_values, component_type, adjustment_type, scenario_params) {
      adjusted_values <- raw_values

      # Check for component-specific parameter group first
      component_specific_params <- scenario_params[[component_type]]

      multiplier <- 1.0
      fixed_change <- 0.0

      # Determine multiplier
      multiplier_name_specific <- paste0(adjustment_type, "_multiplier") # e.g. "rate_multiplier"
      multiplier_name_general <- paste0(component_type, "_multiplier") # e.g. "mortality_multiplier"

      if (!is.null(component_specific_params) && !is.null(component_specific_params[[multiplier_name_specific]])) {
        multiplier <- component_specific_params[[multiplier_name_specific]]
      } else if (!is.null(scenario_params[[multiplier_name_general]])) { # Check global for component_multiplier
        multiplier <- scenario_params[[multiplier_name_general]]
      }

      # Determine fixed change
      fixed_change_name_specific <- paste0(adjustment_type, "_fixed_change") # e.g. "in_migration_fixed_change"
      fixed_change_name_general <- paste0(component_type, "_fixed_change")   # e.g. "migration_fixed_change"

      if (!is.null(component_specific_params) && !is.null(component_specific_params[[fixed_change_name_specific]])) {
        fixed_change <- component_specific_params[[fixed_change_name_specific]]
      } else if (!is.null(scenario_params[[fixed_change_name_general]])) { # Check global for component_fixed_change
         fixed_change <- scenario_params[[fixed_change_name_general]]
      }


      if(!is.numeric(multiplier) || length(multiplier) != 1) multiplier <- 1.0
      if(!is.numeric(fixed_change) || length(fixed_change) != 1) fixed_change <- 0.0

      adjusted_values <- (adjusted_values * multiplier) + fixed_change
      # Ensure non-negativity for rates or counts after adjustment (though pmax(0,...) is also used later for counts)
      adjusted_values <- pmax(0, adjusted_values)

      return(adjusted_values)
    },

    age_population = function(population_data) {
      # Simplified aging: Assumes all survivors from a non-terminal group move to the next.
      # This is appropriate for single-year age groups or a very basic placeholder.
      # A proper cohort-component model with multi-year age groups would be more complex.

      dt_copy <- data.table::copy(population_data)
      dt_copy[, population_after_aging := 0.0] # Initialize with float for sums

      # Robustly sort age groups (e.g., "0-4", "5-9", ..., "85+")
      unique_age_groups <- unique(dt_copy$age_group)
      get_start_age <- function(ag_str) {
          tryCatch({ as.numeric(strsplit(gsub("\\+", "", ag_str), "-")[[1]][1]) },
                   error = function(e) { Inf }) # Treat malformed or non-standard as oldest
      }
      sorted_age_groups <- unique_age_groups[order(sapply(unique_age_groups, get_start_age))]

      # Identify strata for grouping (all columns except population and age_group related)
      strata_cols <- setdiff(names(dt_copy), c("age_group", "population", "population_after_aging",
                                               "births", "deaths", "in_migration", "out_migration", "year_std", "age_factor"))
      # Ensure only valid column names are used
      strata_cols <- intersect(strata_cols, names(dt_copy))


      for (i in seq_along(sorted_age_groups)) {
        current_ag <- sorted_age_groups[i]
        survivors_in_current_ag <- dt_copy[age_group == current_ag]

        if (i < length(sorted_age_groups)) { # Not the terminal age group
          next_ag <- sorted_age_groups[i+1]

          if(nrow(survivors_in_current_ag) > 0) {
            # Aggregate population by strata and assign to next age group
            moving_pop_summary <- survivors_in_current_ag[, .(pop_to_move = sum(population, na.rm = TRUE)), by = strata_cols]

            # Update the population_after_aging in dt_copy for the next_ag
            # This uses a data.table join-update
            dt_copy[moving_pop_summary,
                    on = strata_cols, # Join by strata columns
                    population_after_aging := population_after_aging + ifelse(age_group == next_ag, i.pop_to_move, 0.0)
                   ]
          }
        } else { # Terminal age group - survivors remain
           if(nrow(survivors_in_current_ag) > 0) {
             # Add their population to their own group in population_after_aging
             dt_copy[survivors_in_current_ag,
                     on = c(strata_cols, "age_group"), # Match exactly these rows
                     population_after_aging := population_after_aging + i.population
                    ]
           }
        }
      }
      dt_copy[, population := round(population_after_aging)] # Round at the end of aging
      dt_copy[, population_after_aging := NULL] # Remove helper column
      return(dt_copy)
    },

    distribute_births = function(population_data) {
      # Assumes 'births' column contains total births for the strata of that row.
      if (!"births" %in% names(population_data) || sum(population_data$births, na.rm = TRUE) == 0) {
        return(population_data)
      }

      # Robustly find youngest age group
      unique_age_groups <- unique(population_data$age_group)
      get_start_age <- function(ag_str) {
          tryCatch({ as.numeric(strsplit(gsub("\\+", "", ag_str), "-")[[1]][1]) },
                   error = function(e) { Inf })
      }
      youngest_age_group <- unique_age_groups[which.min(sapply(unique_age_groups, get_start_age))]

      sex_ratio_male <- 0.515

      # Create a summary of total births per stratum (excluding age and sex of parent)
      # Strata for birth distribution (e.g., region, district, year)
      strata_cols_for_births <- intersect(names(population_data), c("region", "district", "year"))
      if(length(strata_cols_for_births) == 0 && "year" %in% names(population_data)) strata_cols_for_births <- "year"
      if(length(strata_cols_for_births) == 0) { # No identifiable strata, sum all births
          total_births_overall <- sum(population_data$births, na.rm = TRUE)
          # This case is tricky: where to add these births? Assume first stratum if data exists.
          # For robust app, base_population should have clear strata.
          # For this placeholder, if no strata, this might not work as expected.
          # logger::log_warn("No strata (region, district, year) for birth distribution. Births might not be assigned correctly if multiple implied strata exist.")
          # Fallback: if no strata, all births are assigned to the first unique combination of other columns for youngest age/sex.
          # This part needs careful thought if data has no region/district/year.
          # For now, assume strata_cols_for_births is not empty or this won't work well.
          if(nrow(population_data) == 0) return(population_data) # Should not happen if sum(births) > 0
          # Create a single row of aggregated births if no strata.
          aggregated_births <- data.table::data.table(total_newborns = total_births_overall)
          if (length(strata_cols_for_births) > 0) { # Should be true if we proceed
             aggregated_births <- population_data[, .(total_newborns = sum(births, na.rm = TRUE)), by = strata_cols_for_births]
          }

      } else {
          aggregated_births <- population_data[, .(total_newborns = sum(births, na.rm = TRUE)), by = strata_cols_for_births]
      }

      aggregated_births <- aggregated_births[total_newborns > 0]
      if(nrow(aggregated_births) == 0) return(population_data)

      output_pop_data <- data.table::copy(population_data)

      for(r_idx in 1:nrow(aggregated_births)){
          birth_row <- aggregated_births[r_idx]
          total_n <- birth_row$total_newborns

          male_n <- round(total_n * sex_ratio_male)
          female_n <- total_n - male_n

          # Update Male population for youngest_age_group in the current stratum
          male_join_cond <- c(list(age_group = youngest_age_group, sex = "Male"),
                              sapply(strata_cols_for_births, function(col) birth_row[[col]], simplify=FALSE))
          names(male_join_cond) <- c("age_group", "sex", strata_cols_for_births)

          # Check if the target row exists, if not, it implies an incomplete base_population structure (missing 0-4 age groups)
          # For a robust placeholder, we might need to add rows if they don't exist.
          # Current data.table behavior for X[Y, on=, Z:=A] will add rows if Y contains keys not in X and X is keyed.
          # If not keyed, it's an update-join. We assume target rows exist.
          output_pop_data[male_join_cond, on = names(male_join_cond), population := population + male_n]

          # Update Female population
          female_join_cond <- c(list(age_group = youngest_age_group, sex = "Female"),
                                sapply(strata_cols_for_births, function(col) birth_row[[col]], simplify=FALSE))
          names(female_join_cond) <- c("age_group", "sex", strata_cols_for_births)
          output_pop_data[female_join_cond, on = names(female_join_cond), population := population + female_n]
      }

      return(output_pop_data)
    }
  )
)

