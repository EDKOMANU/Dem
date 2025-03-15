#' Population Projector Class
#' @description R6 Class for combining demographic models and projecting population
#' @export
PopulationProjector <- R6::R6Class(
  "PopulationProjector",

  public = list(
    #' @field fertility_model FertilityModel instance
    fertility_model = NULL,
    #' @field mortality_model MortalityModel instance
    mortality_model = NULL,
    #' @field migration_model MigrationModel instance
    migration_model = NULL,
    #' @field projections Stored projection results
    projections = NULL,
    #' @field projection_variants Different projection scenarios
    projection_variants = NULL,
    #' @field simulations Stored simulation results
    simulations = NULL,
    #' @field credible_intervals Calculated credible intervals
    credible_intervals = NULL,

    #' @description Initialize the population projector
    initialize = function(fertility_model = NULL, mortality_model = NULL, migration_model = NULL) {
      self$fertility_model <- fertility_model
      self$mortality_model <- mortality_model
      self$migration_model <- migration_model
      private$init_timestamp <- "2025-03-12 04:31:20"
      private$init_user <- "EDKOMANU"
      private$validate_models()
      log_info("Population Projector initialized at: {private$init_timestamp} by {private$init_user}")
    },

    #' @description Project population
    #' @param base_population Initial population data
    #' @param horizon_year Target year for projection
    #' @param n_trajectories Number of trajectories to simulate
    project = function(base_population, horizon_year, n_trajectories = 10) {
      # Initialize metadata
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      user <- Sys.getenv("USER")
      log_info(paste0("Starting projection at: ", timestamp))
      log_info(paste0("User: ", user))

      # Convert to data.table if needed
      if (!inherits(base_population, "data.table")) {
        base_population <- as.data.table(base_population)
      }

      # Get variable mappings
      var_map <- private$get_variable_mapping()

      # Get base year and clean data
      base_year <- max(base_population$year)
      base_data <- base_population[year == base_year]
      log_info(paste0("Base year: ", base_year))

      # Create projection years sequence
      projection_years <- seq(base_year + 1, horizon_year)
      log_info(paste0("Projecting years: ", paste(projection_years, collapse = ", ")))

      # Initialize empty list to store each year's results
      all_years_data <- list()

      # Store base year data
      all_years_data[[as.character(base_year)]] <- copy(base_data)

      # Project each year
      current_pop <- copy(base_data)

      for (projection_year in projection_years) {
        log_info(paste0("\nProjecting year: ", projection_year))

        # Create new year's data with same structure
        new_year_data <- copy(current_pop)

        # Update year and reset demographic components
        new_year_data[, ':='(
          year = projection_year,
          births = 0,
          deaths = 0,
          in_migration = 0,
          out_migration = 0
        )]

        # Apply mortality
        if (!is.null(self$mortality_model)) {
          mort_rates <- self$mortality_model$predict_mortality(new_year_data)
          new_year_data[, ':='(
            deaths = round(pmin(population * mort_rates, population)),
            population = population - round(pmin(population * mort_rates, population))
          )]

          log_info(paste0("Applied mortality - Deaths: ", sum(new_year_data$deaths)))
        }

        # Apply aging - now with var_map
        new_year_data <- private$age_population(new_year_data, var_map)
        log_info("Applied aging to population")

        # Apply fertility
        if (!is.null(self$fertility_model)) {
          fert_rates <- self$fertility_model$predict_fertility(new_year_data)
          new_year_data[, births := round(fert_rates)]
          new_year_data <- private$distribute_births(new_year_data, var_map)  # Added var_map here too

          log_info(paste0("Applied fertility - Births: ", sum(new_year_data$births)))
        }

        # Apply migration
        if (!is.null(self$migration_model)) {
          migration_pred <- self$migration_model$predict_migration(new_year_data)
          new_year_data[, ':='(
            in_migration = migration_pred$in_migration,
            out_migration = pmin(migration_pred$out_migration, population),
            population = population + migration_pred$in_migration -
              pmin(migration_pred$out_migration, population)
          )]

          log_info(paste0("Applied migration - In: ", sum(new_year_data$in_migration),
                          ", Out: ", sum(new_year_data$out_migration)))
        }

        # Ensure non-negative population
        new_year_data[, population := pmax(0, population)]

        # Verify data structure
        expected_cols <- c("region", "district", "year", "age_group", "sex",
                           "population", "births", "deaths", "in_migration", "out_migration")
        missing_cols <- setdiff(expected_cols, names(new_year_data))

        if (length(missing_cols) > 0) {
          log_error(paste0("Missing columns in year ", projection_year, ": ",
                           paste(missing_cols, collapse = ", ")))
          break
        }

        # Store this year's results
        all_years_data[[as.character(projection_year)]] <- copy(new_year_data)

        # Log year completion
        log_info(paste0("Completed projection for year ", projection_year))
        log_info(paste0("Final population: ", sum(new_year_data$population)))

        # Update current population for next iteration
        current_pop <- copy(new_year_data)
      }

      # Combine all years into final dataset
      final_results <- rbindlist(all_years_data)

      # Verify results
      summary_by_year <- final_results[, .(
        total_pop = sum(population),
        total_births = sum(births),
        total_deaths = sum(deaths),
        total_in_mig = sum(in_migration),
        total_out_mig = sum(out_migration),
        n_districts = uniqueN(district),
        n_age_groups = uniqueN(age_group)
      ), by = year][order(year)]

      log_info("\nProjection results by year:")
      print(summary_by_year)

      # Store and return results
      self$projections <- copy(final_results)
      return(final_results)
    },

    # calculate_projection_variants = function(trajectories, var_map) {
    #   log_info("Starting variant calculations...")
    #
    #   if (length(trajectories) == 0) {
    #     log_error("No trajectories available for variant calculation")
    #     return(NULL)
    #   }
    #
    #   # Initialize result data.table
    #   result_dt <- data.table()
    #
    #   # Combine trajectories with detailed error checking
    #   for (idx in seq_along(trajectories)) {
    #     traj <- trajectories[[idx]]
    #
    #     if (is.null(traj) || nrow(traj) == 0) {
    #       log_error(sprintf("Invalid trajectory %d", idx))
    #       next
    #     }
    #
    #     # Print debug information for each trajectory
    #     log_debug(sprintf("Processing trajectory %d:", idx))
    #     log_debug(sprintf("- Years: %s",
    #                       paste(unique(traj[[var_map$time]]), collapse = ", ")))
    #     log_debug(sprintf("- Total population: %d",
    #                       sum(traj[[var_map$population]])))
    #
    #     temp_dt <- traj[, .(
    #       total_population = sum(get(var_map$population)),
    #       trajectory = idx
    #     ), by = get(var_map$time)]
    #
    #     result_dt <- rbindlist(list(result_dt, temp_dt))
    #   }
    #
    #   if (nrow(result_dt) == 0) {
    #     log_error("No valid data for variant calculation")
    #     return(NULL)
    #   }
    #
    #   # Calculate variants
    #   years <- sort(unique(result_dt[[var_map$time]]))
    #   log_info(sprintf("Calculating variants for %d years", length(years)))
    #
    #   variants <- list()
    #
    #   for (yr in years) {
    #     year_data <- result_dt[get(var_map$time) == yr]
    #
    #     # Print debug information for each year
    #     log_debug(sprintf("Year %d:", yr))
    #     log_debug(sprintf("- Number of trajectories: %d", nrow(year_data)))
    #     log_debug(sprintf("- Population range: [%d, %d]",
    #                       min(year_data$total_population),
    #                       max(year_data$total_population)))
    #
    #     quantiles <- quantile(year_data$total_population,
    #                           probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
    #                           na.rm = TRUE)
    #
    #     variants[[as.character(yr)]] <- list(
    #       low = trajectories[[which.min(abs(year_data$total_population - quantiles[1]))]][get(var_map$time) == yr],
    #       medium_low = trajectories[[which.min(abs(year_data$total_population - quantiles[2]))]][get(var_map$time) == yr],
    #       median = trajectories[[which.min(abs(year_data$total_population - quantiles[3]))]][get(var_map$time) == yr],
    #       medium_high = trajectories[[which.min(abs(year_data$total_population - quantiles[4]))]][get(var_map$time) == yr],
    #       high = trajectories[[which.min(abs(year_data$total_population - quantiles[5]))]][get(var_map$time) == yr]
    #     )
    #   }
    #
    #   log_info("Variant calculations completed")
    #   return(variants)
    # },

    #' @description Print projection summary
    print_summary = function() {
      cat("\nPopulation Projection Summary\n")
      cat("===========================\n")
      cat(sprintf("Initialized: %s by %s\n", private$init_timestamp, private$init_user))

      if (!is.null(self$projections)) {
        cat(sprintf("\nProjection Details:\n"))
        cat(sprintf("- Performed: %s by %s\n", private$projection_timestamp, private$projection_user))
        cat(sprintf("- Horizon Year: %d\n", private$projection_horizon))
        cat(sprintf("- Number of Trajectories: %d\n", private$projection_trajectories))
      }

      if (!is.null(self$credible_intervals)) {
        cat(sprintf("\nSimulation Details:\n"))
        cat(sprintf("- Performed: %s by %s\n", private$simulation_timestamp, private$simulation_user))
        cat(sprintf("- Number of Simulations: %d\n", private$simulation_count))
      }
    }
  ),

  private = list(
    init_timestamp = NULL,
    init_user = NULL,
    projection_timestamp = NULL,
    projection_user = NULL,
    projection_horizon = NULL,
    projection_trajectories = NULL,
    simulation_timestamp = NULL,
    simulation_user = NULL,
    simulation_count = NULL,

    validate_models = function() {
      if (!is.null(self$fertility_model) && !inherits(self$fertility_model, "FertilityModel")) {
        stop("fertility_model must be a FertilityModel instance")
      }
      if (!is.null(self$mortality_model) && !inherits(self$mortality_model, "MortalityModel")) {
        stop("mortality_model must be a MortalityModel instance")
      }
      if (!is.null(self$migration_model) && !inherits(self$migration_model, "MigrationModel")) {
        stop("migration_model must be a MigrationModel instance")
      }
    },

    get_variable_mapping = function() {
      if (!is.null(self$fertility_model)) return(self$fertility_model$get_mapping())
      if (!is.null(self$mortality_model)) return(self$mortality_model$get_mapping())
      if (!is.null(self$migration_model)) return(self$migration_model$get_mapping())
      stop("No models available to get variable mapping")
    },

    project_single_year = function(population, year, var_map) {
      tryCatch({
        pop <- copy(population)

        # Log initial state
        log_info(paste0("Processing year ", year))
        log_info(paste0("Initial population: ", sum(pop[[var_map$population]])))

        # 1. Apply mortality
        if (!is.null(self$mortality_model)) {
          mort_rates <- self$mortality_model$predict_mortality(pop)
          pop[, deaths := round(pmin(get(var_map$population) * mort_rates,
                                     get(var_map$population)))]
          pop[, (var_map$population) := get(var_map$population) - deaths]
        }

        # 2. Age the population
        pop <- private$age_population(pop, var_map)

        # 3. Apply fertility
        if (!is.null(self$fertility_model)) {
          fert_rates <- self$fertility_model$predict_fertility(pop)
          pop[, births := round(fert_rates)]
          pop <- private$distribute_births(pop, var_map)
        }

        # 4. Apply migration
        if (!is.null(self$migration_model)) {
          migration_pred <- self$migration_model$predict_migration(pop, type = "both")
          pop[, `:=`(
            in_migration = migration_pred$in_migration,
            out_migration = pmin(migration_pred$out_migration, get(var_map$population))
          )]
          pop[, (var_map$population) := get(var_map$population) +
                in_migration - out_migration]
        }

        # Ensure non-negative population
        pop[, (var_map$population) := pmax(0, get(var_map$population))]

        # Log final state
        log_info(paste0("Final population for year ", year, ": ",
                        sum(pop[[var_map$population]])))

        return(pop)

      }, error = function(e) {
        log_error(paste0("Error processing year ", year, ": ", conditionMessage(e)))
        return(NULL)
      })
    },

    age_population = function(population, var_map) {
      pop <- copy(population)

      # Get age groups and sort them properly
      age_groups <- unique(pop[[var_map$age]])
      age_starts <- sapply(strsplit(gsub("\\+", "", age_groups), "-"),
                           function(x) as.numeric(x[1]))
      age_order <- order(age_starts)
      sorted_age_groups <- age_groups[age_order]

      # Process aging by district and sex
      districts <- unique(pop$district)
      sexes <- unique(pop[[var_map$sex]])

      for(dist in districts) {
        for(sex in sexes) {
          # Process each age group from oldest to youngest (reverse order)
          for (i in (length(sorted_age_groups)-1):1) {
            curr_group <- sorted_age_groups[i]
            next_group <- sorted_age_groups[i + 1]

            # Calculate aging population for current group
            aging_rate <- private$get_aging_rate(curr_group)

            # Update population within district and sex
            curr_pop <- pop[district == dist &
                              get(var_map$sex) == sex &
                              get(var_map$age) == curr_group]

            aging_pop <- round(curr_pop[[var_map$population]] * aging_rate)

            pop[district == dist &
                  get(var_map$sex) == sex &
                  get(var_map$age) == curr_group,
                (var_map$population) := get(var_map$population) - aging_pop]

            pop[district == dist &
                  get(var_map$sex) == sex &
                  get(var_map$age) == next_group,
                (var_map$population) := get(var_map$population) + aging_pop]
          }
        }
      }

      return(pop)
    },

    distribute_births = function(population, var_map) {
      pop <- copy(population)

      # Process births by district
      districts <- unique(pop$district)

      for(dist in districts) {
        # Calculate total births for the district
        total_births <- sum(pop[district == dist]$births, na.rm = TRUE)

        if (total_births > 0) {
          # Calculate births by sex using standard sex ratio at birth
          male_births <- round(total_births * 0.515)
          female_births <- total_births - male_births

          # Find youngest age group
          youngest_age <- min(pop[[var_map$age]])

          # Add births to population by sex within district
          pop[district == dist &
                get(var_map$age) == youngest_age &
                get(var_map$sex) == "Male",
              (var_map$population) := get(var_map$population) + male_births]

          pop[district == dist &
                get(var_map$age) == youngest_age &
                get(var_map$sex) == "Female",
              (var_map$population) := get(var_map$population) + female_births]
        }
      }

      return(pop)
    },

    get_aging_rate = function(age_group) {
      # Extract age range and calculate aging rate
      ages <- as.numeric(unlist(strsplit(gsub("\\+", "", age_group), "-")))
      if (length(ages) == 1) return(0)  # Terminal age group
      return(1 / (diff(ages) + 1))
    },

    calculate_projection_variants = function(trajectories, var_map) {
      log_info("Starting variant calculations...")

      if (length(trajectories) == 0) {
        log_error("No trajectories available for variant calculation")
        return(NULL)
      }

      # Initialize storage for variants
      result_dt <- data.table()
      variants <- list()

      # Combine trajectories safely
      for (idx in seq_along(trajectories)) {
        tryCatch({
          traj <- trajectories[[idx]]
          if (!is.null(traj) && nrow(traj) > 0) {
            # Calculate total population by year for this trajectory
            temp_dt <- traj[, .(
              total_population = sum(get(var_map$population)),
              trajectory = idx
            ), by = .(year = get(var_map$time))]

            result_dt <- rbindlist(list(result_dt, temp_dt), use.names = TRUE)
          }
        }, error = function(e) {
          log_error(sprintf("Error processing trajectory %d: %s", idx, conditionMessage(e)))
        })
      }

      if (nrow(result_dt) == 0) {
        log_error("No valid data for variant calculation")
        return(NULL)
      }

      # Calculate variants for each year
      years <- sort(unique(result_dt$year))
      log_info(sprintf("Calculating variants for %d years", length(years)))

      # Store median trajectory
      median_trajectory <- NULL

      for (yr in years) {
        year_data <- result_dt[year == yr]

        if (nrow(year_data) > 0) {
          # Calculate quantiles
          quantiles <- quantile(year_data$total_population,
                                probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                na.rm = TRUE)

          # Find closest trajectories to each quantile
          closest_trajectories <- sapply(quantiles, function(q) {
            which.min(abs(year_data$total_population - q))
          })

          # Store variants for this year
          variants[[as.character(yr)]] <- list(
            low = trajectories[[closest_trajectories[1]]][get(var_map$time) == yr],
            medium_low = trajectories[[closest_trajectories[2]]][get(var_map$time) == yr],
            median = trajectories[[closest_trajectories[3]]][get(var_map$time) == yr],
            medium_high = trajectories[[closest_trajectories[4]]][get(var_map$time) == yr],
            high = trajectories[[closest_trajectories[5]]][get(var_map$time) == yr]
          )

          # Store median trajectory
          if (is.null(median_trajectory)) {
            median_trajectory <- trajectories[[closest_trajectories[3]]]
          } else {
            median_trajectory <- rbind(
              median_trajectory,
              trajectories[[closest_trajectories[3]]][get(var_map$time) == yr]
            )
          }
        }
      }

      log_info("Variant calculations completed")

      return(list(
        variants = variants,
        median = median_trajectory
      ))
    },

    verify_projections = function() {
      if (is.null(self$projections)) {
        log_error("Projections are NULL")
        return(FALSE)
      }

      var_map <- private$get_variable_mapping()

      # Check basic structure
      required_cols <- c(var_map$time, var_map$age, var_map$sex, var_map$population)
      missing_cols <- setdiff(required_cols, names(self$projections))

      if (length(missing_cols) > 0) {
        log_error(sprintf("Missing columns in projections: %s",
                          paste(missing_cols, collapse = ", ")))
        return(FALSE)
      }

      # Check for valid values
      if (any(is.na(self$projections[[var_map$population]]))) {
        log_error("NA values found in population")
        return(FALSE)
      }

      if (any(self$projections[[var_map$population]] < 0)) {
        log_error("Negative population values found")
        return(FALSE)
      }

      return(TRUE)
    },
    calculate_credible_intervals = function(n_sims, credible_levels) {
      self$credible_intervals <- list()

      for (level in credible_levels) {
        probs <- c((1 - level)/2, 0.5, 1 - (1 - level)/2)
        intervals <- private$calculate_interval_bounds(probs)
        self$credible_intervals[[as.character(level)]] <- intervals
      }
    },

    calculate_interval_bounds = function(probs) {
      var_map <- private$get_variable_mapping()
      years <- sort(unique(self$projections[[var_map$time]]))

      intervals <- data.table(year = years)
      pop_by_year <- lapply(self$simulations, function(sim) {
        sim[, .(total_pop = sum(get(var_map$population))), by = get(var_map$time)]
      })

      pop_matrix <- do.call(cbind, lapply(pop_by_year, function(x) x$total_pop))
      intervals[, c("lower", "median", "upper") :=
                  as.list(apply(pop_matrix, 1, quantile, probs = probs))]

      return(intervals)
    }
  )
)
