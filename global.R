# global.R: Global objects and setup for the Demographic Projection Shiny App

# 1. Load Libraries
# Ensure these packages are installed in your R environment
# install.packages(c("shiny", "R6", "ggplot2", "dplyr", "data.table", "DT", "shinythemes"))

library(shiny)
library(R6)
library(ggplot2)
library(dplyr)      # Used for data manipulation, though data.table is primary for models
library(data.table) # Core data structure for models and projection
library(DT)         # For interactive data tables in Shiny
library(shinythemes)# For UI styling

# Optional: logger for more structured messages than print/message, if used in R6 classes
# library(logger)
# log_threshold(INFO) # Example threshold

# 2. Source R Files

# Source all R6 class definitions from the R/ subdirectory
r_files_path <- "R"
r_files <- list.files(path = r_files_path, pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0) {
  warning("No R files found in R/ directory. Ensure class definition files are present.")
} else {
  # Ensure DemographicModelBase is sourced first if other models depend on it immediately at parse time
  # For R6, order usually doesn't matter as long as superclasses are defined before instantiation.
  # Explicitly ordering for clarity or strict dependency:
  ordered_r_files <- c(
    file.path(r_files_path, "DemographicModelBase.R"),
    file.path(r_files_path, "McmcSettings.R"),
    file.path(r_files_path, "FertilityModel.R"),
    file.path(r_files_path, "MortalityModel.R"),
    file.path(r_files_path, "MigrationModel.R"),
    file.path(r_files_path, "PopulationProjector.R")
  )
  # Filter out any that might not exist to prevent errors
  ordered_r_files_exist <- ordered_r_files[sapply(ordered_r_files, file.exists)]
  other_r_files_exist <- setdiff(r_files, ordered_r_files_exist) # Get any remaining files
  
  files_to_source <- c(ordered_r_files_exist, other_r_files_exist)
  
  if (!file.exists(file.path(r_files_path, "DemographicModelBase.R"))) {
      warning("R/DemographicModelBase.R not found. This is critical for other models.")
  }
  
  for (file_path in files_to_source) {
    if (file.exists(file_path)) {
      # message(paste("Sourcing:", file_path)) # Optional: for debugging
      source(file_path, local = TRUE) # Source into the global environment of the Shiny app
    } else {
      warning(paste("File not found during sourcing:", file_path))
    }
  }
}

# Source utility functions
utils_file_path <- "R/utils.R" # Corrected path assuming utils.R is in R/
if (file.exists(utils_file_path)) {
  source(utils_file_path, local = TRUE)
} else {
  warning(paste("Utility file not found:", utils_file_path, 
                "The validate_base_population function will be missing."))
}


# 3. Define Global Variables/Mappings

# Standardized column names expected by the projection engine and validation function
# These are the names used *after* any mapping from user-provided column names.
# The placeholder models are designed to work with these standard names internally.
REQUIRED_COLS <- c(
  "year", "age_group", "sex", "population",
  "births", "deaths", "in_migration", "out_migration",
  "region", "district"
)

# Default variable mapping: maps standard names to themselves.
# This is used when initializing the placeholder models, assuming that the
# base_population data passed to the projector will already use these standard names.
DEFAULT_VAR_MAPPING <- stats::setNames(as.list(REQUIRED_COLS), REQUIRED_COLS)
# Example: DEFAULT_VAR_MAPPING$year will be "year"


# 4. Instantiate Placeholder Models

# Create a global MCMC settings object (using defaults)
# These settings are part of the model class structure but won't be used for fitting
# in the placeholder versions.
mcmc_settings_global <- McmcSettings$new()

# Instantiate placeholder component models
# Note: The placeholder models' initialize methods should handle data = NULL gracefully.
# Their internal validate_*_data methods check for component-specific columns (e.g., 'births')
# in self$data. If self$data is NULL or an empty data.table, these checks should pass
# or be skipped, as these columns aren't needed for instantiation without data.
# The current implementation of validate_*_data in placeholders checks `self$var_map` and then `names(self$data)`.
# If self$data is `data.table(NULL)`, `names(self$data)` is `character(0)`, so the check
# `!(self$var_map$births %in% names(self$data))` will be TRUE, and it will stop.
# This needs a slight adjustment in the placeholder model validation logic:
# only check for columns in self$data if self$data is not empty.
# For now, we proceed assuming this adjustment or that the current setup is tolerant.
# A quick fix: provide an empty data.table with the required mapped columns for initialization.

# Create a minimal dummy data.table with expected columns for initialization if models require it.
# This allows validation checks on column names to pass if they look at names(self$data).
# The actual data for projection will come from user upload.
dummy_data_for_init <- data.table::data.table(
  year = integer(0), age_group = character(0), sex = character(0), population = numeric(0),
  births = numeric(0), deaths = numeric(0), in_migration = numeric(0), out_migration = numeric(0),
  region = character(0), district = character(0)
)
# Ensure all columns from DEFAULT_VAR_MAPPING values are present
for (col_name in unlist(DEFAULT_VAR_MAPPING)) {
    if (!col_name %in% names(dummy_data_for_init)) {
        # This logic is a bit circular if DEFAULT_VAR_MAPPING keys and values are the same.
        # The dummy_data_for_init above uses standard names already.
    }
}


fertility_model_global <- FertilityModel$new(
  data = dummy_data_for_init, # Use dummy data for initialization if needed
  variable_mapping = DEFAULT_VAR_MAPPING,
  mcmc_settings = mcmc_settings_global
)

mortality_model_global <- MortalityModel$new(
  data = dummy_data_for_init,
  variable_mapping = DEFAULT_VAR_MAPPING,
  mcmc_settings = mcmc_settings_global
)

migration_model_global <- MigrationModel$new(
  data = dummy_data_for_init,
  variable_mapping = DEFAULT_VAR_MAPPING,
  mcmc_settings = mcmc_settings_global
)

# Instantiate the main PopulationProjector
population_projector_global <- PopulationProjector$new(
  fertility_model = fertility_model_global,
  mortality_model = mortality_model_global,
  migration_model = migration_model_global
)

# message("Placeholder demographic models and projector instantiated globally.") # Optional

# 5. (Optional) Load Example Base Population Data
# For easier testing, you can load a default example base population.
# This should be a data.table and conform to REQUIRED_COLS.
# Example:
# if (file.exists("data/base_population_example.csv")) {
#   base_population_example <- data.table::fread("data/base_population_example.csv")
#   # Perform validation on this example data
#   # validation_result <- validate_base_population(base_population_example, REQUIRED_COLS)
#   # if (!validation_result$is_valid) {
#   #   warning(paste("Example base population data failed validation:", validation_result$message))
#   #   base_population_example <- NULL
#   # } else {
#   #   message("Loaded and validated example base population data.")
#   # }
# } else {
#   base_population_example <- NULL
#   # message("No example base population data loaded. Upload data via UI.")
# }
base_population_example <- NULL # Default to NULL if not loading an example

# End of global.R
```
