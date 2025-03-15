#' @name DemographicModelBase
#' @title Base Demographic Model Clas
#'
#' @description Base R6 Class that provides common functionality for demographic models
#'
#' @param data The model's data
#' @param var_map Variable mapping
#' @return A processed data frame with standardized variables for the models to inherit
#'
#'
#' @export
DemographicModelBase <- R6::R6Class(
  "DemographicModelBase",

  public = list(
    #' @field data The model's data
    data = NULL,
    #' @field var_map Variable mapping
    var_map = NULL,

    #' @description Initialize the base model
    #' @param data Data frame containing demographic data with variables listed in the variable mapping
    #' @param variable_mapping List of variable names mapping to the data frame
    #' @examples
    #' var_mapping <- list(
  #'   region = "region",
  #'   district = "district",
  #'   time = "year",
  #'   age = "age_group",
  #'   sex = "sex",
  #'   population = "population",
  #'   deaths = "deaths",
  #'   in_migration= "in_migration" ,
  #'   out_migration = "out_migration"
  #' )
  #'
  #'
  #' DemographicModelBase$new(data=testdata, variable_mapping = var_mapping)
  #'
    initialize = function(data, variable_mapping) {
      private$validate_mapping(variable_mapping)
      self$var_map <- variable_mapping
      self$data <- data.table::as.data.table(data)
      private$init_timestamp <- "2025-03-11 18:48:47"
      private$init_user <- "EDKOMANU"
    },

    #' @description Prepare data for modeling
    prepare_model_data = function(data = NULL) {
      if (is.null(data)) data <- self$data
      model_data <- data.table::copy(data)

      model_data[, `:=`(
        year_std = (get(self$var_map$time) - min(self$data[[self$var_map$time]])) /
          (max(self$data[[self$var_map$time]]) - min(self$data[[self$var_map$time]])),
        region_id = factor(get(self$var_map$region)),
        district_id = factor(get(self$var_map$district)),
        age_factor = factor(get(self$var_map$age))
      )]

      return(model_data)
    },

    #' @description Get variable mapping
    get_mapping = function() {
      return(self$var_map)
    }
  ),

  private = list(
    init_timestamp = NULL,
    init_user = NULL,

    validate_mapping = function(mapping) {
      required_vars <- c("region", "district", "time", "age", "sex", "population")
      missing_vars <- setdiff(required_vars, names(mapping))
      if (length(missing_vars) > 0) {
        stop(sprintf("Missing required variable mappings: %s",
                    paste(missing_vars, collapse = ", ")))
      }
    }
  )
)
