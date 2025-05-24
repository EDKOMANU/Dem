# server.R: Server logic for the Demographic Projection Shiny App

# Ensure necessary libraries are loaded (typically in global.R)
# library(shiny)
# library(data.table)
# library(dplyr) # if used explicitly, though data.table is primary
# library(ggplot2)
# library(scales) # For formatting plot axes and numbers

# ==========================================================================
# Server Function
# ==========================================================================
server <- function(input, output, session) {

  # ==========================================================================
  # Part 1: Data Input & Model Execution (Content from previous step)
  # ==========================================================================

  # 1. Reactive Values Initialization
  rv <- shiny::reactiveValues(
    base_pop_data = NULL,
    projection_results = NULL,
    validation_message = "Please upload a base population CSV file.",
    projection_status_message = "", # For UI text output
    base_year_for_slider = NULL # To dynamically set pyramid slider min year
  )

  # 2. File Input Handling
  shiny::observeEvent(input$basePopulationFile, {
    shiny::req(input$basePopulationFile) 

    shiny::withProgress(message = 'Processing uploaded file...', value = 0, {
      shiny::incProgress(0.1, detail = "Reading data...")
      tryCatch({
        df <- data.table::fread(input$basePopulationFile$datapath, header = TRUE, stringsAsFactors = FALSE)
        
        shiny::incProgress(0.5, detail = "Validating data structure...")
        validation_result <- validate_base_population(df, REQUIRED_COLS) # Assumes REQUIRED_COLS from global.R
        
        rv$validation_message <- validation_result$message 

        if (validation_result$is_valid) {
          rv$base_pop_data <- validation_result$data 
          if ("year" %in% names(rv$base_pop_data) && is.numeric(rv$base_pop_data$year)) {
             rv$base_year_for_slider <- min(as.numeric(rv$base_pop_data$year), na.rm = TRUE)
          } else {
             rv$base_year_for_slider <- NULL 
          }
          rv$projection_results <- NULL 
          rv$projection_status_message <- "" 
        } else {
          rv$base_pop_data <- NULL
          rv$base_year_for_slider <- NULL
        }
        shiny::incProgress(1, detail = "Validation complete.")
        
      }, error = function(e) {
        rv$base_pop_data <- NULL
        rv$base_year_for_slider <- NULL
        rv$projection_results <- NULL
        rv$validation_message <- paste("Error reading or processing file:", e$message)
      })
    }) 
  })

  # 3. Render Validation Message
  output$dataValidationStatusOutput <- shiny::renderPrint({
    cat(rv$validation_message)
  })

  # 4. Parameter Input Reactives
  horizon_year_reactive <- shiny::reactive({
    h_year_val <- input$horizonYear
    if (is.null(h_year_val) || is.na(as.integer(h_year_val)) || as.integer(h_year_val) <= 0) {
      return(NULL) 
    }
    return(as.integer(h_year_val))
  })

  scenario_params_reactive <- shiny::reactive({
    params <- list(
      fertility_multiplier = as.numeric(input$fertilityMultiplier), 
      mortality_multiplier = as.numeric(input$mortalityMultiplier), 
      fertility = list( births_multiplier = as.numeric(input$fertilityMultiplier) ),
      mortality = list( rate_multiplier = as.numeric(input$mortalityMultiplier) ),
      migration = list(
        in_migration_multiplier = as.numeric(input$inMigrationMultiplier),
        in_migration_fixed_change = as.numeric(input$inMigrationFixedChange),
        out_migration_multiplier = as.numeric(input$outMigrationMultiplier),
        out_migration_fixed_change = as.numeric(input$outMigrationFixedChange)
      )
    )
    return(params)
  })

  # 5. Projection Execution
  shiny::observeEvent(input$runProjectionButton, {
    shiny::req(rv$base_pop_data, input$horizonYear) 

    shiny::withProgress(message = 'Running projection...', value = 0, {
      shiny::incProgress(0.1, detail = "Preparing data...")
      h_year <- horizon_year_reactive()
      s_params <- scenario_params_reactive()

      if (is.null(h_year)) {
         rv$projection_status_message <- "Error: Horizon year is invalid."
         shiny::showNotification(rv$projection_status_message, type = "error", duration = NULL)
         return()
      }
      if (!is.null(rv$base_year_for_slider) && h_year <= rv$base_year_for_slider) {
        rv$projection_status_message <- paste("Error: Horizon year (", h_year, ") must be greater than the base data year (", rv$base_year_for_slider, ").")
        shiny::showNotification(rv$projection_status_message, type = "error", duration = NULL)
        return() 
      }

      shiny::incProgress(0.3, detail = "Projecting population...")
      if (!exists("population_projector_global") || !inherits(population_projector_global, "PopulationProjector")) {
         rv$projection_status_message <- "Critical Error: Population projector model object is not loaded. Please check global.R."
         shiny::showNotification(rv$projection_status_message, type = "error", duration = NULL)
         return()
      }

      tryCatch({
        results <- population_projector_global$project(
          base_population = rv$base_pop_data,
          horizon_year = h_year,
          scenario_params = s_params
        )
        rv$projection_results <- results
        rv$projection_status_message <- "Projection successful!" 
        shiny::showNotification("Projection completed!", type = "message", duration = 5)
      }, error = function(e) {
        rv$projection_results <- NULL 
        rv$projection_status_message <- paste("Error during projection:", e$message)
        shiny::showNotification(paste("Projection Error:", e$message), type = "error", duration = NULL) 
      })
      shiny::incProgress(1, detail = "Finalizing.") 
    }) 
  })

  # 6. Render Projection Status Message
  output$projectionStatusMessage <- shiny::renderText({
    rv$projection_status_message
  })

  # 7. Update Pyramid Year Slider Dynamically
  shiny::observe({
    shiny::req(rv$base_year_for_slider) 
    current_horizon <- isolate(horizon_year_reactive()) 
    min_val <- as.integer(rv$base_year_for_slider)
    default_max_offset <- 50 
    max_val <- if(!is.null(current_horizon) && current_horizon > min_val) current_horizon else min_val + default_max_offset 
    shiny::updateSliderInput(session, "pyramidYearSlider", min = min_val, max = max_val, value = min_val)
  })

  shiny::observe({
    min_val_slider <- isolate(input$pyramidYearSliderOpts$min) 
    if(is.null(min_val_slider) && !is.null(rv$base_year_for_slider)) min_val_slider <- rv$base_year_for_slider
    if(is.null(min_val_slider)) min_val_slider <- as.integer(format(Sys.Date(), "%Y")) 

    max_val_slider <- isolate(horizon_year_reactive())
    if(is.null(max_val_slider)) max_val_slider <- min_val_slider + 50

    if(!is.null(rv$projection_results)) {
        proj_years_numeric <- suppressWarnings(as.numeric(rv$projection_results$year))
        min_proj_year <- min(proj_years_numeric, na.rm = TRUE)
        max_proj_year <- max(proj_years_numeric, na.rm = TRUE)
        min_val_slider <- min(min_val_slider, min_proj_year, na.rm = TRUE)
        max_val_slider <- max(max_val_slider, max_proj_year, na.rm = TRUE)
    }
    
    current_slider_val <- isolate(input$pyramidYearSlider) 
    new_slider_val <- current_slider_val
    
    if (is.null(new_slider_val) || new_slider_val < min_val_slider || new_slider_val > max_val_slider) {
      new_slider_val <- min_val_slider
    }
    if (new_slider_val < min_val_slider && !is.na(min_val_slider)) new_slider_val <- min_val_slider
    if (new_slider_val > max_val_slider && !is.na(max_val_slider)) new_slider_val <- max_val_slider
    if (min_val_slider > max_val_slider && !is.na(min_val_slider) && !is.na(max_val_slider)) max_val_slider <- min_val_slider

    # Ensure min_val_slider and max_val_slider are single, non-NA, finite values
    if(length(min_val_slider)!=1 || is.na(min_val_slider) || is.infinite(min_val_slider)) min_val_slider <- as.integer(format(Sys.Date(), "%Y")) -1
    if(length(max_val_slider)!=1 || is.na(max_val_slider) || is.infinite(max_val_slider)) max_val_slider <- as.integer(format(Sys.Date(), "%Y")) + 50
    if(length(new_slider_val)!=1 || is.na(new_slider_val) || is.infinite(new_slider_val)) new_slider_val <- min_val_slider


    shiny::updateSliderInput(session, "pyramidYearSlider",
                             min = as.integer(min_val_slider),
                             max = as.integer(max_val_slider),
                             value = as.integer(new_slider_val))
  })


  # ==========================================================================
  # Part 2: Output Generation & Visualization
  # ==========================================================================

  # Helper function to create ordered age_group factor
  order_age_groups <- function(ag_vector) {
    get_start_age <- function(ag_str) {
      tryCatch({ as.numeric(strsplit(gsub("\\+", "", ag_str), "-")[[1]][1]) }, 
               error = function(e) { Inf })
    }
    unique_ags <- unique(as.character(ag_vector))
    if (length(unique_ags) == 0) return(factor())
    sorted_unique_ags <- unique_ags[order(sapply(unique_ags, get_start_age))]
    factor(ag_vector, levels = sorted_unique_ags, ordered = TRUE)
  }

  # 1. Initial Population Pyramid
  output$initialPopulationPyramid <- shiny::renderPlot({
    shiny::req(rv$base_pop_data)
    plot_data <- data.table::copy(rv$base_pop_data)
    if (nrow(plot_data) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "Base population data is empty.") + ggplot2::theme_void())
    
    pyramid_data <- plot_data[, .(population = sum(population, na.rm = TRUE)), by = .(age_group, sex)]
    if (nrow(pyramid_data) > 0 && "age_group" %in% names(pyramid_data)) {
      pyramid_data[, age_group := order_age_groups(age_group)]
    } else {
      return(ggplot2::ggplot() + ggplot2::labs(title = "Not enough data for initial pyramid.") + ggplot2::theme_void())
    }
    if ("sex" %in% names(pyramid_data) && "Male" %in% unique(pyramid_data$sex)) {
        pyramid_data[sex == "Male", population := -population]
    }
    max_pop_abs <- max(abs(pyramid_data$population), na.rm = TRUE)
    if (is.infinite(max_pop_abs) || length(max_pop_abs) == 0) max_pop_abs <- 1

    ggplot2::ggplot(pyramid_data, ggplot2::aes(x = age_group, y = population, fill = sex)) +
      ggplot2::geom_col(width = 0.9) + ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Base Population Structure - Year", if(!is.null(rv$base_year_for_slider)) rv$base_year_for_slider else ""),
                    x = "Age Group", y = "Population Count", fill = "Sex") +
      ggplot2::scale_y_continuous(labels = function(x) scales::comma(abs(x)), limits = c(-max_pop_abs, max_pop_abs) * 1.1) +
      ggplot2::scale_fill_manual(values = c("Male" = "steelblue", "Female" = "salmon", "Unknown" = "grey")) +
      ggplot2::theme_minimal(base_size = 14)
  }, res = 96)

  # 2. Summary Statistics Output
  summary_data_reactive <- shiny::reactive({
    shiny::req(rv$projection_results)
    results_dt <- data.table::copy(rv$projection_results)
    results_dt[, year := as.numeric(as.character(year))] 
    start_year <- min(results_dt$year, na.rm = TRUE)
    end_year <- max(results_dt$year, na.rm = TRUE)
    pop_start <- sum(results_dt[year == start_year, population], na.rm = TRUE)
    pop_end <- sum(results_dt[year == end_year, population], na.rm = TRUE)
    projected_period_dt <- results_dt[year > start_year]
    total_births <- sum(projected_period_dt$births, na.rm = TRUE)
    total_deaths <- sum(projected_period_dt$deaths, na.rm = TRUE)
    total_in_migration <- sum(projected_period_dt$in_migration, na.rm = TRUE)
    total_out_migration <- sum(projected_period_dt$out_migration, na.rm = TRUE)
    total_net_migration <- total_in_migration - total_out_migration
    abs_growth <- pop_end - pop_start
    perc_growth <- if(pop_start != 0) (abs_growth / pop_start) * 100 else NA_real_
    num_proj_years <- end_year - start_year
    avg_annual_growth_rate <- if(pop_start != 0 && num_proj_years > 0) {
        ((pop_end / pop_start)^(1 / num_proj_years) - 1) * 100
    } else { NA_real_ }
    list(start_year = start_year, end_year = end_year, pop_start = pop_start, pop_end = pop_end,
         total_births = total_births, total_deaths = total_deaths, total_in_migration = total_in_migration,
         total_out_migration = total_out_migration, total_net_migration = total_net_migration,
         abs_growth = abs_growth, perc_growth = perc_growth, avg_annual_growth_rate = avg_annual_growth_rate,
         projection_period_label = if(num_proj_years > 0) paste0(start_year + 1, " - ", end_year) else as.character(start_year))
  })

  output$summaryStatsOutput <- shiny::renderUI({
    shiny::req(summary_data_reactive())
    stats <- summary_data_reactive()
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(6, shiny::tags$h4("Projection Period Overview"),
          shiny::tags$p(shiny::strong("Base Year: "), stats$start_year),
          shiny::tags$p(shiny::strong("Horizon Year: "), stats$end_year),
          shiny::tags$p(shiny::strong("Initial Population (", stats$start_year, "): "), scales::comma(stats$pop_start)),
          shiny::tags$p(shiny::strong("Final Population (", stats$end_year, "): "), scales::comma(stats$pop_end))),
        shiny::column(6, shiny::tags$h4("Population Growth"),
          shiny::tags$p(shiny::strong("Absolute Growth: "), scales::comma(round(stats$abs_growth,0))),
          shiny::tags$p(shiny::strong("Percentage Growth: "), if(is.na(stats$perc_growth)) "N/A" else paste0(round(stats$perc_growth, 2), "%")),
          shiny::tags$p(shiny::strong("Avg. Annual Growth Rate: "), if(is.na(stats$avg_annual_growth_rate)) "N/A" else paste0(round(stats$avg_annual_growth_rate, 2), "%")))),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(12, shiny::tags$h4(paste0("Total Demographic Components (Projected Period: ", stats$projection_period_label, ")"))),
        shiny::column(4, shiny::tags$p(shiny::strong("Total Births: "), scales::comma(stats$total_births))),
        shiny::column(4, shiny::tags$p(shiny::strong("Total Deaths: "), scales::comma(stats$total_deaths))),
        shiny::column(4, shiny::tags$p(shiny::strong("Net Natural Increase: "), scales::comma(stats$total_births - stats$total_deaths)))),
      shiny::fluidRow(
        shiny::column(4, shiny::tags$p(shiny::strong("Total In-Migration: "), scales::comma(stats$total_in_migration))),
        shiny::column(4, shiny::tags$p(shiny::strong("Total Out-Migration: "), scales::comma(stats$total_out_migration))),
        shiny::column(4, shiny::tags$p(shiny::strong("Net Migration: "), scales::comma(stats$total_net_migration))))
    )
  })

  # 3. Population Trends Plots
  aggregated_annual_data_reactive <- shiny::reactive({
    shiny::req(rv$projection_results)
    results_dt <- data.table::copy(rv$projection_results)
    results_dt[, year := as.numeric(as.character(year))]
    results_dt[, .(total_population = sum(population, na.rm = TRUE),
                   total_births = sum(births, na.rm = TRUE), total_deaths = sum(deaths, na.rm = TRUE),
                   total_in_migration = sum(in_migration, na.rm = TRUE), total_out_migration = sum(out_migration, na.rm = TRUE)
    ), by = year][order(year)]
  })

  output$totalPopulationPlot <- shiny::renderPlot({
    shiny::req(aggregated_annual_data_reactive())
    ggplot2::ggplot(aggregated_annual_data_reactive(), ggplot2::aes(x = year, y = total_population)) +
      ggplot2::geom_line(color = "dodgerblue", size = 1.2) + ggplot2::geom_point(color = "dodgerblue", size = 2.5) +
      ggplot2::labs(title = "Total Population Over Time", x = "Year", y = "Total Population") +
      ggplot2::scale_y_continuous(labels = scales::comma) + ggplot2::theme_minimal(base_size = 14)
  }, res = 96)

  output$demographicComponentsPlot <- shiny::renderPlot({
    shiny::req(aggregated_annual_data_reactive(), summary_data_reactive())
    plot_data_annual <- aggregated_annual_data_reactive()
    plot_data_annual[, net_migration := total_in_migration - total_out_migration]
    plot_data_long <- data.table::melt(plot_data_annual, id.vars = "year", 
                                       measure.vars = c("total_births", "total_deaths", "net_migration"),
                                       variable.name = "component", value.name = "count")
    start_data_year <- summary_data_reactive()$start_year
    plot_data_long_filtered <- plot_data_long[year > start_data_year]
    ggplot2::ggplot(plot_data_long_filtered, ggplot2::aes(x = year, y = count, color = component, group = component)) +
      ggplot2::geom_line(size = 1.2) + ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(title = "Projected Demographic Components Over Time", x = "Year", y = "Annual Count", color = "Component") +
      ggplot2::scale_y_continuous(labels = scales::comma) + ggplot2::theme_minimal(base_size = 14) +
      ggplot2::scale_color_manual(values = c("total_births" = "forestgreen", "total_deaths" = "firebrick", "net_migration" = "goldenrod"))
  }, res = 96)

  # 4. Age Structure Plots
  output$populationPyramidPlot <- shiny::renderPlot({
    shiny::req(rv$projection_results, input$pyramidYearSlider)
    year_selected <- as.numeric(input$pyramidYearSlider)
    plot_data <- rv$projection_results[year == year_selected]
    if(nrow(plot_data) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = paste("No data for year", year_selected)) + ggplot2::theme_void())
    pyramid_data <- plot_data[, .(population = sum(population, na.rm = TRUE)), by = .(age_group, sex)]
    if (nrow(pyramid_data) > 0 && "age_group" %in% names(pyramid_data)) {
        pyramid_data[, age_group := order_age_groups(age_group)]
    } else { return(ggplot2::ggplot() + ggplot2::labs(title = "Not enough data for pyramid.") + ggplot2::theme_void())}
    if ("sex" %in% names(pyramid_data) && "Male" %in% unique(pyramid_data$sex)) {
        pyramid_data[sex == "Male", population := -population]
    }
    max_pop_abs <- max(abs(pyramid_data$population), na.rm = TRUE)
    if (is.infinite(max_pop_abs) || length(max_pop_abs)==0) max_pop_abs <- 1
    ggplot2::ggplot(pyramid_data, ggplot2::aes(x = age_group, y = population, fill = sex)) +
      ggplot2::geom_col(width = 0.9) + ggplot2::coord_flip() +
      ggplot2::labs(title = paste("Population Pyramid - Year", year_selected), x = "Age Group", y = "Population Count", fill = "Sex") +
      ggplot2::scale_y_continuous(labels = function(x) scales::comma(abs(x)), limits = c(-max_pop_abs, max_pop_abs) * 1.1) +
      ggplot2::scale_fill_manual(values = c("Male" = "steelblue", "Female" = "salmon", "Unknown" = "grey")) +
      ggplot2::theme_minimal(base_size = 14)
  }, res = 96)

  output$ageDistributionPlot <- shiny::renderPlot({
    shiny::req(rv$projection_results)
    results_dt_copy <- data.table::copy(rv$projection_results)
    results_dt_copy[, year := as.numeric(as.character(year))]
    results_dt_copy[, start_age := suppressWarnings(as.numeric(gsub("-.*|\\+", "", age_group)))]
    results_dt_copy[, broad_age_group := cut(start_age, breaks = c(-Inf, 14, 24, 64, Inf), 
                                          labels = c("0-14", "15-24", "25-64", "65+"), right = TRUE, include.lowest = TRUE)]
    age_dist_data <- results_dt_copy[!is.na(broad_age_group), .(group_pop = sum(population, na.rm = TRUE)), by = .(year, broad_age_group)]
    total_pop_per_year <- age_dist_data[, .(total_year_pop = sum(group_pop, na.rm = TRUE)), by = year]
    age_dist_data <- merge(age_dist_data, total_pop_per_year, by = "year")
    age_dist_data[total_year_pop > 0, percentage := (group_pop / total_year_pop)] 
    age_dist_data[total_year_pop == 0, percentage := 0] 
    if(nrow(age_dist_data) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "Not enough data for age distribution plot.") + ggplot2::theme_void())
    ggplot2::ggplot(age_dist_data, ggplot2::aes(x = year, y = percentage, fill = broad_age_group)) +
      ggplot2::geom_area(alpha = 0.8, position = "fill") + 
      ggplot2::labs(title = "Age Distribution Over Time (Percentage)", x = "Year", y = "Percentage of Total Population", fill = "Broad Age Group") +
      ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
      ggplot2::theme_minimal(base_size = 14) + ggplot2::scale_fill_brewer(palette = "viridis") 
  }, res = 96)

  # 5. Data Table Output
  output$projectionDataTable <- DT::renderDataTable({
    shiny::req(rv$projection_results)
    display_data <- data.table::copy(rv$projection_results)
    cols_to_round <- c("population", "births", "deaths", "in_migration", "out_migration")
    cols_exist_to_round <- intersect(cols_to_round, names(display_data))
    if (length(cols_exist_to_round) > 0) {
      display_data[, (cols_exist_to_round) := lapply(.SD, function(x) if(is.numeric(x)) round(x, 0) else x), .SDcols = cols_exist_to_round]
    }
    numeric_cols_for_dt <- which(sapply(display_data, is.numeric)) -1 
    year_col_idx_dt <- which(names(display_data) == "year") -1
    numeric_cols_for_dt_aligned <- setdiff(numeric_cols_for_dt, year_col_idx_dt)
    DT::datatable(display_data, rownames = FALSE, filter = 'top', extensions = c('Buttons', 'FixedHeader'),
      options = list(pageLength = 10, lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
                     dom = 'Blfrtip', buttons = list(list(extend='copy',title=NULL), list(extend='csv',title=NULL,filename=paste0("population_projection_",Sys.Date())),
                                                     list(extend='excel',title=NULL,filename=paste0("population_projection_",Sys.Date()))),
                     scrollX = TRUE, fixedHeader = TRUE,
                     columnDefs = list(list(className = 'dt-right', targets = numeric_cols_for_dt_aligned))))
  })

  # 6. Download Handler
  output$downloadDataButton <- shiny::downloadHandler(
    filename = function() { paste0("population_projection_results_", Sys.Date(), ".csv") },
    content = function(file) { shiny::req(rv$projection_results); data.table::fwrite(rv$projection_results, file, row.names = FALSE) }
  )

} # End of server function
```
