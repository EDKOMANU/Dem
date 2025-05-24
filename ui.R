# ui.R: User Interface for the Demographic Projection Shiny App

#Ensure necessary libraries for UI elements are loaded (typically in global.R or server.R for shinyapps.io)
library(shiny)
library(shinythemes)
library(DT)
library(htmltools) # Not explicitly needed to load, but tags are from it.

ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"), # Apply a theme

  navbarPage(
    title = "Demographic Projection Suite",

    # ==========================================================================
    # Tab 1: Setup & Project
    # ==========================================================================
    tabPanel("Setup & Project", icon = icon("cog"),
      sidebarLayout(
        sidebarPanel(width = 4, # Adjusted width for potentially more controls
          h3("Projection Setup"),

          fileInput("basePopulationFile", "Upload Base Population Data (CSV)",
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

          numericInput("horizonYear", "Projection Horizon Year",
                       value = 2050, min = 2025, max = 2100, step = 1), # Defaulting min to a reasonable future start

          hr(),
          h4("Scenario Adjustments (Optional)"),
          p("Adjustments are applied annually to baseline model predictions."),

          strong("Fertility:"),
          sliderInput("fertilityMultiplier", "Fertility Multiplier",
                      min = 0.5, max = 1.5, value = 1.0, step = 0.05),

          strong("Mortality:"),
          sliderInput("mortalityMultiplier", "Mortality Rate Multiplier",
                      min = 0.5, max = 1.5, value = 1.0, step = 0.05),

          strong("In-Migration:"),
          sliderInput("inMigrationMultiplier", "In-Migration Multiplier",
                      min = 0.0, max = 2.0, value = 1.0, step = 0.1),
          numericInput("inMigrationFixedChange", "In-Migration Fixed Annual Change",
                       value = 0, step = 10), # Assuming step of 10 is reasonable

          strong("Out-Migration:"),
          sliderInput("outMigrationMultiplier", "Out-Migration Multiplier",
                      min = 0.0, max = 2.0, value = 1.0, step = 0.1),
          numericInput("outMigrationFixedChange", "Out-Migration Fixed Annual Change",
                       value = 0, step = 10),

          hr(),
          actionButton("runProjectionButton", "Run Projection", icon = icon("play-circle"), class = "btn-primary btn-block"), # btn-block for full width

          hr(),
          h5("Status & Validation:"),
          verbatimTextOutput("dataValidationStatusOutput") # For detailed validation messages
        ),

        mainPanel(width = 8,
          h3("Welcome & Initial Data Overview"), # Changed from htmltools::h3 for consistency
          p("Upload your base population data (CSV) and set the projection parameters using the sidebar. Once data is validated, click 'Run Projection'."), # Changed from htmltools::p

          plotOutput("initialPopulationPyramid"), # For base year's age structure

          hr(),
          h4("Projection Status:"),
          verbatimTextOutput("projectionStatusMessage") # For messages like "Projection successful"
        )
      )
    ),

    # ==========================================================================
    # Tab 2: Projection Outputs
    # ==========================================================================
    tabPanel("Projection Outputs", icon = icon("chart-line"),
      tabsetPanel(id = "outputTabs",
        tabPanel("Key Summary", icon = icon("list-alt"),
                 br(), # Add some space
                 uiOutput("summaryStatsOutput") # Single uiOutput for all summary stats
        ),
        tabPanel("Population Trends", icon = icon("chart-line"),
                 br(),
                 plotOutput("totalPopulationPlot", height = "400px"),
                 hr(),
                 plotOutput("demographicComponentsPlot", height = "400px")
        ),
        tabPanel("Age Structure", icon = icon("users"),
                 br(),
                 sliderInput("pyramidYearSlider", "Select Year for Pyramid:",
                             min = 2024, max = 2100, value = 2024, step = 1, sep = "",
                             animate = animationOptions(interval = 1000, loop = FALSE), width = '100%'),
                 plotOutput("populationPyramidPlot", height = "500px"),
                 hr(),
                 plotOutput("ageDistributionPlot", height = "400px")
        ),
        tabPanel("Detailed Data Table", icon = icon("table"),
                 br(),
                 DT::dataTableOutput("projectionDataTable"),
                 br(),
                 downloadButton("downloadDataButton", "Download Full Data (CSV)", class = "btn-success")
        )
      )
    ),

    # ==========================================================================
    # Tab 3: Model Info & Help
    # ==========================================================================
    tabPanel("Info & Help", icon = icon("info-circle"),
      fluidRow(
        column(width = 8, offset = 2, # Center content slightly
          h3("About This Application"),
          p("This application projects population based on demographic component models for fertility, mortality, and migration. It allows for scenario adjustments to explore different future population trajectories."),

          hr(),
          h4("Input Data Format"),
          p("Please upload a CSV file with the following required columns. Column names must match exactly:"),
          tags$ul(
            tags$li(strong("year:"), " Integer representing the base year of the population (e.g., 2020). Must be a single year for all rows."),
            tags$li(strong("age_group:"), " Character string defining age categories. Expected formats are 'X-Y' (e.g., '0-4', '25-29') or 'X+' for an open-ended terminal age group (e.g., '85+'). Consistent age group widths are recommended for sensible aging, though the placeholder aging is simplified."),
            tags$li(strong("sex:"), " Character string, typically 'Male' and 'Female' (case-sensitive)."),
            tags$li(strong("population:"), " Numeric count of people in each stratum."),
            tags$li(strong("births:"), " Numeric count of births in the base year for each stratum (can be 0; primarily for structural completeness)."),
            tags$li(strong("deaths:"), " Numeric count of deaths in the base year for each stratum (can be 0)."),
            tags$li(strong("in_migration:"), " Numeric count of in-migrants in the base year for each stratum (can be 0)."),
            tags$li(strong("out_migration:"), " Numeric count of out-migrants in the base year for each stratum (can be 0)."),
            tags$li(strong("region:"), " Character string for regional identifier (e.g., 'North', 'Region A')."),
            tags$li(strong("district:"), " Character string for district or sub-regional identifier (e.g., 'District1', 'AreaX').")
          ),
          p("Ensure data is clean: no NA values in key columns (year, age_group, sex, population, region, district), numeric columns are indeed numeric, and counts are non-negative."),

          hr(),
          h4("Scenario Controls"),
          p("Scenario adjustments are applied to the baseline predictions from the placeholder models:"),
          tags$ul(
            tags$li(strong("Multipliers:"), " A value of 1.0 means no change. A value of 1.1 increases the component by 10%, while 0.9 decreases it by 10%. For mortality, a multiplier less than 1.0 implies lower mortality rates."),
            tags$li(strong("Fixed Annual Changes:"), " These are absolute numbers added to (if positive) or subtracted from (if negative) the component counts *after* any multiplier has been applied. For example, an In-Migration Fixed Annual Change of 50 adds 50 in-migrants to each group's projection for each year.")
          ),

          hr(),
          h4("Output Interpretation"),
          p("The 'Projection Outputs' tab provides various ways to explore the results:"),
          tags$ul(
            tags$li(strong("Key Summary:"), " Overall statistics for the projection period."),
            tags$li(strong("Population Trends:"), " Visualizes total population and key demographic events (births, deaths, net migration) over time."),
            tags$li(strong("Age Structure:"), " Shows population pyramids for selected years and the changing age distribution over the projection period."),
            tags$li(strong("Detailed Data Table:"), " Provides the full projected data in a table format, available for download.")
          ),

          hr(),
          h4("Technical Notes"),
          p("This application uses R6-based placeholder models for demographic components. These models generate heuristic predictions and do not rely on statistical fitting for this demonstration version."),
          p("The aging process implemented is a simplified cohort component step where survivors from one age group move to the next. Births are distributed to the youngest age group based on a standard sex ratio.")
        )
      )
    )
  )
)
```
