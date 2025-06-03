# Module Template
# This is a template for creating Shiny modules using shared UI components

# UI Function
moduleNameUI <- function(id) {
  ns <- NS(id)

  create_module_ui(
    title = "Module Title",
    summary_output = uiOutput(ns("moduleSummary")),
    table_output = DTOutput(ns("moduleTable")),
    download_ns = ns,
    additional_content = list(
      # Optional: Add any additional UI elements here
      # create_alert("Information about this module", type = "info"),
      # actionButton(ns("refresh"), "Refresh Data", class = "btn btn-primary")
    )
  )
}

# Server Function
moduleNameServer <- function(id, filtered_data, fiscal_years, summary_stats, time_period = reactive("fiscal")) {
  moduleServer(id, function(input, output, session) {

    # Reactive expression for processed data
    processed_data <- reactive({
      # Validate input data
      req(filtered_data())
      
      # Process the data here
      data <- filtered_data()
      
      # Example processing:
      # - Filter by time period
      # - Aggregate data
      # - Calculate summaries
      
      return(data)
    })

    # Render summary
    output$moduleSummary <- renderUI({
      # Check for data availability
      if (length(fiscal_years()) == 0) {
        return(create_alert("No data available for the selected filters.", type = "warning"))
      }
      
      # Calculate summary statistics
      data <- processed_data()
      
      # Example summary statistics
      stats <- list(
        list(title = "Total Records", value = format_number(nrow(data)), color = "primary", icon = "list"),
        list(title = "Total Amount", value = format_currency(sum(data$amount, na.rm = TRUE)), color = "success", icon = "dollar-sign"),
        list(title = "Average", value = format_currency(mean(data$amount, na.rm = TRUE)), color = "info", icon = "calculator")
      )
      
      create_stats_row(stats)
    })

    # Render data table
    output$moduleTable <- renderDT({
      data <- processed_data()
      
      # Get appropriate title for year columns based on time period
      time_label <- get_time_period_label(time_period())
      
      # Rename year columns to include time period label
      if (time_period() == "fiscal") {
        old_names <- names(data)
        new_names <- old_names
        
        # For each column that's a year, add "FY" prefix
        for (i in seq_along(old_names)) {
          if (grepl("^\\d{4}$", old_names[i])) {
            new_names[i] <- paste0("FY ", old_names[i])
          }
        }
        
        names(data) <- new_names
      } else {
        # For calendar years, add "CY" prefix
        old_names <- names(data)
        new_names <- old_names
        
        for (i in seq_along(old_names)) {
          if (grepl("^\\d{4}$", old_names[i])) {
            new_names[i] <- paste0("CY ", old_names[i])
          }
        }
        
        names(data) <- new_names
      }
      
      # Identify currency columns for formatting
      currency_cols <- which(sapply(data, is.numeric)) - 1
      
      # Create enhanced datatable
      create_datatable(data, currency_cols = currency_cols)
    })

    # Create download handlers
    downloads <- create_download_handlers("module_data", processed_data, session)

    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
    
    # Optional: Add reactive values or additional server logic here
    # values <- reactiveValues(
    #   last_updated = Sys.time()
    # )
  })
}
