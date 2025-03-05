# Module Template
# This is a template for creating Shiny modules

# UI Function
moduleNameUI <- function(id) {
  ns <- NS(id)
  
  # Summary section
  tagList(
    h4("Module Title"),
    
    fluidRow(
      column(12,
             div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                 uiOutput(ns("moduleSummary"))
             )
      )
    ),
    
    # Table section
    div(style = 'overflow-x: scroll',
        withSpinner(DTOutput(ns("moduleTable")))),
    
    # Download buttons
    div(style = "margin-top: 15px;",
        downloadButton(ns("download_csv"), "Download Full CSV"),
        downloadButton(ns("download_excel"), "Download Full Excel")
    )
  )
}

# Server Function
moduleNameServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      # Process the data here
      # Return processed data
    })
    
    # Render summary
    output$moduleSummary <- renderUI({
      # Create HTML summary table
      HTML("Summary content goes here")
    })
    
    # Render data table
    output$moduleTable <- renderDT({
      # Create datatable
      create_datatable(processed_data())
    })
    
    # Create download handlers
    downloads <- create_download_handlers("module_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}