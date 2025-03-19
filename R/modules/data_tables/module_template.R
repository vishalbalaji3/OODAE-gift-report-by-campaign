# Module Template
# This is a template for creating Shiny modules

# UI Function
moduleNameUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading", 
                     h4("Module Title")),
                 div(class = "panel-body",
                     uiOutput(ns("moduleSummary")))
             )
      )
    ),
    
    div(class = "table-responsive",
        withSpinner(DTOutput(ns("moduleTable")))),
    
    create_download_buttons(ns)
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