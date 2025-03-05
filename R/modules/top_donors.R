# Top Donors Module

# UI Function
topDonorsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                 h4("Top Donors Summary"),
                 uiOutput(ns("topDonorsSummary"))
             )
      )
    ),
    
    downloadButton(ns("download_csv"), "Download Full CSV"),
    downloadButton(ns("download_excel"), "Download Full Excel"),
    
    div(style = 'overflow-x: scroll',
        withSpinner(DTOutput(ns("topDonorsTable"))))
  )
}

# Server Function
topDonorsServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_top_donors_data(filtered_data())
    })
    
    # Top Donors Summary
    output$topDonorsSummary <- renderUI({
      # Use shared fiscal_years reactive
      if (length(fiscal_years()) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Process the top donors data (top 5)
      donors_data <- processed_data() %>%
        head(5)
      
      # Calculate overall total for percentage calculation
      overall_total <- sum(processed_data()$Total, na.rm = TRUE)
      
      create_summary_table(
        data = donors_data,
        id_col = "Name",
        value_col = "Total Contribution", 
        percent_col = "% of Total",
        overall_total = overall_total
      )
    })
    
    # Render data table
    output$topDonorsTable <- renderDT({
      data <- processed_data()
      currency_cols <- which(sapply(data, is.numeric)) - 1
      create_datatable(data, currency_cols)
    })
    
    # Create download handlers
    downloads <- create_download_handlers("top_donors_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}