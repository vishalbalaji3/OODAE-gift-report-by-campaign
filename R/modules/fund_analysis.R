# Fund Analysis Module

# UI Function
fundAnalysisUI <- function(id) {
  ns <- NS(id)
  
  # Summary section
  tagList(
    fluidRow(
      column(12,
             div(style = config$ui$panel$style,
                 h4("Top Funds Summary"),
                 uiOutput(ns("fundAnalysisSummary"))
             )
      )
    ),
    
    # Table section
    div(style = 'overflow-x: scroll',
        withSpinner(DTOutput(ns("fundAnalysisTable")))),
    
    # Download buttons
    div(style = paste0("margin-top: ", config$ui$spacing$margin, ";"),
        downloadButton(ns("download_csv"), "Download Full CSV"),
        downloadButton(ns("download_excel"), "Download Full Excel")
    )
  )
}

# Server Function
fundAnalysisServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_fund_analysis_data(filtered_data())
    })
    
    # Fund Analysis Summary
    output$fundAnalysisSummary <- renderUI({
      # Use the shared fiscal_years reactive
      if (length(fiscal_years()) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Process the fund analysis data for summary (top 5 funds)
      fund_data <- processed_data() %>%
        arrange(desc(Total)) %>%
        head(5)
      
      # Calculate overall total for percentage calculation
      overall_total <- sum(processed_data()$Total, na.rm = TRUE)
      
      create_summary_table(
        data = fund_data,
        id_col = "Fund Description",
        value_col = "Total Amount",
        percent_col = "% of Total",
        format_fn = format_currency,
        overall_total = overall_total
      )
    })
    
    # Render data table
    output$fundAnalysisTable <- renderDT({
      data <- processed_data()
      currency_cols <- which(sapply(data, is.numeric)) - 1
      create_datatable(data, currency_cols)
    })
    
    # Create download handlers
    downloads <- create_download_handlers("fund_analysis_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}