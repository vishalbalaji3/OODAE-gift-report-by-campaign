# Fund Split by Constituency Module

# UI Function
fundSplitUI <- function(id) {
  ns <- NS(id)
  
  # Summary section
  tagList(
    fluidRow(
      column(12,
             div(style = config$ui$panel$style,
                 h4("Fund Split by Constituency"),
                 uiOutput(ns("fundSplitSummary"))
             )
      )
    ),
    
    # Table section
    div(style = 'overflow-x: scroll',
        withSpinner(DTOutput(ns("fundSplitTable")))),
    
    # Download buttons
    div(style = paste0("margin-top: ", config$ui$spacing$margin, ";"),
        downloadButton(ns("download_csv"), "Download Full CSV"),
        downloadButton(ns("download_excel"), "Download Full Excel")
    )
  )
}

# Server Function
fundSplitServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_fund_split_data(filtered_data())
    })
    
    # Fund Split Summary
    output$fundSplitSummary <- renderUI({
      # Use shared fiscal_years reactive
      if (length(fiscal_years()) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Process the fund split data for summary (top 5 constituencies)
      fund_data <- processed_data() %>%
        arrange(desc(Total)) %>%
        head(5)
      
      create_summary_table(
        data = fund_data,
        id_col = "Primary Constituency Code",
        value_col = "Total Amount",
        percent_col = "% of Total"
      )
    })
    
    # Render data table
    output$fundSplitTable <- renderDT({
      data <- processed_data()
      currency_cols <- which(sapply(data, is.numeric)) - 1
      create_datatable(data, currency_cols)
    })
    
    # Create download handlers
    downloads <- create_download_handlers("fund_split_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}