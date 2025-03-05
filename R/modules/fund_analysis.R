# Fund Analysis Module

# UI Function
fundAnalysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                 h4("Top Funds Summary"),
                 uiOutput(ns("fundAnalysisSummary"))
             )
      )
    ),
    
    downloadButton(ns("download_csv"), "Download Full CSV"),
    downloadButton(ns("download_excel"), "Download Full Excel"),
    
    div(style = 'overflow-x: scroll',
        withSpinner(DTOutput(ns("fundAnalysisTable"))))
  )
}

# Server Function
# Server Function
fundAnalysisServer <- function(id, filtered_data, fiscal_years = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Use local fiscal_years if provided, otherwise calculate it
    local_fiscal_years <- if(!is.null(fiscal_years)) {
      fiscal_years
    } else {
      reactive({
        sort(unique(filtered_data()$`Fiscal Year`))
      })
    }
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_fund_analysis_data(filtered_data())
    })
    
    # Fund Analysis Summary
    output$fundAnalysisSummary <- renderUI({
      # Use the local fiscal years reactive
      if (length(local_fiscal_years()) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Process the fund analysis data for summary (top 5 funds)
      fund_data <- processed_data() %>%
        arrange(desc(Total)) %>%
        head(5)
      
      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Fund</th>',
                           '<th class="text-right">Total Amount</th>',
                           '<th class="text-right">% of Total</th>',
                           '</tr></thead><tbody>')
      
      # Calculate grand total for percentage calculation
      grand_total <- sum(fund_data$Total, na.rm = TRUE)
      overall_total <- sum(processed_data()$Total, na.rm = TRUE)
      
      # Add rows for each fund
      for(i in 1:nrow(fund_data)) {
        row <- fund_data[i, ]
        amount <- format_currency(row$Total)
        percent <- sprintf("%.1f%%", (row$Total / overall_total) * 100)
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$`Fund Description`, '</td>',
                             '<td class="text-right">', amount, '</td>',
                             '<td class="text-right">', percent, '</td>',
                             '</tr>')
      }
      
      # Add totals row
      html_table <- paste0(html_table, '<tr class="info">',
                           '<th>Total (Top 5)</th>',
                           '<th class="text-right">', format_currency(grand_total), '</th>',
                           '<th class="text-right">', sprintf("%.1f%%", (grand_total / overall_total) * 100), '</th>',
                           '</tr></tbody></table>')
      
      HTML(html_table)
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