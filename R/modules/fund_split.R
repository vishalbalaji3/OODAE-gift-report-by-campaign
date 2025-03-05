# Fund Split by Constituency Module

# UI Function
fundSplitUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                 h4("Fund Split by Constituency"),
                 uiOutput(ns("fundSplitSummary"))
             )
      )
    ),
    
    downloadButton(ns("download_csv"), "Download Full CSV"),
    downloadButton(ns("download_excel"), "Download Full Excel"),
    
    div(style = 'overflow-x: scroll',
        withSpinner(DTOutput(ns("fundSplitTable"))))
  )
}

# Server Function
fundSplitServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_fund_split_data(filtered_data())
    })
    
    # Fund Split Summary
    output$fundSplitSummary <- renderUI({
      # Get primary constituency codes
      const_codes <- sort(unique(filtered_data()$`Primary Constituency Code`))
      
      # Get selected fiscal years or most recent if none selected
      fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
      
      if (length(fiscal_years) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Use the most recent year for display if no specific filters selected
      most_recent_year <- tail(fiscal_years, 1)
      
      # Process the fund split data for summary (top 5 constituencies)
      fund_data <- processed_data() %>%
        arrange(desc(Total)) %>%
        head(5)
      
      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Constituency</th>',
                           '<th class="text-right">Total Amount</th>',
                           '<th class="text-right">% of Total</th>',
                           '</tr></thead><tbody>')
      
      # Calculate grand total for percentage calculation
      grand_total <- sum(fund_data$Total, na.rm = TRUE)
      
      # Add rows for each constituency
      for(i in 1:nrow(fund_data)) {
        row <- fund_data[i, ]
        amount <- format_currency(row$Total)
        percent <- sprintf("%.1f%%", (row$Total / grand_total) * 100)
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$`Primary Constituency Code`, '</td>',
                             '<td class="text-right">', amount, '</td>',
                             '<td class="text-right">', percent, '</td>',
                             '</tr>')
      }
      
      # Add totals row
      html_table <- paste0(html_table, '<tr class="info">',
                           '<th>Total (Top 5)</th>',
                           '<th class="text-right">', format_currency(grand_total), '</th>',
                           '<th class="text-right">100.0%</th>',
                           '</tr></tbody></table>')
      
      HTML(html_table)
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