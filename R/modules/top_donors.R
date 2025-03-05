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
topDonorsServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_top_donors_data(filtered_data())
    })
    
    # Top Donors Summary
    output$topDonorsSummary <- renderUI({
      # Get selected fiscal years
      fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
      
      if (length(fiscal_years) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Process the top donors data (top 5)
      donors_data <- processed_data() %>%
        head(5)
      
      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Donor</th>',
                           '<th class="text-right">Total Contribution</th>',
                           '<th class="text-right">% of Total</th>',
                           '</tr></thead><tbody>')
      
      # Calculate grand total for percentage calculation
      grand_total <- sum(donors_data$Total, na.rm = TRUE)
      overall_total <- sum(processed_data()$Total, na.rm = TRUE)
      
      # Add rows for each donor
      for(i in 1:nrow(donors_data)) {
        row <- donors_data[i, ]
        amount <- format_currency(row$Total)
        percent <- sprintf("%.1f%%", (row$Total / overall_total) * 100)
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$Name, '</td>',
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