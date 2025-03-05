# Full Data Module

# UI Function
fullDataUI <- function(id) {
  ns <- NS(id)
  
  # Summary section
  tagList(
    fluidRow(
      column(12,
             div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                 h4("Data Overview"),
                 uiOutput(ns("fullDataSummary"))
             )
      )
    ),
    
    # Table section
    div(style = 'overflow-x: scroll',
        withSpinner(DTOutput(ns("fullDataTable")))),
    
    # Download buttons
    div(style = "margin-top: 15px;",
        downloadButton(ns("download_csv"), "Download Full CSV"),
        downloadButton(ns("download_excel"), "Download Full Excel")
    )
  )
}

# Server Function
fullDataServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Use the filtered data directly
    processed_data <- filtered_data
    
    # Full Data Summary
    output$fullDataSummary <- renderUI({
      # Get data
      data <- filtered_data()
      
      if (nrow(data) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Use shared summary_stats reactive
      stats <- summary_stats()
      
      # Get fiscal year range
      years <- fiscal_years()
      year_range <- ifelse(length(years) > 1, 
                           paste(years[1], "to", tail(years, 1)),
                           years[1])
      
      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Metric</th>',
                           '<th class="text-right">Value</th>',
                           '</tr></thead><tbody>')
      
      # Add summary rows
      html_table <- paste0(html_table, '<tr><td>Fiscal Year(s)</td><td class="text-right">', year_range, '</td></tr>')
      html_table <- paste0(html_table, '<tr><td>Total Number of Gifts</td><td class="text-right">', format(stats$total_gifts, big.mark = ","), '</td></tr>')
      html_table <- paste0(html_table, '<tr><td>Total Gift Amount</td><td class="text-right">', format_currency(stats$total_amount), '</td></tr>')
      html_table <- paste0(html_table, '<tr><td>Unique Constituents</td><td class="text-right">', format(stats$total_constituents, big.mark = ","), '</td></tr>')
      html_table <- paste0(html_table, '<tr><td>Average Gift Size</td><td class="text-right">', format_currency(stats$avg_gift_size), '</td></tr>')
      
      html_table <- paste0(html_table, '</tbody></table>')
      
      HTML(html_table)
    })
    
    # Render data table
    output$fullDataTable <- renderDT({
      data <- filtered_data() %>% arrange(`Fiscal Year`)
      currency_cols <- which(names(data) %in% c("Fund Split Amount", "Gift Amount", 
                                                "Gift Receipt Amount", "Gift Pledge Balance")) - 1
      create_datatable(data, currency_cols)
    })
    
    # Create download handlers
    downloads <- create_download_handlers("full_data", filtered_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}