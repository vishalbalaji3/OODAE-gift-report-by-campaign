# Average Gift Size Module

# UI Function
avgGiftUI <- function(id) {
  ns <- NS(id)
  
  # Summary section
  tagList(
    fluidRow(
      column(12,
             div(style = config$ui$panel$style,
                 h4("Average Gift Size Summary"),
                 uiOutput(ns("avgGiftSummary"))
             )
      )
    ),
    
    # Table section
    div(style = 'overflow-x: scroll',
        withSpinner(DTOutput(ns("avgGiftTable")))),
    
    # Download buttons
    div(style = paste0("margin-top: ", config$ui$spacing$margin, ";"),
        downloadButton(ns("download_csv"), "Download Full CSV"),
        downloadButton(ns("download_excel"), "Download Full Excel")
    )
  )
}

# Server Function
avgGiftServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_avg_gift_data(filtered_data())
    })
    
    # Average Gift Size Summary
    output$avgGiftSummary <- renderUI({
      # Get selected fiscal years
      fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
      
      if (length(fiscal_years) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Process the average gift size data
      avg_data <- processed_data()
      
      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Gift Type</th>',
                           '<th class="text-right">Average Gift Size</th>',
                           '</tr></thead><tbody>')
      
      # Add rows for each gift type
      for(i in 1:nrow(avg_data)) {
        row <- avg_data[i, ]
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$`Gift Type`, '</td>',
                             '<td class="text-right">', format_currency(row$`Overall Avg`), '</td>',
                             '</tr>')
      }
      
      # Calculate overall average
      overall_avg <- filtered_data() %>%
        summarise(avg = sum(`Fund Split Amount`, na.rm = TRUE) / n()) %>%
        pull(avg)
      
      # Add overall average row
      html_table <- paste0(html_table, '<tr class="info">',
                           '<th>Overall Average</th>',
                           '<th class="text-right">', format_currency(overall_avg), '</th>',
                           '</tr></tbody></table>')
      
      HTML(html_table)
    })
    
    # Render data table
    output$avgGiftTable <- renderDT({
      data <- processed_data()
      currency_cols <- which(sapply(data, is.numeric)) - 1
      create_datatable(data, currency_cols)
    })
    
    # Create download handlers
    downloads <- create_download_handlers("avg_gift_size_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}