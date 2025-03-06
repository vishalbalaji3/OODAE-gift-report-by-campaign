# Unique Constituents Module

# UI Function
constituentsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading", 
                     h4("Unique Constituents Summary")),
                 div(class = "panel-body",
                     uiOutput(ns("constituentsSummary")))
             )
      )
    ),
    
    div(class = "table-responsive",
        withSpinner(DTOutput(ns("uniqueConstituentsTable")))),
    
    create_download_buttons(ns)
  )
}

# Server Function
constituentsServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_unique_constituents_data(filtered_data())
    })
    
    # Constituents Summary
    output$constituentsSummary <- renderUI({
      # Get selected fiscal years
      fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
      
      if (length(fiscal_years) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Process the unique constituents data
      const_data <- processed_data()
      
      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Constituency</th>',
                           '<th class="text-right">Unique Constituents</th>',
                           '<th class="text-right">% of Total</th>',
                           '</tr></thead><tbody>')
      
      # Calculate total for percentage calculation
      total_constituents <- sum(const_data$Total, na.rm = TRUE)
      
      # Add rows for each constituency code
      const_data <- const_data %>% arrange(desc(Total))
      for(i in 1:nrow(const_data)) {
        row <- const_data[i, ]
        percent <- sprintf("%.1f%%", (row$Total / total_constituents) * 100)
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$`Primary Constituency Code`, '</td>',
                             '<td class="text-right">', format(row$Total, big.mark = ","), '</td>',
                             '<td class="text-right">', percent, '</td>',
                             '</tr>')
      }
      
      # Add totals row
      html_table <- paste0(html_table, '<tr class="info">',
                           '<th>Total</th>',
                           '<th class="text-right">', format(total_constituents, big.mark = ","), '</th>',
                           '<th class="text-right">100.0%</th>',
                           '</tr></tbody></table>')
      
      HTML(html_table)
    })
    
    # Render data table
    output$uniqueConstituentsTable <- renderDT({
      create_datatable(processed_data())
    })
    
    # Create download handlers
    downloads <- create_download_handlers("unique_constituents_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}