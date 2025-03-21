# Average Gift Size Module

# UI Function
avgGiftUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading", 
                     h4("Average Gift Size Summary")),
                 div(class = "panel-body",
                     uiOutput(ns("avgGiftSummary")))
             )
      )
    ),
    
    div(class = "table-responsive",
        withSpinner(DTOutput(ns("avgGiftTable")))),
    
    create_download_buttons(ns)
  )
}

# Server Function
avgGiftServer <- function(id, filtered_data, fiscal_years, summary_stats, time_period = reactive("fiscal")) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_avg_gift_data(filtered_data(), time_period = time_period())
    })
    
    # Average Gift Size Summary
    output$avgGiftSummary <- renderUI({
      # Get selected fiscal years
      years <- fiscal_years()
      
      if (length(years) == 0) {
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
      
      # Get appropriate title for year columns
      time_label <- get_time_period_label(time_period())
      
      # Rename year columns to include time period label
      if (time_period() == "fiscal") {
        old_names <- names(data)
        new_names <- old_names
        
        # For each column that's a year, add "FY" prefix
        for (i in seq_along(old_names)) {
          if (grepl("^\\d{4}$", old_names[i])) {
            new_names[i] <- paste0("FY ", old_names[i])
          }
        }
        
        names(data) <- new_names
      } else {
        # For calendar years, add "CY" prefix
        old_names <- names(data)
        new_names <- old_names
        
        # For each column that's a year, add "CY" prefix
        for (i in seq_along(old_names)) {
          if (grepl("^\\d{4}$", old_names[i])) {
            new_names[i] <- paste0("CY ", old_names[i])
          }
        }
        
        names(data) <- new_names
      }
      
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