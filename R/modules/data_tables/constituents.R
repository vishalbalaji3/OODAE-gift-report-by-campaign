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
constituentsServer <- function(id, filtered_data, fiscal_years, summary_stats, time_period = reactive("fiscal")) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_unique_constituents_data(filtered_data(), time_period = time_period())
    })
    
    # Constituents Summary
    output$constituentsSummary <- renderUI({
      # Get selected years
      years <- fiscal_years()
      
      if (length(years) == 0) {
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
        
        # Format constituent count with commas
        formatted_total <- format_number(row$Total)
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$`Primary Constituency Code`, '</td>',
                             '<td class="text-right">', formatted_total, '</td>',
                             '<td class="text-right">', percent, '</td>',
                             '</tr>')
      }
      
      # Add totals row with formatted value
      formatted_total_constituents <- format_number(total_constituents)
      
      html_table <- paste0(html_table, '<tr class="info">',
                           '<th>Total</th>',
                           '<th class="text-right">', formatted_total_constituents, '</th>',
                           '<th class="text-right">100.0%</th>',
                           '</tr></tbody></table>')
      
      HTML(html_table)
    })
    
    # Render data table
    output$uniqueConstituentsTable <- renderDT({
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
      
      create_datatable(data)
    })
    
    # Create download handlers
    downloads <- create_download_handlers("unique_constituents_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}