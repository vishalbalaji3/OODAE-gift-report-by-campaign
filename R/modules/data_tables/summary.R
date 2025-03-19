# Summary Statistics Module

# UI Function
summaryStatisticsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading", 
                     uiOutput(ns("summaryHeading"))),
                 div(class = "panel-body",
                     uiOutput(ns("giftDistSummary")))
             )
      )
    ),
    
    div(class = "table-responsive",
        withSpinner(DTOutput(ns("summaryTable")))),
    
    create_download_buttons(ns)
  )
}

# Server Function
summaryStatisticsServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Process summary data
    processed_data <- reactive({
      # Ensure all Fund Split Amount values are numeric
      data <- filtered_data() %>%
        mutate(`Fund Split Amount` = as.numeric(`Fund Split Amount`))
      
      # Then process the data
      process_summary_data(data)
    })
    
    # Dynamic heading for the Gift Range Distribution Summary
    output$summaryHeading <- renderUI({
      # Get selected fiscal years
      fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
      
      if (length(fiscal_years) == 0) {
        heading <- "Gift Range Distribution Summary"
      } else if (length(fiscal_years) == 1) {
        heading <- paste("Gift Range Distribution Summary -", fiscal_years[1], "Fiscal Year")
      } else if (all(diff(as.numeric(fiscal_years)) == 1)) {
        # Consecutive years
        heading <- paste("Gift Range Distribution Summary -", 
                         fiscal_years[1], "to", tail(fiscal_years, 1), 
                         "Fiscal Years")
      } else {
        # Non-consecutive years
        heading <- paste("Gift Range Distribution Summary -", 
                         paste(fiscal_years, collapse = ", "), 
                         "Fiscal Years")
      }
      
      # Check if gift types are filtered
      if (length(input$giftTypeFilter) > 0) {
        if (length(input$giftTypeFilter) < length(levels(FullData$`Gift Type`))) {
          heading <- paste0(heading, " (", 
                            paste(input$giftTypeFilter, collapse = ", "), 
                            " only)")
        }
      }
      
      h4(heading)
    })
    
    # Gift distribution summary
    output$giftDistSummary <- renderUI({
      # Get selected fiscal years or most recent if none selected
      fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
      
      if (length(fiscal_years) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Use the most recent year if no specific filters selected
      most_recent_year <- tail(fiscal_years, 1)
      
      # Make sure data has numeric Fund Split Amount
      gift_data <- filtered_data() %>%
        mutate(`Fund Split Amount` = as.numeric(`Fund Split Amount`))
      
      # Process the gift distribution data for the summary
      dist_data <- process_gift_distribution_data(gift_data)
      
      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Gift Range</th>',
                           '<th class="text-right"># of Gifts</th>',
                           '<th class="text-right">Amount</th>',
                           '</tr></thead><tbody>')
      
      # If multiple years are selected, use Total columns
      if (length(input$yearFilter) > 1 || length(fiscal_years) > 1) {
        # Add rows for each gift range using totals
        for(i in 1:nrow(dist_data)) {
          row <- dist_data[i, ]
          gift_count <- row$Total_Gifts
          
          # Ensure amount is numeric before formatting
          amount_value <- as.numeric(row$Total_Amount)
          amount <- format_currency(amount_value)
          
          html_table <- paste0(html_table, '<tr>',
                               '<td>', row$Gift_Range, '</td>',
                               '<td class="text-right">', gift_count, '</td>',
                               '<td class="text-right">', amount, '</td>',
                               '</tr>')
        }
        
        # Add totals row
        total_gifts <- sum(dist_data$Total_Gifts, na.rm = TRUE)
        total_amount <- sum(as.numeric(dist_data$Total_Amount), na.rm = TRUE)
      } else {
        # Use only the most recent year data
        gift_count_col <- paste0("Number_of_Gifts_", most_recent_year)
        amount_col <- paste0("Total_Amount_", most_recent_year)
        
        # Check if columns exist before proceeding
        if(!(gift_count_col %in% names(dist_data) && amount_col %in% names(dist_data))) {
          return(HTML("<p>No data available for the selected fiscal year.</p>"))
        }
        
        # Add rows for each gift range
        for(i in 1:nrow(dist_data)) {
          row <- dist_data[i, ]
          gift_count <- if(gift_count_col %in% names(row)) row[[gift_count_col]] else 0
          
          # Ensure amount is numeric before formatting
          amount_value <- if(amount_col %in% names(row)) as.numeric(row[[amount_col]]) else 0
          amount <- format_currency(amount_value)
          
          html_table <- paste0(html_table, '<tr>',
                               '<td>', row$Gift_Range, '</td>',
                               '<td class="text-right">', gift_count, '</td>',
                               '<td class="text-right">', amount, '</td>',
                               '</tr>')
        }
        
        # Add totals row
        total_gifts <- sum(dist_data[[gift_count_col]], na.rm = TRUE)
        total_amount <- sum(as.numeric(dist_data[[amount_col]]), na.rm = TRUE)
      }
      
      # Format total amount
      formatted_total <- format_currency(total_amount)
      
      html_table <- paste0(html_table, '<tr class="info">',
                           '<th>Total</th>',
                           '<th class="text-right">', total_gifts, '</th>',
                           '<th class="text-right">', formatted_total, '</th>',
                           '</tr></tbody></table>')
      
      HTML(html_table)
    })
    
    # Summary table
    output$summaryTable <- renderDT({
      data <- processed_data()
      
      # Make sure all numeric columns are actually numeric
      data <- data %>%
        mutate(across(where(is.numeric), as.numeric))
      
      currency_cols <- which(sapply(data, is.numeric)) - 1
      create_datatable(data, currency_cols)
    })
    
    # Create download handlers
    downloads <- create_download_handlers("summary_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}