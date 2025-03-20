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
summaryStatisticsServer <- function(id, filtered_data, fiscal_years, summary_stats, time_period = reactive("fiscal")) {
  moduleServer(id, function(input, output, session) {
    
    # Process summary data
    processed_data <- reactive({
      # Ensure all Fund Split Amount values are numeric
      data <- filtered_data() %>%
        mutate(`Fund Split Amount` = as.numeric(`Fund Split Amount`))
      
      # Then process the data with time period
      process_summary_data(data, time_period = time_period())
    })
    
    # Dynamic heading for the Gift Range Distribution Summary
    output$summaryHeading <- renderUI({
      # Get years based on time period
      years <- fiscal_years()
      time_label <- get_time_period_label(time_period())
      
      if (length(years) == 0) {
        heading <- "Gift Range Distribution Summary"
      } else if (length(years) == 1) {
        heading <- paste("Gift Range Distribution Summary -", years[1], time_label)
      } else if (all(diff(as.numeric(years)) == 1)) {
        # Consecutive years
        heading <- paste("Gift Range Distribution Summary -", 
                         years[1], "to", tail(years, 1), 
                         paste0(time_label, "s"))
      } else {
        # Non-consecutive years
        heading <- paste("Gift Range Distribution Summary -", 
                         paste(years, collapse = ", "), 
                         paste0(time_label, "s"))
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
      # Get years based on time period
      years <- fiscal_years()
      
      if (length(years) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Use the most recent year if no specific filters selected
      most_recent_year <- tail(years, 1)
      
      # Make sure data has numeric Fund Split Amount
      gift_data <- filtered_data() %>%
        mutate(`Fund Split Amount` = as.numeric(`Fund Split Amount`))
      
      # Process the gift distribution data for the summary
      dist_data <- process_gift_distribution_data(gift_data, time_period = time_period())
      
      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Gift Range</th>',
                           '<th class="text-right"># of Gifts</th>',
                           '<th class="text-right">Amount</th>',
                           '</tr></thead><tbody>')
      
      # Determine prefix based on time period
      prefix <- ifelse(time_period() == "calendar", "CY", "FY")
      
      # If multiple years are selected, use Total columns
      if (length(years) > 1) {
        # Add rows for each gift range using totals
        for(i in 1:nrow(dist_data)) {
          row <- dist_data[i, ]
          
          # Format gift count with commas
          gift_count <- format_number(row$Total_Gifts)
          
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
        total_gifts <- format_number(sum(dist_data$Total_Gifts, na.rm = TRUE))
        total_amount <- sum(as.numeric(dist_data$Total_Amount), na.rm = TRUE)
      } else {
        # Use only the most recent year data
        gift_count_col <- paste0("Number_of_Gifts_", prefix, "_", most_recent_year)
        amount_col <- paste0("Total_Amount_", prefix, "_", most_recent_year)
        
        # Check if columns exist before proceeding
        if(!(gift_count_col %in% names(dist_data) && amount_col %in% names(dist_data))) {
          # Try without prefix for backward compatibility
          gift_count_col <- paste0("Number_of_Gifts_", most_recent_year)
          amount_col <- paste0("Total_Amount_", most_recent_year)
          
          if(!(gift_count_col %in% names(dist_data) && amount_col %in% names(dist_data))) {
            return(HTML("<p>No data available for the selected year.</p>"))
          }
        }
        
        # Add rows for each gift range
        for(i in 1:nrow(dist_data)) {
          row <- dist_data[i, ]
          
          # Get and format gift count with commas
          gift_count_value <- if(gift_count_col %in% names(row)) row[[gift_count_col]] else 0
          gift_count <- format_number(gift_count_value)
          
          # Ensure amount is numeric before formatting
          amount_value <- if(amount_col %in% names(row)) as.numeric(row[[amount_col]]) else 0
          amount <- format_currency(amount_value)
          
          html_table <- paste0(html_table, '<tr>',
                               '<td>', row$Gift_Range, '</td>',
                               '<td class="text-right">', gift_count, '</td>',
                               '<td class="text-right">', amount, '</td>',
                               '</tr>')
        }
        
        # Add totals row with formatted values
        total_gifts_value <- sum(dist_data[[gift_count_col]], na.rm = TRUE)
        total_gifts <- format_number(total_gifts_value)
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
      
      # Get appropriate title for year columns
      time_label <- get_time_period_label(time_period())
      
      # Rename columns if they're fiscal years
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
    downloads <- create_download_handlers("summary_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}