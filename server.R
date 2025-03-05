# Server Definition
server <- function(input, output, session) {
  # Create a timestamp when the app starts
  app_start_time <- Sys.time()
  
  if(!exists("FullData")) {
    stop("Required data not found. Please ensure data_prep.R runs successfully.")
  }
  
  # Get data file information for last updated
  data_file_info <- file.info("data/AllGifts.CSV")
  data_last_modified <- data_file_info$mtime
  
  # Output the last updated text
  output$lastUpdatedText <- renderText({
    paste("Data last updated:", format(data_last_modified, "%B %d, %Y at %I:%M %p"),
          "| Dashboard refreshed:", format(app_start_time, "%B %d, %Y at %I:%M %p"))
  })
  
  # Filtered data reactive expression
  filtered_data <- reactive({
    data <- FullData
    
    if (!is.null(input$campaignFilter)) {
      data <- data %>% filter(`Campaign ID` == input$campaignFilter)
    }
    
    if (length(input$giftTypeFilter) > 0) {
      data <- data %>% filter(`Gift Type` %in% input$giftTypeFilter)
    }
    
    if (length(input$yearFilter) > 0) {
      data <- data %>% filter(`Fiscal Year` %in% input$yearFilter)
    }
    
    data
  })
  
  # Dynamic heading for the Gift Range Distribution Summary
  output$summaryHeading <- renderUI({
    # Get selected filters
    selected_years <- input$yearFilter
    selected_gift_types <- input$giftTypeFilter
    
    if (length(selected_years) == 0) {
      # No years filter selected, show all fiscal years
      fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
      if (length(fiscal_years) >= 2) {
        # Check if years are consecutive
        if (all(diff(as.numeric(fiscal_years)) == 1)) {
          heading <- paste("Gift Range Distribution Summary -", 
                           fiscal_years[1], "to", tail(fiscal_years, 1), 
                           "Fiscal Years")
        } else {
          # Non-consecutive years
          heading <- paste("Gift Range Distribution Summary -", 
                           paste(fiscal_years, collapse = ", "), 
                           "Fiscal Years")
        }
      } else if (length(fiscal_years) == 1) {
        heading <- paste("Gift Range Distribution Summary -", fiscal_years[1], "Fiscal Year")
      } else {
        heading <- "Gift Range Distribution Summary"
      }
    } else {
      # Years filter selected
      selected_years <- sort(selected_years)
      if (length(selected_years) >= 2) {
        # Check if selected years are consecutive
        if (all(diff(as.numeric(selected_years)) == 1)) {
          heading <- paste("Gift Range Distribution Summary -", 
                           selected_years[1], "to", tail(selected_years, 1), 
                           "Fiscal Years")
        } else {
          # Non-consecutive selected years
          heading <- paste("Gift Range Distribution Summary -", 
                           paste(selected_years, collapse = ", "), 
                           "Fiscal Years")
        }
      } else if (length(selected_years) == 1) {
        heading <- paste("Gift Range Distribution Summary -", selected_years, "Fiscal Year")
      } else {
        heading <- "Gift Range Distribution Summary - Filtered Data"
      }
    }
    
    # If gift types are filtered, add that information
    if (length(selected_gift_types) > 0) {
      if (length(selected_gift_types) < length(levels(FullData$`Gift Type`))) {
        heading <- paste0(heading, " (", 
                          paste(selected_gift_types, collapse = ", "), 
                          " only)")
      }
    }
    
    h4(heading)
  })
  
  # Summary modules content - This would ideally be moved to separate module files
  
  # Donor Levels Summary
  output$donorLevelsSummary <- renderUI({
    # Get donor level data
    donor_data <- process_donor_levels_summary(filtered_data())
    
    if (nrow(donor_data) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Donor Level</th>',
                         '<th class="text-right"># of Donors</th>',
                         '<th class="text-right">Total Amount</th>',
                         '<th class="text-right">% of Total</th>',
                         '</tr></thead><tbody>')
    
    # Calculate grand total for percentage calculation
    grand_total <- sum(donor_data$Total_Amount, na.rm = TRUE)
    total_donors <- sum(donor_data$Number_of_Donors, na.rm = TRUE)
    
    # Add rows for each donor level
    for(i in 1:nrow(donor_data)) {
      row <- donor_data[i, ]
      percent <- sprintf("%.1f%%", (row$Total_Amount / grand_total) * 100)
      
      html_table <- paste0(html_table, '<tr>',
                           '<td>', row$Donor_Level, '</td>',
                           '<td class="text-right">', row$Number_of_Donors, '</td>',
                           '<td class="text-right">', format_currency(row$Total_Amount), '</td>',
                           '<td class="text-right">', percent, '</td>',
                           '</tr>')
    }
    
    # Add totals row
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Total</th>',
                         '<th class="text-right">', total_donors, '</th>',
                         '<th class="text-right">', format_currency(grand_total), '</th>',
                         '<th class="text-right">100.0%</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
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
    
    # Process the gift distribution data for the summary
    dist_data <- process_gift_distribution_data(filtered_data())
    
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
        amount <- format_currency(row$Total_Amount)
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$Gift_Range, '</td>',
                             '<td class="text-right">', gift_count, '</td>',
                             '<td class="text-right">', amount, '</td>',
                             '</tr>')
      }
      
      # Add totals row
      total_gifts <- sum(dist_data$Total_Gifts, na.rm = TRUE)
      total_amount <- sum(dist_data$Total_Amount, na.rm = TRUE)
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
        amount <- if(amount_col %in% names(row)) format_currency(row[[amount_col]]) else "$0"
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$Gift_Range, '</td>',
                             '<td class="text-right">', gift_count, '</td>',
                             '<td class="text-right">', amount, '</td>',
                             '</tr>')
      }
      
      # Add totals row
      total_gifts <- sum(dist_data[[gift_count_col]], na.rm = TRUE)
      total_amount <- sum(dist_data[[amount_col]], na.rm = TRUE)
    }
    
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Total</th>',
                         '<th class="text-right">', total_gifts, '</th>',
                         '<th class="text-right">', format_currency(total_amount), '</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
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
    fund_data <- process_fund_split_data(filtered_data()) %>%
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
  
  # Fund Analysis Summary
  output$fundAnalysisSummary <- renderUI({
    # Get selected fiscal years
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Process the fund analysis data for summary (top 5 funds)
    fund_data <- process_fund_analysis_data(filtered_data()) %>%
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
    overall_total <- sum(process_fund_analysis_data(filtered_data())$Total, na.rm = TRUE)
    
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
  
  # Constituents Summary
  output$constituentsSummary <- renderUI({
    # Get selected fiscal years
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Process the unique constituents data
    const_data <- process_unique_constituents_data(filtered_data())
    
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
  
  # Average Gift Size Summary
  output$avgGiftSummary <- renderUI({
    # Get selected fiscal years
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Process the average gift size data
    avg_data <- process_avg_gift_data(filtered_data())
    
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
  
  # Top Donors Summary
  output$topDonorsSummary <- renderUI({
    # Get selected fiscal years
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Process the top donors data (top 5)
    donors_data <- process_top_donors_data(filtered_data()) %>%
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
    overall_total <- sum(process_top_donors_data(filtered_data())$Total, na.rm = TRUE)
    
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
  
  # Full Data Summary
  output$fullDataSummary <- renderUI({
    # Get data
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Calculate summary statistics
    total_gifts <- nrow(data)
    total_amount <- sum(data$`Fund Split Amount`, na.rm = TRUE)
    total_constituents <- n_distinct(data$`Constituent ID`)
    avg_gift_size <- total_amount / total_gifts
    
    # Get fiscal year range
    fiscal_years <- sort(unique(data$`Fiscal Year`))
    year_range <- ifelse(length(fiscal_years) > 1, 
                         paste(fiscal_years[1], "to", tail(fiscal_years, 1)),
                         fiscal_years[1])
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Metric</th>',
                         '<th class="text-right">Value</th>',
                         '</tr></thead><tbody>')
    
    # Add summary rows
    html_table <- paste0(html_table, '<tr><td>Fiscal Year(s)</td><td class="text-right">', year_range, '</td></tr>')
    html_table <- paste0(html_table, '<tr><td>Total Number of Gifts</td><td class="text-right">', format(total_gifts, big.mark = ","), '</td></tr>')
    html_table <- paste0(html_table, '<tr><td>Total Gift Amount</td><td class="text-right">', format_currency(total_amount), '</td></tr>')
    html_table <- paste0(html_table, '<tr><td>Unique Constituents</td><td class="text-right">', format(total_constituents, big.mark = ","), '</td></tr>')
    html_table <- paste0(html_table, '<tr><td>Average Gift Size</td><td class="text-right">', format_currency(avg_gift_size), '</td></tr>')
    
    html_table <- paste0(html_table, '</tbody></table>')
    
    HTML(html_table)
  })
  
  # Table renderers for the different tabs
  
  # Donor Count Table
  output$donorCountTable <- renderDT({
    data <- process_donor_levels_data(filtered_data())
    
    if (nrow(data) == 0 || ncol(data) <= 1) {
      return(datatable(data.frame(Message = "No data available for the selected filters."),
                       options = list(dom = 't')))
    }
    
    # Extract only donor count columns
    donor_cols <- c("Donor_Level", grep("^Donors_", names(data), value = TRUE), "Total_Donors")
    display_data <- data[, donor_cols]
    
    # Rename columns for better readability
    col_names <- names(display_data)
    for (i in seq_along(col_names)) {
      if (startsWith(col_names[i], "Donors_")) {
        fiscal_year <- substr(col_names[i], 8, nchar(col_names[i]))
        col_names[i] <- fiscal_year
      }
    }
    names(display_data) <- col_names
    
    # Rename the total column
    names(display_data)[names(display_data) == "Total_Donors"] <- "Total"
    
    # Create datatable
    create_datatable(display_data)
  })
  
  # Donor Amount Table
  output$donorAmountTable <- renderDT({
    data <- process_donor_levels_data(filtered_data())
    
    if (nrow(data) == 0 || ncol(data) <= 1) {
      return(datatable(data.frame(Message = "No data available for the selected filters."),
                       options = list(dom = 't')))
    }
    
    # Extract only amount columns
    amount_cols <- c("Donor_Level", grep("^Amount_", names(data), value = TRUE), "Total_Amount")
    display_data <- data[, amount_cols]
    
    # Rename columns for better readability
    col_names <- names(display_data)
    for (i in seq_along(col_names)) {
      if (startsWith(col_names[i], "Amount_")) {
        fiscal_year <- substr(col_names[i], 8, nchar(col_names[i]))
        col_names[i] <- fiscal_year
      }
    }
    names(display_data) <- col_names
    
    # Rename the total column
    names(display_data)[names(display_data) == "Total_Amount"] <- "Total"
    
    # All columns except Donor_Level should be formatted as currency
    currency_cols <- which(names(display_data) != "Donor_Level") - 1
    
    # Create datatable
    create_datatable(display_data, currency_cols)
  })
  
  # Gift Count Table renderer
  output$giftCountTable <- renderDT({
    data <- process_gift_distribution_data(filtered_data())
    
    # Get column names for gift counts
    gift_count_cols <- grep("^Number_of_Gifts_", names(data), value = TRUE)
    
    # Create a display data frame with just the count columns
    display_data <- data %>%
      select(Gift_Range, all_of(gift_count_cols), Total_Gifts)
    
    # Rename columns to remove prefixes and use cleaner names
    new_names <- names(display_data)
    
    # Clean up gift count column names
    for (col in gift_count_cols) {
      year <- gsub("Number_of_Gifts_", "", col)
      new_col_name <- year
      new_names[which(names(display_data) == col)] <- new_col_name
    }
    
    # Rename total column
    new_names[which(names(display_data) == "Total_Gifts")] <- "Total"
    
    # Apply new column names
    names(display_data) <- new_names
    
    # Determine which columns should be right-aligned (numbers)
    number_cols <- which(names(display_data) != "Gift_Range") - 1
    
    # Create the datatable with appropriate formatting
    datatable(
      display_data,
      caption = "Number of Gifts by Gift Range",
      options = list(
        pageLength = 15,  # Show all gift ranges on one page
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'lfrtip',
        columnDefs = list(
          list(targets = number_cols, className = 'dt-right')
        ),
        language = list(search = "Search:")
      ),
      class = 'cell-border stripe',
      filter = 'none',
      selection = 'none'
    )
  })
  
  # Gift Amount Table renderer
  output$giftAmountTable <- renderDT({
    data <- process_gift_distribution_data(filtered_data())
    
    # Get column names for amounts
    amount_cols <- grep("^Total_Amount_", names(data), value = TRUE)
    
    # Create a display data frame with just the amount columns
    display_data <- data %>%
      select(Gift_Range, all_of(amount_cols), Total_Amount)
    
    # Format currency values
    for (col in c(amount_cols, "Total_Amount")) {
      display_data[[col]] <- format_currency(display_data[[col]])
    }
    
    # Rename columns to remove prefixes and use cleaner names
    new_names <- names(display_data)
    
    # Clean up amount column names
    for (col in amount_cols) {
      year <- gsub("Total_Amount_", "", col)
      new_col_name <- year
      new_names[which(names(display_data) == col)] <- new_col_name
    }
    
    # Rename total column
    new_names[which(names(display_data) == "Total_Amount")] <- "Total"
    
    # Apply new column names
    names(display_data) <- new_names
    
    # All columns except Gift_Range should be right-aligned
    currency_cols <- which(names(display_data) != "Gift_Range") - 1
    
    # Create the data table with appropriate formatting
    datatable(
      display_data,
      caption = "Gift Amount by Gift Range",
      options = list(
        pageLength = 15,  # Show all gift ranges on one page
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'lfrtip',
        columnDefs = list(
          list(targets = currency_cols, className = 'dt-right')
        ),
        language = list(search = "Search:")
      ),
      class = 'cell-border stripe',
      filter = 'none',
      selection = 'none'
    )
  })
  
  # Simple Table Renderers
  output$summaryTable <- renderDT({
    data <- process_summary_data(filtered_data())
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$fundSplitTable <- renderDT({
    data <- process_fund_split_data(filtered_data())
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$fundAnalysisTable <- renderDT({
    data <- process_fund_analysis_data(filtered_data())
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$uniqueConstituentsTable <- renderDT({
    create_datatable(process_unique_constituents_data(filtered_data()))
  })
  
  output$topDonorsTable <- renderDT({
    data <- process_top_donors_data(filtered_data())
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$avgGiftTable <- renderDT({
    data <- process_avg_gift_data(filtered_data())
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$fullDataTable <- renderDT({
    data <- filtered_data() %>% arrange(`Fiscal Year`)
    currency_cols <- which(names(data) %in% c("Fund Split Amount", "Gift Amount", 
                                              "Gift Receipt Amount", "Gift Pledge Balance")) - 1
    create_datatable(data, currency_cols)
  })
  
  # Download handlers
  downloads <- list(
    summary = create_download_handlers("summary_data", function() process_summary_data(filtered_data()), session),
    fundSplit = create_download_handlers("fund_split_data", function() process_fund_split_data(filtered_data()), session),
    fundAnalysis = create_download_handlers("fund_analysis_data", function() process_fund_analysis_data(filtered_data()), session),
    constituents = create_download_handlers("unique_constituents_data", function() process_unique_constituents_data(filtered_data()), session),
    topDonors = create_download_handlers("top_donors_data", function() process_top_donors_data(filtered_data()), session),
    avgGift = create_download_handlers("avg_gift_size_data", function() process_avg_gift_data(filtered_data()), session),
    giftDist = create_download_handlers("gift_distribution_data", function() process_gift_distribution_data(filtered_data()), session),
    donorLevels = create_download_handlers("donor_levels_data", function() process_donor_levels_data(filtered_data()), session),
    fullData = create_download_handlers("full_data", filtered_data, session)
  )
  
  # Assign download handlers to outputs
  mapply(function(prefix, handlers) {
    output[[paste0("download", prefix, "_csv")]] <- handlers$csv
    output[[paste0("download", prefix, "_excel")]] <- handlers$excel
  },
  c("Summary", "FundSplit", "FundAnalysis", "Constituents", "TopDonors", "AvgGift", "GiftDist", "DonorLevels", "FullData"),
  downloads)
}