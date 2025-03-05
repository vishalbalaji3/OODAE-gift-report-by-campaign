# Shared processing functions to reduce redundant calculations

# Common function to process data by group (handles multiple processing patterns)
process_data_by_group <- function(filtered_data, group_cols, value_col = "Fund Split Amount", 
                                  include_total = TRUE, sort_by_total = TRUE) {
  # Get fiscal years once
  fiscal_years <- sort(unique(filtered_data$`Fiscal Year`))
  
  # Build the grouping expression dynamically
  group_vars <- c(group_cols, "Fiscal Year")
  
  # Do the grouping, summarizing, and pivoting in one pipeline
  result <- filtered_data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(Total_Value = sum(!!sym(value_col), na.rm = TRUE),
              .groups = 'drop') %>%
    pivot_wider(names_from = "Fiscal Year",
                values_from = "Total_Value",
                values_fill = 0)
  
  # Add total column if requested
  if(include_total) {
    year_cols <- intersect(names(result), fiscal_years)
    result <- result %>%
      mutate(Total = rowSums(select(., all_of(year_cols)), na.rm = TRUE))
  }
  
  # Sort by total if requested
  if(sort_by_total && "Total" %in% names(result)) {
    result <- result %>% arrange(desc(Total))
  }
  
  # Return the final result with columns in proper order
  if(include_total) {
    result <- result %>% select(all_of(group_cols), all_of(fiscal_years), Total)
  } else {
    result <- result %>% select(all_of(group_cols), all_of(fiscal_years))
  }
  
  return(result)
}

# Create an HTML summary table (reduces duplication in UI rendering)
# In R/shared_processing.R
# Update create_summary_table to handle overall_total parameter
create_summary_table <- function(data, id_col, value_col, percent_col = NULL, 
                                 total_row = TRUE, format_fn = format_currency,
                                 overall_total = NULL) {
  # Start building the HTML table
  html_table <- '<table class="table table-striped table-bordered">'
  
  # Create headers
  headers <- c(id_col, value_col)
  if(!is.null(percent_col)) headers <- c(headers, percent_col)
  
  html_table <- paste0(html_table, '<thead><tr>')
  for(header in headers) {
    class <- if(header != id_col) 'class="text-right"' else ''
    html_table <- paste0(html_table, '<th ', class, '>', header, '</th>')
  }
  html_table <- paste0(html_table, '</tr></thead><tbody>')
  
  # Calculate total if needed for percentages
  value_col_idx <- which(names(data) == "Total")
  id_col_idx <- which(names(data) == id_col)
  
  grand_total <- sum(data$Total, na.rm = TRUE)
  
  # Use overall_total if provided, otherwise grand_total
  total_for_percent <- if(!is.null(overall_total)) overall_total else grand_total
  
  # Add data rows
  for(i in 1:min(nrow(data), 5)) { # Show only top 5 rows in summary
    row <- data[i, ]
    
    # Format value
    formatted_value <- format_fn(as.numeric(row[[value_col_idx]]))
    
    # Build row HTML
    html_table <- paste0(html_table, '<tr>',
                         '<td>', row[[id_col_idx]], '</td>',
                         '<td class="text-right">', formatted_value, '</td>')
    
    # Add percent column if requested
    if(!is.null(percent_col)) {
      percent <- sprintf("%.1f%%", (row[[value_col_idx]] / total_for_percent) * 100)
      html_table <- paste0(html_table, '<td class="text-right">', percent, '</td>')
    }
    
    html_table <- paste0(html_table, '</tr>')
  }
  
  # Add total row if requested
  if(total_row) {
    total_label <- if(!is.null(overall_total) && overall_total > grand_total) 
      "Total (Top 5)" else "Total"
    
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>', total_label, '</th>',
                         '<th class="text-right">', format_fn(grand_total), '</th>')
    
    if(!is.null(percent_col)) {
      percent_total <- if(!is.null(overall_total)) 
        sprintf("%.1f%%", (grand_total / total_for_percent) * 100)
      else "100.0%"
      html_table <- paste0(html_table, '<th class="text-right">', percent_total, '</th>')
    }
    
    html_table <- paste0(html_table, '</tr>')
  }
  
  html_table <- paste0(html_table, '</tbody></table>')
  
  return(HTML(html_table))
}