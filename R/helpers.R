# Helper Functions

# Format currency values with abbreviations for large numbers
format_currency <- function(x) {
  # Get currency formatting settings from config
  currency_config <- config$format$currency
  
  # For vectors - process each element
  if(length(x) > 1) {
    return(sapply(x, format_currency))
  }
  
  # Force numeric conversion and handle edge cases
  tryCatch({
    # First try direct conversion
    x_numeric <- as.numeric(x)
    
    # Check if conversion worked
    if(is.na(x_numeric) && !is.na(x)) {
      # If conversion failed but x wasn't NA, try parsing with special handling
      x_str <- as.character(x)
      # Remove any non-numeric characters except decimal point
      x_str <- gsub("[^0-9.-]", "", x_str)
      x_numeric <- as.numeric(x_str)
    }
    
    # Use the converted value
    x <- x_numeric
  }, error = function(e) {
    # If all else fails, return "$0" for unconvertible values
    return("$0")
  })
  
  # Check for NA or zero
  if (is.na(x) || x == 0) return("$0")
  
  # Apply abbreviations based on value size and configuration settings
  if (abs(x) >= currency_config$abbreviateThreshold && isTRUE(currency_config$abbreviateMillions)) {
    # Millions
    return(sprintf(currency_config$millionsFormat, x / 1e6))
  } else if (abs(x) >= currency_config$thousandsThreshold && isTRUE(currency_config$abbreviateThousands)) {
    # Thousands
    return(sprintf(currency_config$thousandsFormat, x / 1e3))
  } else {
    # Regular formatting with commas
    if (isTRUE(currency_config$useThousandsSeparator)) {
      return(sprintf(currency_config$standardFormat, 
                     format(round(x, currency_config$decimalPlaces), big.mark = ",")))
    } else {
      return(sprintf(currency_config$standardFormat, 
                     format(round(x, currency_config$decimalPlaces))))
    }
  }
}

# Format numeric values with abbreviations and separators
format_number <- function(x) {
  # Get numeric formatting settings from config
  numeric_config <- config$format$numeric
  
  # For vectors - process each element
  if(length(x) > 1) {
    return(sapply(x, format_number))
  }
  
  # Force numeric conversion and handle edge cases
  tryCatch({
    # First try direct conversion
    x_numeric <- as.numeric(x)
    
    # Check if conversion worked
    if(is.na(x_numeric) && !is.na(x)) {
      # If conversion failed but x wasn't NA, try parsing with special handling
      x_str <- as.character(x)
      # Remove any non-numeric characters except decimal point
      x_str <- gsub("[^0-9.-]", "", x_str)
      x_numeric <- as.numeric(x_str)
    }
    
    # Use the converted value
    x <- x_numeric
  }, error = function(e) {
    # If all else fails, return "0" for unconvertible values
    return("0")
  })
  
  # Check for NA or zero
  if (is.na(x) || x == 0) return("0")
  
  # Apply abbreviations based on value size and configuration settings
  if (abs(x) >= numeric_config$abbreviateThreshold && isTRUE(numeric_config$abbreviateMillions)) {
    # Millions
    return(sprintf(numeric_config$millionsFormat, x / 1e6))
  } else if (abs(x) >= numeric_config$thousandsThreshold && isTRUE(numeric_config$abbreviateThousands)) {
    # Thousands
    return(sprintf(numeric_config$thousandsFormat, x / 1e3))
  } else {
    # Regular formatting with commas
    if (isTRUE(numeric_config$useThousandsSeparator)) {
      return(sprintf(numeric_config$standardFormat, 
                     format(round(x, numeric_config$decimalPlaces), big.mark = ",")))
    } else {
      return(sprintf(numeric_config$standardFormat, 
                     format(round(x, numeric_config$decimalPlaces))))
    }
  }
}

# Create standardized data tables
create_datatable <- function(data, currency_cols = NULL) {
  # Create a copy of the data for display
  display_data <- data
  
  # Apply formatting to all columns
  for (i in 1:ncol(display_data)) {
    # 0-indexed column for DT
    i_0 <- i - 1
    
    # Check if it's a numeric column
    if (is.numeric(display_data[[i]])) {
      # Check if it's a currency column
      if (!is.null(currency_cols) && (i_0 %in% currency_cols)) {
        display_data[[i]] <- format_currency(display_data[[i]])
      } else {
        # Apply numeric formatting to non-currency numeric columns
        display_data[[i]] <- format_number(display_data[[i]])
      }
    }
  }
  
  # Get all numeric columns (0-indexed for DT)
  numeric_cols_0 <- which(sapply(data, is.numeric)) - 1
  
  # Create datatable with right-aligned numeric columns
  datatable(
    display_data,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = 'lfrtip',
      columnDefs = list(list(targets = numeric_cols_0, className = 'dt-right')),
      language = list(search = "Search:")
    ),
    class = 'table table-striped table-bordered',
    filter = 'none',
    selection = 'none'
  )
}

# Create download handlers for CSV and Excel
create_download_handlers <- function(id, data_reactive, session) {
  output_name <- function(ext) paste0(id, "_", Sys.Date(), ext)
  
  list(
    csv = downloadHandler(
      filename = function() output_name(".csv"),
      content = function(file) write.csv(data_reactive(), file, row.names = FALSE)
    ),
    excel = downloadHandler(
      filename = function() output_name(".xlsx"),
      content = function(file) write_xlsx(data_reactive(), file)
    )
  )
}

# Create download buttons
create_download_buttons <- function(ns, csv_id = "download_csv", excel_id = "download_excel") {
  div(class = "btn-group", style = "margin-top: 20px; margin-bottom: 30px;",
      downloadButton(ns(csv_id), "Download Full CSV", class = "btn-primary"),
      downloadButton(ns(excel_id), "Download Full Excel", class = "btn-primary")
  )
}