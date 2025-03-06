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

# Create standardized data tables
create_datatable <- function(data, currency_cols = NULL) {
  # Get table styling settings from config
  table_config <- config$ui$table
  
  display_data <- data
  
  if (!is.null(currency_cols)) {
    display_data <- as.data.frame(lapply(seq_along(display_data), function(i) {
      if ((i-1) %in% currency_cols) format_currency(display_data[[i]]) else display_data[[i]]
    }))
    names(display_data) <- names(data)
  }
  
  datatable(
    display_data,
    options = list(
      pageLength = table_config$pageLength,
      scrollX = table_config$scrollX,
      autoWidth = table_config$autoWidth,
      dom = table_config$dom,
      columnDefs = list(list(targets = currency_cols, className = 'dt-right')),
      language = list(search = "Search:")
    ),
    class = table_config$class,
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