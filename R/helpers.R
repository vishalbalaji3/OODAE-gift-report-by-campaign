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
  # Get all numeric columns (0-indexed for DT)
  numeric_cols_0 <- which(sapply(data, is.numeric)) - 1

  # Create base datatable options
  dt_options <- list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = 'lfrtip',
      columnDefs = list(list(targets = numeric_cols_0, className = 'dt-right')),
      language = list(search = "Search:")
  )

  # Create the base datatable without pre-formatting data
  dt <- datatable(
    data,
    options = dt_options,
    class = 'table table-striped table-bordered',
    filter = 'none',
    selection = 'none',
    rownames = FALSE
  )

  # Apply formatting to numeric columns while preserving data types for sorting
  if (length(numeric_cols_0) > 0) {
    # Apply currency formatting to specified currency columns
    if (!is.null(currency_cols) && length(currency_cols) > 0) {
      # Convert to 1-based indexing for formatCurrency
      currency_cols_1 <- currency_cols + 1
      # Only format columns that exist and are numeric
      valid_currency_cols <- currency_cols_1[currency_cols_1 <= ncol(data) &
                                              currency_cols_1 %in% (numeric_cols_0 + 1)]
      if (length(valid_currency_cols) > 0) {
        dt <- dt %>% formatCurrency(valid_currency_cols, currency = "$", digits = 0)
      }
    }

    # Apply number formatting to remaining numeric columns (non-currency)
    non_currency_numeric_cols <- numeric_cols_0 + 1  # Convert to 1-based
    if (!is.null(currency_cols) && length(currency_cols) > 0) {
      # Remove currency columns from numeric formatting
      non_currency_numeric_cols <- setdiff(non_currency_numeric_cols, currency_cols + 1)
    }

    if (length(non_currency_numeric_cols) > 0) {
      # Format numbers with commas, no decimal places for integers
      dt <- dt %>% formatRound(non_currency_numeric_cols, digits = 0, mark = ",")
    }
  }

  return(dt)
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
