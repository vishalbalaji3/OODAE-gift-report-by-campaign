# Helper Functions

# Format currency values with abbreviations for large numbers
format_currency <- function(x) {
  ifelse(
    is.na(x) | x == 0,
    "$0",
    ifelse(
      abs(x) >= 1e6,
      sprintf("$%.1fM", x / 1e6),
      sprintf("$%s", format(round(x, 0), big.mark = ","))
    )
  )
}

# Create standardized data tables
create_datatable <- function(data, currency_cols = NULL) {
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
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = 'lfrtip',
      columnDefs = list(list(targets = currency_cols, className = 'dt-right')),
      language = list(search = "Search:")
    ),
    class = 'cell-border stripe',
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