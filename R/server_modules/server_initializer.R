# Server Initialization Module
# Coordinates all server modules and provides a clean initialization interface

#' Initialize Server Components
#'
#' Initializes all server components including filters, UI, modules, and synchronization
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param filter_mgr Filter manager instance
#' @param tables_context Filter context for data tables
#' @param viz_context Filter context for visualizations
initializeServerComponents <- function(input, output, session, filter_mgr, tables_context, viz_context) {
  # Initialize dynamic UI components
  initializeDynamicUI(output, input, filter_mgr, tables_context, viz_context)

  # Initialize data table modules
  initializeDataTablesServer(tables_context)

  # Initialize visualization modules
  initializeVisualizationsServer(viz_context)

  # Initialize filter synchronization
  initializeFilterSynchronization(input, session)
}

#' Setup App Metadata
#'
#' Sets up app metadata like timestamps and data information
#'
#' @param output Shiny output object
setupAppMetadata <- function(output) {
  # Create a timestamp when the app starts
  app_start_time <- Sys.time()

  # Get data file information for last updated
  data_file_info <- file.info("data/AllGifts.CSV")
  data_last_modified <- data_file_info$mtime

  # Output the last updated text using date format from config
  output$lastUpdatedText <- renderText({
    paste("Data last updated:", format(data_last_modified, config$date_format$display),
          "| Dashboard refreshed:", format(app_start_time, config$date_format$display))
  })
}
