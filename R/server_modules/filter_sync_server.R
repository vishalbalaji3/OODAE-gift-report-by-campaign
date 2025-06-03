# Filter Synchronization Server Module
# Handles synchronization of filters between different tabs

#' Initialize Filter Synchronization
#'
#' Sets up observers to synchronize filters between Data Tables and Visualizations tabs
#'
#' @param input Shiny input object
#' @param session Shiny session object
initializeFilterSynchronization <- function(input, session) {
  # Synchronize filters between tabs to maintain consistency
  observeEvent(input$mainNav, {
    if (input$mainNav == "Data Tables") {
      # When switching to Data Tables, update its filters based on Visualization filters
      syncFiltersToDataTables(input, session)
    } else if (input$mainNav == "Visualizations") {
      # When switching to Visualizations, update its filters based on Data Tables filters
      syncFiltersToVisualizations(input, session)
    }
  })
}

#' Synchronize filters to Data Tables tab
#'
#' Updates Data Tables filters based on current Visualization filters
#'
#' @param input Shiny input object
#' @param session Shiny session object
syncFiltersToDataTables <- function(input, session) {
  if (!is.null(input$vizFilter_campaign)) {
    updateSelectInput(session, "dataTabFilter_campaign", selected = input$vizFilter_campaign)
  }
  if (length(input$vizFilter_giftType) > 0) {
    updateSelectInput(session, "dataTabFilter_giftType", selected = input$vizFilter_giftType)
  }
  updateRadioButtons(session, "dataTabFilter_timeframe", selected = input$vizFilter_timeframe)
  # Year filter will update automatically via the reactive UI
}

#' Synchronize filters to Visualizations tab
#'
#' Updates Visualization filters based on current Data Tables filters
#'
#' @param input Shiny input object
#' @param session Shiny session object
syncFiltersToVisualizations <- function(input, session) {
  if (!is.null(input$dataTabFilter_campaign)) {
    updateSelectInput(session, "vizFilter_campaign", selected = input$dataTabFilter_campaign)
  }
  if (length(input$dataTabFilter_giftType) > 0) {
    updateSelectInput(session, "vizFilter_giftType", selected = input$dataTabFilter_giftType)
  }
  updateRadioButtons(session, "vizFilter_timeframe", selected = input$dataTabFilter_timeframe)
  # Year filter will update automatically via the reactive UI
}
