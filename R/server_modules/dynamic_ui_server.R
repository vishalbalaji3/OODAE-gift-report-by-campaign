# Dynamic UI Server Module
# Handles generation of dynamic UI elements

#' Initialize Dynamic UI
#'
#' Sets up dynamic UI elements like year selection dropdowns
#'
#' @param output Shiny output object
#' @param input Shiny input object
#' @param filter_mgr Filter manager instance
#' @param tables_context Filter context for data tables
#' @param viz_context Filter context for visualizations
initializeDynamicUI <- function(output, input, filter_mgr, tables_context, viz_context) {
  # Dynamic UI for year selection in data tables section
  output$dataTabFilter_yearUI <- renderUI({
    createYearUI("dataTabFilter_year", input$dataTabFilter_timeframe, filter_mgr, tables_context)
  })

  # Dynamic UI for year selection in visualizations section
  output$vizFilter_yearUI <- renderUI({
    createYearUI("vizFilter_year", input$vizFilter_timeframe, filter_mgr, viz_context)
  })
}

#' Create Year Selection UI
#'
#' Creates a year selection dropdown based on timeframe and context
#'
#' @param input_id The input ID for the year selection
#' @param timeframe The current timeframe selection
#' @param filter_mgr Filter manager instance
#' @param context Filter context
#' @return A selectInput UI element
createYearUI <- function(input_id, timeframe, filter_mgr, context) {
  label <- filter_mgr$create_year_label(timeframe)

  selectInput(input_id,
              label = label,
              choices = context$available_years(),
              multiple = TRUE)
}
