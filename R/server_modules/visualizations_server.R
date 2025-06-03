# Visualizations Server Module
# Handles initialization and management of all visualization modules

#' Initialize Visualizations Server
#'
#' Initializes all visualization modules with the provided context
#'
#' @param context The filter context containing filtered data and parameters
initializeVisualizationsServer <- function(context) {
  # Initialize visualization modules
  donorPyramidServer("donor_pyramid",
                    context$filtered_data,
                    context$fiscal_years,
                    context$summary_stats,
                    context$timeframe)

  donorRetentionServer("donor_retention",
                      context$filtered_data,
                      context$fiscal_years,
                      context$summary_stats,
                      context$timeframe)
}
