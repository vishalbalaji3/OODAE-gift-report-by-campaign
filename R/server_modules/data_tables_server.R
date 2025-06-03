# Data Tables Server Module
# Handles initialization and management of all data table modules

#' Initialize Data Tables Server
#'
#' Initializes all data table modules with the provided context
#'
#' @param context The filter context containing filtered data and parameters
initializeDataTablesServer <- function(context) {
  # Initialize all data table modules with simplified parameters
  summaryStatisticsServer("summary",
                         context$filtered_data,
                         context$fiscal_years,
                         context$summary_stats,
                         context$timeframe)

  fundSplitServer("fundSplit",
                 context$filtered_data,
                 context$fiscal_years,
                 context$summary_stats,
                 context$timeframe)

  fundAnalysisServer("fundAnalysis",
                    context$filtered_data,
                    context$fiscal_years,
                    context$summary_stats,
                    context$timeframe)

  constituentsServer("constituents",
                    context$filtered_data,
                    context$fiscal_years,
                    context$summary_stats,
                    context$timeframe)

  avgGiftServer("avgGift",
               context$filtered_data,
               context$fiscal_years,
               context$summary_stats,
               context$timeframe)

  topDonorsServer("topDonors",
                 context$filtered_data,
                 context$fiscal_years,
                 context$summary_stats,
                 context$timeframe)

  giftDistServer("giftDist",
                context$filtered_data,
                context$fiscal_years,
                context$summary_stats,
                context$timeframe)

  donorLevelsServer("donorLevels",
                   context$filtered_data,
                   context$fiscal_years,
                   context$summary_stats,
                   context$timeframe)

  fullDataServer("fullData",
                context$filtered_data,
                context$fiscal_years,
                context$summary_stats,
                context$timeframe)
}
