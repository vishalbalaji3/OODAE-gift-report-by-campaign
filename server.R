# Server Definition
server <- function(input, output, session) {
  # Create a timestamp when the app starts
  app_start_time <- Sys.time()
  
  if(!exists("FullData")) {
    stop("Required data not found. Please ensure data_prep.R runs successfully.")
  }
  
  # Get data file information for last updated
  data_file_info <- file.info("data/AllGifts.CSV")
  data_last_modified <- data_file_info$mtime
  
  # Output the last updated text
  output$lastUpdatedText <- renderText({
    paste("Data last updated:", format(data_last_modified, "%B %d, %Y at %I:%M %p"),
          "| Dashboard refreshed:", format(app_start_time, "%B %d, %Y at %I:%M %p"))
  })
  
  # Filtered data reactive expression
  filtered_data <- reactive({
    data <- FullData
    
    if (!is.null(input$campaignFilter)) {
      data <- data %>% filter(`Campaign ID` == input$campaignFilter)
    }
    
    if (length(input$giftTypeFilter) > 0) {
      data <- data %>% filter(`Gift Type` %in% input$giftTypeFilter)
    }
    
    if (length(input$yearFilter) > 0) {
      data <- data %>% filter(`Fiscal Year` %in% input$yearFilter)
    }
    
    data
  })
  
  # Initialize modules
  summaryStatisticsServer("summary", filtered_data)
  fundSplitServer("fundSplit", filtered_data)
  fundAnalysisServer("fundAnalysis", filtered_data)
  constituentsServer("constituents", filtered_data)
  avgGiftServer("avgGift", filtered_data)
  topDonorsServer("topDonors", filtered_data)
  giftDistServer("giftDist", filtered_data)
  donorLevelsServer("donorLevels", filtered_data)
  fullDataServer("fullData", filtered_data)
}