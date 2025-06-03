# Server Definition
server <- function(input, output, session) {
  # Create a timestamp when the app starts
  app_start_time <- Sys.time()
  
  if(!exists("FullData")) {
    stop("Required data not found. Please ensure data_prep.R runs successfully.")
  }
  
  # Initialize the centralized filter manager
  filter_mgr <- create_filter_manager()
  
  # Get data file information for last updated
  data_file_info <- file.info("data/AllGifts.CSV")
  data_last_modified <- data_file_info$mtime
  
  # Output the last updated text using date format from config
  output$lastUpdatedText <- renderText({
    paste("Data last updated:", format(data_last_modified, config$date_format$display),
          "| Dashboard refreshed:", format(app_start_time, config$date_format$display))
  })
  
  # ========== REACTIVE CONTEXTS ==========
  # Create reactive contexts for both data tables and visualizations
  tables_context <- create_filter_context(filter_mgr, input, "tables")
  viz_context <- create_filter_context(filter_mgr, input, "viz")
  
  # ========== DYNAMIC UI FOR YEAR SELECTION ==========
  
  # Dynamic UI for year selection in data tables section
  output$dataTabFilter_yearUI <- renderUI({
    label <- filter_mgr$create_year_label(input$dataTabFilter_timeframe)
    
    selectInput("dataTabFilter_year", 
                label = label,
                choices = tables_context$available_years(),
                multiple = TRUE)
  })
  
  # Dynamic UI for year selection in visualizations section
  output$vizFilter_yearUI <- renderUI({
    label <- filter_mgr$create_year_label(input$vizFilter_timeframe)
    
    selectInput("vizFilter_year", 
                label = label,
                choices = viz_context$available_years(),
                multiple = TRUE)
  })
  
  # ========== MODULE INITIALIZATION ==========
  
  # Initialize data table modules with simplified parameters
  summaryStatisticsServer("summary", 
                         tables_context$filtered_data, 
                         tables_context$fiscal_years, 
                         tables_context$summary_stats, 
                         tables_context$timeframe)
                         
  fundSplitServer("fundSplit", 
                 tables_context$filtered_data, 
                 tables_context$fiscal_years, 
                 tables_context$summary_stats, 
                 tables_context$timeframe)
                 
  fundAnalysisServer("fundAnalysis", 
                    tables_context$filtered_data, 
                    tables_context$fiscal_years, 
                    tables_context$summary_stats, 
                    tables_context$timeframe)
                    
  constituentsServer("constituents", 
                    tables_context$filtered_data, 
                    tables_context$fiscal_years, 
                    tables_context$summary_stats, 
                    tables_context$timeframe)
                    
  avgGiftServer("avgGift", 
               tables_context$filtered_data, 
               tables_context$fiscal_years, 
               tables_context$summary_stats, 
               tables_context$timeframe)
               
  topDonorsServer("topDonors", 
                 tables_context$filtered_data, 
                 tables_context$fiscal_years, 
                 tables_context$summary_stats, 
                 tables_context$timeframe)
                 
  giftDistServer("giftDist", 
                tables_context$filtered_data, 
                tables_context$fiscal_years, 
                tables_context$summary_stats, 
                tables_context$timeframe)
                
  donorLevelsServer("donorLevels", 
                   tables_context$filtered_data, 
                   tables_context$fiscal_years, 
                   tables_context$summary_stats, 
                   tables_context$timeframe)
                   
  fullDataServer("fullData", 
                tables_context$filtered_data, 
                tables_context$fiscal_years, 
                tables_context$summary_stats, 
                tables_context$timeframe)
  
  # Initialize visualization modules
  donorPyramidServer("donor_pyramid", 
                    viz_context$filtered_data, 
                    viz_context$fiscal_years, 
                    viz_context$summary_stats, 
                    viz_context$timeframe)
                    
  donorRetentionServer("donor_retention", 
                      viz_context$filtered_data, 
                      viz_context$fiscal_years, 
                      viz_context$summary_stats, 
                      viz_context$timeframe)
  
  # ========== FILTER SYNCHRONIZATION ==========
  # Synchronize filters between tabs to maintain consistency
  observeEvent(input$mainNav, {
    if (input$mainNav == "Data Tables") {
      # When switching to Data Tables, update its filters based on Visualization filters
      if (!is.null(input$vizFilter_campaign)) {
        updateSelectInput(session, "dataTabFilter_campaign", selected = input$vizFilter_campaign)
      }
      if (length(input$vizFilter_giftType) > 0) {
        updateSelectInput(session, "dataTabFilter_giftType", selected = input$vizFilter_giftType)
      }
      updateRadioButtons(session, "dataTabFilter_timeframe", selected = input$vizFilter_timeframe)
      # Year filter will update automatically via the reactive UI
    } else if (input$mainNav == "Visualizations") {
      # When switching to Visualizations, update its filters based on Data Tables filters
      if (!is.null(input$dataTabFilter_campaign)) {
        updateSelectInput(session, "vizFilter_campaign", selected = input$dataTabFilter_campaign)
      }
      if (length(input$dataTabFilter_giftType) > 0) {
        updateSelectInput(session, "vizFilter_giftType", selected = input$dataTabFilter_giftType)
      }
      updateRadioButtons(session, "vizFilter_timeframe", selected = input$dataTabFilter_timeframe)
      # Year filter will update automatically via the reactive UI
    }
  })
}