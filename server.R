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
  
  # Output the last updated text using date format from config
  output$lastUpdatedText <- renderText({
    paste("Data last updated:", format(data_last_modified, config$date_format$display),
          "| Dashboard refreshed:", format(app_start_time, config$date_format$display))
  })
  
  # Generate available years for data tables based on selected time period
  available_years_tables <- reactive({
    if(input$dataTabFilter_timeframe == "calendar") {
      sort(unique(format(as.Date(FullData$`Gift Date`), "%Y")))
    } else {
      sort(unique(FullData$`Fiscal Year`))
    }
  })
  
  # Dynamic UI for year selection in data tables section with proper capitalization
  output$dataTabFilter_yearUI <- renderUI({
    # Create a properly capitalized label based on time period
    if(input$dataTabFilter_timeframe == "calendar") {
      label <- "Calendar Year:"
    } else {
      label <- "Fiscal Year:" 
    }
    
    selectInput("dataTabFilter_year", 
                label = label,
                choices = available_years_tables(),
                multiple = TRUE)
  })
  
  # Generate available years for visualizations based on selected time period
  available_years_viz <- reactive({
    if(input$vizFilter_timeframe == "calendar") {
      sort(unique(format(as.Date(FullData$`Gift Date`), "%Y")))
    } else {
      sort(unique(FullData$`Fiscal Year`))
    }
  })
  
  # Dynamic UI for year selection in visualizations section with proper capitalization
  output$vizFilter_yearUI <- renderUI({
    # Create a properly capitalized label based on time period
    if(input$vizFilter_timeframe == "calendar") {
      label <- "Calendar Year:"
    } else {
      label <- "Fiscal Year:"
    }
    
    selectInput("vizFilter_year", 
                label = label,
                choices = available_years_viz(),
                multiple = TRUE)
  })
  
  # FILTERED DATA FOR DATA TABLES SECTION
  filtered_data_tables <- reactive({
    data <- FullData
    
    if (!is.null(input$dataTabFilter_campaign)) {
      data <- data %>% filter(`Campaign ID` == input$dataTabFilter_campaign)
    }
    
    if (length(input$dataTabFilter_giftType) > 0) {
      data <- data %>% filter(`Gift Type` %in% input$dataTabFilter_giftType)
    }
    
    # Filter by years based on selected time period
    if (length(input$dataTabFilter_year) > 0) {
      if(input$dataTabFilter_timeframe == "calendar") {
        # Filter by calendar years
        data <- data %>% 
          filter(format(as.Date(`Gift Date`), "%Y") %in% input$dataTabFilter_year)
      } else {
        # Filter by fiscal years (default)
        data <- data %>% 
          filter(`Fiscal Year` %in% input$dataTabFilter_year)
      }
    }
    
    data
  })
  
  # Fiscal years calculation for data tables - respects time period selection
  fiscal_years_tables <- reactive({
    # This reactive should return the appropriate years based on time period
    if(input$dataTabFilter_timeframe == "calendar") {
      sort(unique(format(as.Date(filtered_data_tables()$`Gift Date`), "%Y")))
    } else {
      sort(unique(filtered_data_tables()$`Fiscal Year`))
    }
  })
  
  # Summary statistics for data tables
  summary_stats_tables <- reactive({
    data <- filtered_data_tables()
    
    if(nrow(data) == 0) {
      return(list(
        total_gifts = 0,
        total_amount = 0,
        total_constituents = 0,
        avg_gift_size = 0
      ))
    }
    
    list(
      total_gifts = nrow(data),
      total_amount = sum(data$`Fund Split Amount`, na.rm = TRUE),
      total_constituents = n_distinct(data$`Constituent ID`),
      avg_gift_size = sum(data$`Fund Split Amount`, na.rm = TRUE) / nrow(data)
    )
  })
  
  # FILTERED DATA FOR VISUALIZATIONS SECTION
  filtered_data_viz <- reactive({
    data <- FullData
    
    if (!is.null(input$vizFilter_campaign)) {
      data <- data %>% filter(`Campaign ID` == input$vizFilter_campaign)
    }
    
    if (length(input$vizFilter_giftType) > 0) {
      data <- data %>% filter(`Gift Type` %in% input$vizFilter_giftType)
    }
    
    # Filter by years based on selected time period
    if (length(input$vizFilter_year) > 0) {
      if(input$vizFilter_timeframe == "calendar") {
        # Filter by calendar years
        data <- data %>% 
          filter(format(as.Date(`Gift Date`), "%Y") %in% input$vizFilter_year)
      } else {
        # Filter by fiscal years (default)
        data <- data %>% 
          filter(`Fiscal Year` %in% input$vizFilter_year)
      }
    }
    
    data
  })
  
  # Fiscal years calculation for visualizations - respects time period selection
  fiscal_years_viz <- reactive({
    # This reactive should return the appropriate years based on time period
    if(input$vizFilter_timeframe == "calendar") {
      sort(unique(format(as.Date(filtered_data_viz()$`Gift Date`), "%Y")))
    } else {
      sort(unique(filtered_data_viz()$`Fiscal Year`))
    }
  })
  
  # Summary statistics for visualizations
  summary_stats_viz <- reactive({
    data <- filtered_data_viz()
    
    if(nrow(data) == 0) {
      return(list(
        total_gifts = 0,
        total_amount = 0,
        total_constituents = 0,
        avg_gift_size = 0
      ))
    }
    
    list(
      total_gifts = nrow(data),
      total_amount = sum(data$`Fund Split Amount`, na.rm = TRUE),
      total_constituents = n_distinct(data$`Constituent ID`),
      avg_gift_size = sum(data$`Fund Split Amount`, na.rm = TRUE) / nrow(data)
    )
  })
  
  # Add time period setting to be passed to modules
  time_period_tables <- reactive({
    input$dataTabFilter_timeframe
  })
  
  time_period_viz <- reactive({
    input$vizFilter_timeframe
  })
  
  # Initialize data table modules with their reactive values and time period
  summaryStatisticsServer("summary", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  fundSplitServer("fundSplit", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  fundAnalysisServer("fundAnalysis", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  constituentsServer("constituents", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  avgGiftServer("avgGift", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  topDonorsServer("topDonors", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  giftDistServer("giftDist", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  donorLevelsServer("donorLevels", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  fullDataServer("fullData", filtered_data_tables, fiscal_years_tables, summary_stats_tables, reactive({input$dataTabFilter_timeframe}))
  
  
  # Initialize visualization modules
  donorPyramidServer("donor_pyramid", filtered_data_viz, fiscal_years_viz, summary_stats_viz, reactive({input$vizFilter_timeframe}))
  
  # Synchronize filters between tabs (optional)
  # This helps maintain consistent filtering when switching between tabs
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