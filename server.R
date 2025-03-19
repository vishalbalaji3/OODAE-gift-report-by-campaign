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
  
  # FILTERED DATA FOR DATA TABLES SECTION
  filtered_data_tables <- reactive({
    data <- FullData
    
    if (!is.null(input$dataTabFilter_campaign)) {
      data <- data %>% filter(`Campaign ID` == input$dataTabFilter_campaign)
    }
    
    if (length(input$dataTabFilter_giftType) > 0) {
      data <- data %>% filter(`Gift Type` %in% input$dataTabFilter_giftType)
    }
    
    if (length(input$dataTabFilter_year) > 0) {
      data <- data %>% filter(`Fiscal Year` %in% input$dataTabFilter_year)
    }
    
    data
  })
  
  # Fiscal years calculation for data tables
  fiscal_years_tables <- reactive({
    sort(unique(filtered_data_tables()$`Fiscal Year`))
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
    
    if (length(input$vizFilter_year) > 0) {
      data <- data %>% filter(`Fiscal Year` %in% input$vizFilter_year)
    }
    
    data
  })
  
  # Fiscal years calculation for visualizations
  fiscal_years_viz <- reactive({
    sort(unique(filtered_data_viz()$`Fiscal Year`))
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
  
  # Initialize data table modules with their reactive values
  summaryStatisticsServer("summary", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  fundSplitServer("fundSplit", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  fundAnalysisServer("fundAnalysis", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  constituentsServer("constituents", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  avgGiftServer("avgGift", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  topDonorsServer("topDonors", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  giftDistServer("giftDist", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  donorLevelsServer("donorLevels", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  fullDataServer("fullData", filtered_data_tables, fiscal_years_tables, summary_stats_tables)
  
  # Initialize visualization modules (to be implemented later)
  # Example: donorAnalysisServer("donor_analysis", filtered_data_viz, fiscal_years_viz, summary_stats_viz)
  
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
      if (length(input$vizFilter_year) > 0) {
        updateSelectInput(session, "dataTabFilter_year", selected = input$vizFilter_year)
      }
    } else if (input$mainNav == "Visualizations") {
      # When switching to Visualizations, update its filters based on Data Tables filters
      if (!is.null(input$dataTabFilter_campaign)) {
        updateSelectInput(session, "vizFilter_campaign", selected = input$dataTabFilter_campaign)
      }
      if (length(input$dataTabFilter_giftType) > 0) {
        updateSelectInput(session, "vizFilter_giftType", selected = input$dataTabFilter_giftType)
      }
      if (length(input$dataTabFilter_year) > 0) {
        updateSelectInput(session, "vizFilter_year", selected = input$dataTabFilter_year)
      }
    }
  })
}