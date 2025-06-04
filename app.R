# =============================================================================
# SIMPLIFIED OODAE GIFT REPORT DASHBOARD
# =============================================================================
# This is the main Shiny application file. It focuses purely on UI and server
# logic, with all data preparation handled by separate scripts.
# 
# Data preparation is handled by: data_prep.R
# Configuration is handled by: config.R  
# Helper functions are handled by: helpers.R
# =============================================================================

# Load required libraries for Shiny app
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(DT)
library(writexl)
library(plotly)

# Source supporting files
source("config.R")
source("helpers.R")
source("data_prep.R")

# =============================================================================
# UI COMPONENTS
# =============================================================================

#' Create filter panel for data selection
#' 
#' @param id_prefix Prefix for input IDs
#' @param campaigns Available campaign choices
#' @param gift_types Available gift type choices
#' @param years Available year choices
#' @return HTML div with filter controls
create_filter_panel <- function(id_prefix, campaigns, gift_types, years) {
  div(
    class = "well",
    h4("Filter Data", class = "text-primary"),
    fluidRow(
      column(3,
             selectInput(paste0(id_prefix, "_campaign"),
                        "Select Campaign ID:",
                        choices = campaigns,
                        selected = campaigns[1])
      ),
      column(3,
             selectInput(paste0(id_prefix, "_giftType"),
                        "Select Gift Type:",
                        choices = gift_types,
                        multiple = TRUE)
      ),
      column(3,
             radioButtons(paste0(id_prefix, "_timeframe"),
                         "Time Period:",
                         choices = c("Fiscal Year" = "fiscal", "Calendar Year" = "calendar"),
                         selected = "fiscal",
                         inline = TRUE)
      ),
      column(3,
             selectInput(paste0(id_prefix, "_year"),
                        "Select Year:",
                        choices = years,
                        multiple = TRUE)
      )
    )
  )
}

# =============================================================================
# USER INTERFACE
# =============================================================================

ui <- fluidPage(
  theme = shinythemes::shinytheme(CONFIG$theme),
  
  # Include CSS styling
  tags$head(
    tags$style(HTML("
      .well { margin-bottom: 20px; }
      .page-header { text-align: center; }
      .info-box { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.375rem; }
      .download-buttons { margin: 15px 0; }
    "))
  ),
  
  # Page title and header
  fluidRow(
    column(12,
      div(class = "page-header",
        style = "margin: 20px 0;",
        h2("OODAE Gift Report Dashboard"),
        div(class = "text-muted", style = "font-size: 12px; font-style: italic;",
          textOutput("lastUpdatedText")
        )
      )
    )
  ),
  
  # Main navigation
  navbarPage(
    title = NULL, 
    id = "mainNav",
    
    # Data Tables Tab
    tabPanel("Data Tables",
      div(id = "dataTabFilter"),
      
      tabsetPanel(
        id = "dataTabset",
        
        # Summary Statistics
        tabPanel("Summary Statistics", 
          fluidRow(column(12, withSpinner(DT::dataTableOutput("summaryTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadSummary", "Download CSV", class = "btn btn-primary btn-sm")
          )))
        ),
        
        # Fund Split by Constituency
        tabPanel("Fund Split by Constituency",
          h4("Summary by Constituency"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("fundSplitSummaryTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadFundSplitSummary", "Download Summary CSV", class = "btn btn-primary btn-sm")
          ))),
          br(),
          h4("Detailed Breakdown by Year"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("fundSplitDetailTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadFundSplit", "Download Detail CSV", class = "btn btn-primary btn-sm")
          )))
        ),
        
        # Fund Analysis
        tabPanel("Fund Analysis",
          h4("Top 5 Funds Summary"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("fundAnalysisSummaryTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadFundAnalysisSummary", "Download Summary CSV", class = "btn btn-primary btn-sm")
          ))),
          br(),
          h4("All Funds by Year"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("fundAnalysisDetailTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadFundAnalysis", "Download Detail CSV", class = "btn btn-primary btn-sm")
          )))
        ),
        
        # Unique Constituents
        tabPanel("Unique Constituents",
          fluidRow(column(12, withSpinner(DT::dataTableOutput("constituentsTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadConstituents", "Download CSV", class = "btn btn-primary btn-sm")
          )))
        ),
        
        # Average Gift Size
        tabPanel("Average Gift Size",
          h4("By Primary Constituency"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("avgGiftConstituencyTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadAvgGiftConstituency", "Download Constituency CSV", class = "btn btn-primary btn-sm")
          ))),
          br(),
          h4("By Gift Type"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("avgGiftTypeTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadAvgGiftType", "Download Gift Type CSV", class = "btn btn-success btn-sm")
          )))
        ),
        
        # Top Donors
        tabPanel("Top Donors",
          fluidRow(column(12, withSpinner(DT::dataTableOutput("topDonorsTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadTopDonors", "Download CSV", class = "btn btn-primary btn-sm")
          )))
        ),
        
        # Gift Size Distribution
        tabPanel("Gift Size Distribution",
          h4("Gift Count by Range"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("giftCountTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadGiftCounts", "Download Counts CSV", class = "btn btn-primary btn-sm")
          ))),
          br(),
          h4("Gift Amount by Range"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("giftAmountTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadGiftAmounts", "Download Amounts CSV", class = "btn btn-success btn-sm")
          )))
        ),
        
        # Donor Levels
        tabPanel("Donor Levels",
          h4("Number of Donors by Level"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("donorCountsTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadDonorCounts", "Download Counts CSV", class = "btn btn-primary btn-sm")
          ))),
          br(),
          h4("Donation Amounts by Level"),
          fluidRow(column(12, withSpinner(DT::dataTableOutput("donorAmountsTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadDonorAmounts", "Download Amounts CSV", class = "btn btn-success btn-sm")
          )))
        ),
        
        # Full Data
        tabPanel("Full Data",
          fluidRow(column(12, withSpinner(DT::dataTableOutput("fullDataTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadFullData", "Download CSV", class = "btn btn-primary btn-sm")
          )))
        )
      )
    ),
    
    # Visualizations Tab (placeholder for future expansion)
    tabPanel("Visualizations",
      div(id = "vizFilter"),
      h3("Visualization Overview"),
      p("Visualization features can be added here as needed.")
    )
  )
)

# =============================================================================
# SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {
  
  # Create a timestamp when the app starts
  app_start_time <- Sys.time()
  
  # Load processed data once at startup
  full_data <- prepare_data()
  
  # Get data file information for last updated
  data_file_info <- file.info(PATHS$raw_gifts)
  data_last_modified <- data_file_info$mtime
  
  # Display last updated information using actual file modification time
  output$lastUpdatedText <- renderText({
    paste("Data last updated:", format(data_last_modified, "%B %d, %Y at %I:%M %p"),
          "| Dashboard refreshed:", format(app_start_time, "%B %d, %Y at %I:%M %p"))
  })
  
  # Extract unique values for filter options
  campaigns <- c("All Campaigns" = "ALL", sort(unique(full_data$`Campaign ID`)))
  gift_types <- sort(unique(full_data$`Gift Type`))
  fiscal_years <- sort(unique(full_data$`Fiscal Year`))
  calendar_years <- sort(unique(format(as.Date(full_data$`Gift Date`), "%Y")))
  
  # Insert filter panels into the UI
  observe({
    insertUI(
      selector = "#dataTabFilter",
      ui = create_filter_panel("dataTab", campaigns, gift_types, fiscal_years)
    )
  })
  
  observe({
    insertUI(
      selector = "#vizFilter", 
      ui = create_filter_panel("viz", campaigns, gift_types, fiscal_years)
    )
  })
  
  # Update year choices based on timeframe selection
  observe({
    timeframe <- input$dataTab_timeframe
    if (!is.null(timeframe)) {
      if (timeframe == "calendar") {
        updateSelectInput(session, "dataTab_year", choices = calendar_years)
      } else {
        updateSelectInput(session, "dataTab_year", choices = fiscal_years)
      }
    }
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    
    apply_data_filters(
      data = full_data,
      campaign = input$dataTab_campaign,
      gift_types = input$dataTab_giftType,
      years = input$dataTab_year,
      timeframe = timeframe
    )
  })
  
  # =============================================================================
  # DATA TABLE OUTPUTS
  # =============================================================================
  
  # Summary statistics table
  output$summaryTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_summary_stats(filtered_data(), timeframe = timeframe)$detailed_data
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Fund split tables
  output$fundSplitSummaryTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_fund_split(filtered_data(), timeframe = timeframe)$summary_table
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fundSplitDetailTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_fund_split(filtered_data(), timeframe = timeframe)$detailed_data
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Fund analysis tables
  output$fundAnalysisSummaryTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_fund_analysis(filtered_data(), timeframe = timeframe)$summary_table
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fundAnalysisDetailTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_fund_analysis(filtered_data(), timeframe = timeframe)$detailed_data
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Constituents table
  output$constituentsTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_unique_constituents(filtered_data(), timeframe = timeframe)
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Average gift size tables
  output$avgGiftConstituencyTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_avg_gift_analysis(filtered_data(), timeframe = timeframe)$by_constituency
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$avgGiftTypeTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_avg_gift_analysis(filtered_data(), timeframe = timeframe)$by_gift_type
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Top donors table
  output$topDonorsTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_top_donors(filtered_data(), timeframe = timeframe)
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Gift distribution tables
  output$giftCountTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_gift_distribution(filtered_data(), timeframe = timeframe)$gift_counts
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$giftAmountTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_gift_distribution(filtered_data(), timeframe = timeframe)$gift_amounts
    
    # Add timeframe prefix to year columns
    prefix <- if (timeframe == "fiscal") "FY " else "CY "
    data <- add_year_prefix(data, prefix)
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Donor levels tables
  output$donorCountsTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_donor_levels(filtered_data(), timeframe = timeframe)$donor_counts
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$donorAmountsTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_donor_levels(filtered_data(), timeframe = timeframe)$donor_amounts
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Full data table
  output$fullDataTable <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # =============================================================================
  # DOWNLOAD HANDLERS
  # =============================================================================
  
  output$downloadSummary <- downloadHandler(
    filename = function() paste("summary_stats_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      summary_data <- calculate_summary_stats(filtered_data(), timeframe = timeframe)
      write.csv(summary_data$detailed_data, file, row.names = FALSE)
    }
  )
  
  output$downloadFundSplit <- downloadHandler(
    filename = function() paste("fund_split_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_fund_split(filtered_data(), timeframe = timeframe)$detailed_data, file, row.names = FALSE)
    }
  )
  
  output$downloadFundSplitSummary <- downloadHandler(
    filename = function() paste("fund_split_summary_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_fund_split(filtered_data(), timeframe = timeframe)$summary_table, file, row.names = FALSE)
    }
  )
  
  output$downloadFundAnalysis <- downloadHandler(
    filename = function() paste("fund_analysis_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_fund_analysis(filtered_data(), timeframe = timeframe)$detailed_data, file, row.names = FALSE)
    }
  )
  
  output$downloadFundAnalysisSummary <- downloadHandler(
    filename = function() paste("fund_analysis_summary_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_fund_analysis(filtered_data(), timeframe = timeframe)$summary_table, file, row.names = FALSE)
    }
  )
  
  output$downloadConstituents <- downloadHandler(
    filename = function() paste("constituents_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_unique_constituents(filtered_data(), timeframe = timeframe), file, row.names = FALSE)
    }
  )
  
  output$downloadAvgGiftConstituency <- downloadHandler(
    filename = function() paste("avg_gift_constituency_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_avg_gift_analysis(filtered_data(), timeframe = timeframe)$by_constituency, file, row.names = FALSE)
    }
  )
  
  output$downloadAvgGiftType <- downloadHandler(
    filename = function() paste("avg_gift_type_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_avg_gift_analysis(filtered_data(), timeframe = timeframe)$by_gift_type, file, row.names = FALSE)
    }
  )
  
  output$downloadTopDonors <- downloadHandler(
    filename = function() paste("top_donors_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_top_donors(filtered_data(), timeframe = timeframe), file, row.names = FALSE)
    }
  )
  
  output$downloadGiftCounts <- downloadHandler(
    filename = function() paste("gift_counts_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_gift_distribution(filtered_data(), timeframe = timeframe)$gift_counts, file, row.names = FALSE)
    }
  )
  
  output$downloadGiftAmounts <- downloadHandler(
    filename = function() paste("gift_amounts_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_gift_distribution(filtered_data(), timeframe = timeframe)$gift_amounts, file, row.names = FALSE)
    }
  )
  
  output$downloadDonorCounts <- downloadHandler(
    filename = function() paste("donor_counts_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_donor_levels(filtered_data(), timeframe = timeframe)$donor_counts, file, row.names = FALSE)
    }
  )
  
  output$downloadDonorAmounts <- downloadHandler(
    filename = function() paste("donor_amounts_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
      write.csv(calculate_donor_levels(filtered_data(), timeframe = timeframe)$donor_amounts, file, row.names = FALSE)
    }
  )
  
  output$downloadFullData <- downloadHandler(
    filename = function() paste("full_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
}

# =============================================================================
# HELPER FUNCTION FOR UI
# =============================================================================

#' Add year prefix to column names
#' 
#' @param data Data frame with year columns
#' @param prefix Character prefix to add (e.g., "FY " or "CY ")
#' @return Data frame with prefixed year columns
add_year_prefix <- function(data, prefix) {
  if (nrow(data) == 0) return(data)
  
  old_names <- names(data)
  new_names <- old_names
  for (i in seq_along(old_names)) {
    if (grepl("^\\d{4}$", old_names[i])) {
      new_names[i] <- paste0(prefix, old_names[i])
    }
  }
  names(data) <- new_names
  return(data)
}

# =============================================================================
# RUN APPLICATION
# =============================================================================

# Run the Shiny application
shinyApp(ui = ui, server = server) 