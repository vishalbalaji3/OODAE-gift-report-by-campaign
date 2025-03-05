# Load required libraries
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(shinythemes)
library(shinycssloaders)
library(writexl)
library(scales)

# Source data preparation
source("data_prep.R")

# Helper Functions
format_currency <- function(x) {
  ifelse(
    is.na(x) | x == 0,
    "$0",
    ifelse(
      abs(x) >= 1e6,
      sprintf("$%.1fM", x / 1e6),
      sprintf("$%s", format(round(x, 0), big.mark = ","))
    )
  )
}

create_datatable <- function(data, currency_cols = NULL) {
  display_data <- data
  
  if (!is.null(currency_cols)) {
    display_data <- as.data.frame(lapply(seq_along(display_data), function(i) {
      if ((i-1) %in% currency_cols) format_currency(display_data[[i]]) else display_data[[i]]
    }))
    names(display_data) <- names(data)
  }
  
  datatable(
    display_data,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = FALSE,
      dom = 'lfrtip',
      columnDefs = list(list(targets = currency_cols, className = 'dt-right')),
      language = list(search = "Search:")
    ),
    class = 'cell-border stripe',
    filter = 'none',
    selection = 'none'
  )
}

create_download_handlers <- function(id, data_reactive, session) {
  output_name <- function(ext) paste0(id, "_", Sys.Date(), ext)
  
  list(
    csv = downloadHandler(
      filename = function() output_name(".csv"),
      content = function(file) write.csv(data_reactive(), file, row.names = FALSE)
    ),
    excel = downloadHandler(
      filename = function() output_name(".xlsx"),
      content = function(file) write_xlsx(data_reactive(), file)
    )
  )
}

# UI Definition
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Campaign Data Analysis"),
  
  # Add data last updated information
  fluidRow(
    column(12, 
           div(class = "pull-right", 
               textOutput("lastUpdatedText"),
               style = "font-style: italic; color: #666; margin-bottom: 15px;")
    )
  ),
  
  fluidRow(
    column(3, selectInput("campaignFilter", "Select Campaign ID:",
                          choices = unique(FullData$`Campaign ID`),
                          selected = unique(FullData$`Campaign ID`)[1])),
    column(3, selectInput("giftTypeFilter", "Select Gift Type:",
                          choices = levels(FullData$`Gift Type`),
                          multiple = TRUE)),
    column(3, selectInput("yearFilter", "Select Fiscal Years:",
                          choices = sort(unique(FullData$`Fiscal Year`)),
                          multiple = TRUE))
  ),
  
  fluidRow(
    column(12,
           tabsetPanel(
             id = "tabset",
             tabPanel("Summary Statistics",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   uiOutput("summaryHeading"),
                                   uiOutput("giftDistSummary")
                               )
                        )
                      ),
                      downloadButton("downloadSummary_csv", "Download Full CSV"),
                      downloadButton("downloadSummary_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("summaryTable")))),
             tabPanel("Fund Split by Constituency",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Fund Split by Constituency"),
                                   uiOutput("fundSplitSummary")
                               )
                        )
                      ),
                      downloadButton("downloadFundSplit_csv", "Download Full CSV"),
                      downloadButton("downloadFundSplit_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("fundSplitTable")))),
             tabPanel("Fund Analysis",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Top Funds Summary"),
                                   uiOutput("fundAnalysisSummary")
                               )
                        )
                      ),
                      downloadButton("downloadFundAnalysis_csv", "Download Full CSV"),
                      downloadButton("downloadFundAnalysis_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("fundAnalysisTable")))),
             tabPanel("Unique Constituents",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Unique Constituents Summary"),
                                   uiOutput("constituentsSummary")
                               )
                        )
                      ),
                      downloadButton("downloadConstituents_csv", "Download Full CSV"),
                      downloadButton("downloadConstituents_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("uniqueConstituentsTable")))),
             tabPanel("Average Gift Size",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Average Gift Size Summary"),
                                   uiOutput("avgGiftSummary")
                               )
                        )
                      ),
                      downloadButton("downloadAvgGift_csv", "Download Full CSV"),
                      downloadButton("downloadAvgGift_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("avgGiftTable")))),
             tabPanel("Top Donors",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Top Donors Summary"),
                                   uiOutput("topDonorsSummary")
                               )
                        )
                      ),
                      downloadButton("downloadTopDonors_csv", "Download Full CSV"),
                      downloadButton("downloadTopDonors_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("topDonorsTable")))),
             tabPanel("Gift Size Distribution",
                      fluidRow(
                        column(12, 
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Gift Count by Range"),
                                   withSpinner(DTOutput("giftCountTable"))
                               )
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(12, 
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Gift Amount by Range"),
                                   withSpinner(DTOutput("giftAmountTable"))
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(style = "margin-top: 20px;"),
                               downloadButton("downloadGiftDist_csv", "Download Full CSV"),
                               downloadButton("downloadGiftDist_excel", "Download Full Excel")
                        )
                      )
             ),
             tabPanel("Donor Levels",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Donor Levels Summary"),
                                   uiOutput("donorLevelsSummary")
                               )
                        )
                      ),
                      # First table - Number of Donors
                      fluidRow(
                        column(12, 
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Number of Donors by Level"),
                                   withSpinner(DTOutput("donorCountTable"))
                               )
                        )
                      ),
                      # Second table - Donation Amounts
                      fluidRow(
                        column(12, 
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Donation Amount by Level"),
                                   withSpinner(DTOutput("donorAmountTable"))
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(style = "margin-top: 20px;"),
                               downloadButton("downloadDonorLevels_csv", "Download Full CSV"),
                               downloadButton("downloadDonorLevels_excel", "Download Full Excel")
                        )
                      )),
             tabPanel("Full Data",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Data Overview"),
                                   uiOutput("fullDataSummary")
                               )
                        )
                      ),
                      downloadButton("downloadFullData_csv", "Download Full CSV"),
                      downloadButton("downloadFullData_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("fullDataTable"))))
           ))
  )
)

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
  
  # Data processing functions
  
  # Processing function for donor levels summary
  process_donor_levels_summary <- reactive({
    # Calculate total giving per donor across all selected fiscal years
    aggregate_donor_data <- filtered_data() %>%
      group_by(`Constituent ID`, `Name`) %>%
      summarise(
        Total_Giving = sum(`Fund Split Amount`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Categorize donors by giving level
      mutate(
        Donor_Level = case_when(
          Total_Giving >= 150000 ~ "$150,000+",
          Total_Giving >= 75000 ~ "$75,000 - $149,999",
          Total_Giving >= 40000 ~ "$40,000 - $74,999",
          Total_Giving >= 20000 ~ "$20,000 - $39,999",
          Total_Giving >= 10000 ~ "$10,000 - $19,999",
          Total_Giving >= 5000 ~ "$5,000 - $9,999",
          Total_Giving >= 2500 ~ "$2,500 - $4,999",
          Total_Giving >= 1000 ~ "$1,000 - $2,499",
          Total_Giving >= 500 ~ "$500 - $999",
          Total_Giving >= 100 ~ "$100 - $499",
          TRUE ~ "Under $100"
        ),
        Donor_Level = factor(Donor_Level, levels = c(
          "$150,000+",
          "$75,000 - $149,999", 
          "$40,000 - $74,999",
          "$20,000 - $39,999",
          "$10,000 - $19,999",
          "$5,000 - $9,999",
          "$2,500 - $4,999",
          "$1,000 - $2,499",
          "$500 - $999",
          "$100 - $499",
          "Under $100"
        ))
      )
    
    # Count donors by level
    donor_counts <- aggregate_donor_data %>%
      group_by(Donor_Level) %>%
      summarise(
        Number_of_Donors = n_distinct(`Constituent ID`),
        Total_Amount = sum(Total_Giving, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Return the results
    donor_counts
  })
  
  # Processing function for donor levels by fiscal year
  process_donor_levels_data <- reactive({
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    # Process data by fiscal year
    fiscal_year_data <- filtered_data() %>%
      group_by(`Constituent ID`, `Fiscal Year`) %>%
      summarise(
        Year_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate total giving per donor across all selected fiscal years
    total_giving <- fiscal_year_data %>%
      group_by(`Constituent ID`) %>%
      summarise(
        Total_Giving = sum(Year_Amount, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # Categorize donors by giving level
      mutate(
        Donor_Level = case_when(
          Total_Giving >= 150000 ~ "$150,000+",
          Total_Giving >= 75000 ~ "$75,000 - $149,999",
          Total_Giving >= 40000 ~ "$40,000 - $74,999",
          Total_Giving >= 20000 ~ "$20,000 - $39,999",
          Total_Giving >= 10000 ~ "$10,000 - $19,999",
          Total_Giving >= 5000 ~ "$5,000 - $9,999",
          Total_Giving >= 2500 ~ "$2,500 - $4,999",
          Total_Giving >= 1000 ~ "$1,000 - $2,499",
          Total_Giving >= 500 ~ "$500 - $999",
          Total_Giving >= 100 ~ "$100 - $499",
          TRUE ~ "Under $100"
        ),
        Donor_Level = factor(Donor_Level, levels = c(
          "$150,000+",
          "$75,000 - $149,999", 
          "$40,000 - $74,999",
          "$20,000 - $39,999",
          "$10,000 - $19,999",
          "$5,000 - $9,999",
          "$2,500 - $4,999",
          "$1,000 - $2,499",
          "$500 - $999",
          "$100 - $499",
          "Under $100"
        ))
      )
    
    # Join the donor levels back to the fiscal year data
    donor_by_year <- fiscal_year_data %>%
      left_join(total_giving %>% select(`Constituent ID`, Donor_Level), by = "Constituent ID")
    
    # Count donors and sum amounts by level and fiscal year
    donor_counts_by_year <- donor_by_year %>%
      group_by(Donor_Level, `Fiscal Year`) %>%
      summarise(
        Donors = n_distinct(`Constituent ID`),
        Amount = sum(Year_Amount, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Pivot to get fiscal years as columns for donors
    donor_counts_pivot <- donor_counts_by_year %>%
      select(Donor_Level, `Fiscal Year`, Donors) %>%
      pivot_wider(
        names_from = `Fiscal Year`,
        values_from = Donors,
        values_fill = 0,
        names_prefix = "Donors_"
      )
    
    # Pivot to get fiscal years as columns for amounts
    amount_pivot <- donor_counts_by_year %>%
      select(Donor_Level, `Fiscal Year`, Amount) %>%
      pivot_wider(
        names_from = `Fiscal Year`,
        values_from = Amount,
        values_fill = 0,
        names_prefix = "Amount_"
      )
    
    # Calculate overall totals by donor level
    overall_totals <- total_giving %>%
      group_by(Donor_Level) %>%
      summarise(
        Total_Donors = n_distinct(`Constituent ID`),
        Total_Amount = sum(Total_Giving, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Join everything together
    result <- donor_counts_pivot %>%
      left_join(amount_pivot, by = "Donor_Level") %>%
      left_join(overall_totals, by = "Donor_Level")
    
    # Return the results (ensure all donor levels are included even if they have no donors)
    levels_template <- data.frame(
      Donor_Level = factor(levels(total_giving$Donor_Level), 
                           levels = levels(total_giving$Donor_Level))
    )
    
    result <- levels_template %>%
      left_join(result, by = "Donor_Level") %>%
      mutate(across(where(is.numeric), ~replace_na(., 0)))
    
    return(result)
  })
  
  process_summary_data <- reactive({
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    filtered_data() %>%
      group_by(`Gift Type`, `Fiscal Year`) %>%
      summarise(Total_Fund_Split_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
                .groups = 'drop') %>%
      pivot_wider(names_from = `Fiscal Year`,
                  values_from = Total_Fund_Split_Amount,
                  values_fill = 0) %>%
      mutate(Total = rowSums(select(., -`Gift Type`), na.rm = TRUE)) %>%
      select(`Gift Type`, all_of(fiscal_years), Total)
  })
  
  process_fund_split_data <- reactive({
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    filtered_data() %>%
      group_by(`Primary Constituency Code`, `Fiscal Year`) %>%
      summarise(Total_Fund_Split_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
                .groups = 'drop') %>%
      pivot_wider(names_from = `Fiscal Year`,
                  values_from = Total_Fund_Split_Amount,
                  values_fill = 0) %>%
      mutate(Total = rowSums(select(., -`Primary Constituency Code`), na.rm = TRUE)) %>%
      select(`Primary Constituency Code`, all_of(fiscal_years), Total)
  })
  
  process_fund_analysis_data <- reactive({
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    most_recent_year <- max(fiscal_years)
    
    filtered_data() %>%
      group_by(`Fund ID`, `Fund Description`, `Fiscal Year`) %>%
      summarise(Total_Fund_Split_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
                .groups = 'drop') %>%
      pivot_wider(names_from = `Fiscal Year`,
                  values_from = Total_Fund_Split_Amount,
                  values_fill = 0) %>%
      mutate(Total = rowSums(select(., -c(`Fund ID`, `Fund Description`)), na.rm = TRUE)) %>%
      select(`Fund ID`, `Fund Description`, all_of(fiscal_years), Total) %>%
      arrange(desc(Total))
  })
  
  process_unique_constituents_data <- reactive({
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    # Get yearly counts
    yearly_data <- filtered_data() %>%
      group_by(`Primary Constituency Code`, `Fiscal Year`) %>%
      summarise(Unique_Constituents = n_distinct(`Constituent ID`, na.rm = TRUE),
                .groups = 'drop') %>%
      pivot_wider(names_from = `Fiscal Year`,
                  values_from = Unique_Constituents,
                  values_fill = 0)
    
    # Calculate total unique constituents across all years for each constituency code
    total_unique <- filtered_data() %>%
      group_by(`Primary Constituency Code`) %>%
      summarise(Total = n_distinct(`Constituent ID`, na.rm = TRUE),
                .groups = 'drop')
    
    # Combine yearly data with total unique constituents
    yearly_data %>%
      left_join(total_unique, by = "Primary Constituency Code") %>%
      select(`Primary Constituency Code`, all_of(fiscal_years), Total)
  })
  
  process_top_donors_data <- reactive({
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    filtered_data() %>%
      group_by(`Constituent ID`, `Key Indicator`, `Name`, `Fiscal Year`) %>%
      summarise(Total_Fund_Split_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
                .groups = 'drop') %>%
      pivot_wider(names_from = `Fiscal Year`,
                  values_from = Total_Fund_Split_Amount,
                  values_fill = 0) %>%
      mutate(Total = rowSums(select(., -c(`Constituent ID`, `Key Indicator`, `Name`)), na.rm = TRUE)) %>%
      select(`Constituent ID`, `Key Indicator`, `Name`, all_of(fiscal_years), Total) %>%
      arrange(desc(Total))
  })
  
  process_avg_gift_data <- reactive({
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    # Calculate averages by fiscal year
    yearly_avgs <- filtered_data() %>%
      group_by(`Gift Type`, `Fiscal Year`) %>%
      summarise(Avg_Gift_Size = sum(`Fund Split Amount`, na.rm = TRUE) / n(),
                .groups = 'drop') %>%
      pivot_wider(names_from = `Fiscal Year`, 
                  values_from = Avg_Gift_Size,
                  values_fill = 0)
    
    # Calculate overall average
    `Overall Avg` <- filtered_data() %>%
      group_by(`Gift Type`) %>%
      summarise(`Overall Avg` = sum(`Fund Split Amount`, na.rm = TRUE) / n(),
                .groups = 'drop')
    
    # Combine yearly averages with overall average
    yearly_avgs %>%
      left_join(`Overall Avg`, by = "Gift Type") %>%
      select(`Gift Type`, all_of(fiscal_years), `Overall Avg`)
  })
  
  # Updated gift distribution function with new ranges similar to the example
  process_gift_distribution_data <- reactive({
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    filtered_data() %>%
      mutate(
        Gift_Range = case_when(
          `Fund Split Amount` >= 150000 ~ "$150,000+",
          `Fund Split Amount` >= 75000 ~ "$75,000 - $149,999",
          `Fund Split Amount` >= 40000 ~ "$40,000 - $74,999",
          `Fund Split Amount` >= 20000 ~ "$20,000 - $39,999",
          `Fund Split Amount` >= 10000 ~ "$10,000 - $19,999",
          `Fund Split Amount` >= 5000 ~ "$5,000 - $9,999",
          `Fund Split Amount` >= 2500 ~ "$2,500 - $4,999",
          `Fund Split Amount` >= 1000 ~ "$1,000 - $2,499",
          `Fund Split Amount` >= 500 ~ "$500 - $999",
          `Fund Split Amount` >= 100 ~ "$100 - $499",
          TRUE ~ "Under $100"
        ),
        Gift_Range = factor(Gift_Range, levels = c(
          "$150,000+",
          "$75,000 - $149,999", 
          "$40,000 - $74,999",
          "$20,000 - $39,999",
          "$10,000 - $19,999",
          "$5,000 - $9,999",
          "$2,500 - $4,999",
          "$1,000 - $2,499",
          "$500 - $999",
          "$100 - $499",
          "Under $100"
        ))
      ) %>%
      group_by(Gift_Range, `Fiscal Year`) %>%
      summarise(
        Number_of_Gifts = n(),
        Total_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_wider(
        names_from = `Fiscal Year`,
        values_from = c(Number_of_Gifts, Total_Amount),
        values_fill = 0
      ) %>%
      # Calculate totals across all fiscal years
      mutate(
        Total_Gifts = rowSums(select(., starts_with("Number_of_Gifts_")), na.rm = TRUE),
        Total_Amount = rowSums(select(., starts_with("Total_Amount_")), na.rm = TRUE)
      ) %>%
      # Select and arrange columns appropriately
      select(
        Gift_Range, 
        starts_with("Number_of_Gifts_"), 
        Total_Gifts,
        starts_with("Total_Amount_"),
        Total_Amount
      )
  })
  
  # Dynamic heading for the Gift Range Distribution Summary
  # Replace the existing summaryHeading function with this updated version
  
  # Dynamic heading for the Gift Range Distribution Summary
  # Improved summary heading that handles non-consecutive selected years
  
  # Dynamic heading for the Gift Range Distribution Summary
  output$summaryHeading <- renderUI({
    # Get selected filters
    selected_years <- input$yearFilter
    selected_gift_types <- input$giftTypeFilter
    
    if (length(selected_years) == 0) {
      # No years filter selected, show all fiscal years
      fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
      if (length(fiscal_years) >= 2) {
        # Check if years are consecutive
        if (all(diff(as.numeric(fiscal_years)) == 1)) {
          heading <- paste("Gift Range Distribution Summary -", 
                           fiscal_years[1], "to", tail(fiscal_years, 1), 
                           "Fiscal Years")
        } else {
          # Non-consecutive years
          heading <- paste("Gift Range Distribution Summary -", 
                           paste(fiscal_years, collapse = ", "), 
                           "Fiscal Years")
        }
      } else if (length(fiscal_years) == 1) {
        heading <- paste("Gift Range Distribution Summary -", fiscal_years[1], "Fiscal Year")
      } else {
        heading <- "Gift Range Distribution Summary"
      }
    } else {
      # Years filter selected
      selected_years <- sort(selected_years)
      if (length(selected_years) >= 2) {
        # Check if selected years are consecutive
        if (all(diff(as.numeric(selected_years)) == 1)) {
          heading <- paste("Gift Range Distribution Summary -", 
                           selected_years[1], "to", tail(selected_years, 1), 
                           "Fiscal Years")
        } else {
          # Non-consecutive selected years
          heading <- paste("Gift Range Distribution Summary -", 
                           paste(selected_years, collapse = ", "), 
                           "Fiscal Years")
        }
      } else if (length(selected_years) == 1) {
        heading <- paste("Gift Range Distribution Summary -", selected_years, "Fiscal Year")
      } else {
        heading <- "Gift Range Distribution Summary - Filtered Data"
      }
    }
    
    # If gift types are filtered, add that information
    if (length(selected_gift_types) > 0) {
      if (length(selected_gift_types) < length(levels(FullData$`Gift Type`))) {
        heading <- paste0(heading, " (", 
                          paste(selected_gift_types, collapse = ", "), 
                          " only)")
      }
    }
    
    h4(heading)
  })
  
  # Donor Levels Summary
  output$donorLevelsSummary <- renderUI({
    # Get donor level data
    donor_data <- process_donor_levels_summary()
    
    if (nrow(donor_data) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Donor Level</th>',
                         '<th class="text-right"># of Donors</th>',
                         '<th class="text-right">Total Amount</th>',
                         '<th class="text-right">% of Total</th>',
                         '</tr></thead><tbody>')
    
    # Calculate grand total for percentage calculation
    grand_total <- sum(donor_data$Total_Amount, na.rm = TRUE)
    total_donors <- sum(donor_data$Number_of_Donors, na.rm = TRUE)
    
    # Add rows for each donor level
    for(i in 1:nrow(donor_data)) {
      row <- donor_data[i, ]
      percent <- sprintf("%.1f%%", (row$Total_Amount / grand_total) * 100)
      
      html_table <- paste0(html_table, '<tr>',
                           '<td>', row$Donor_Level, '</td>',
                           '<td class="text-right">', row$Number_of_Donors, '</td>',
                           '<td class="text-right">', format_currency(row$Total_Amount), '</td>',
                           '<td class="text-right">', percent, '</td>',
                           '</tr>')
    }
    
    # Add totals row
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Total</th>',
                         '<th class="text-right">', total_donors, '</th>',
                         '<th class="text-right">', format_currency(grand_total), '</th>',
                         '<th class="text-right">100.0%</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
  })
  
  # Donor Count Table
  output$donorCountTable <- renderDT({
    data <- process_donor_levels_data()
    
    if (nrow(data) == 0 || ncol(data) <= 1) {
      return(datatable(data.frame(Message = "No data available for the selected filters."),
                       options = list(dom = 't')))
    }
    
    # Extract only donor count columns
    donor_cols <- c("Donor_Level", grep("^Donors_", names(data), value = TRUE), "Total_Donors")
    display_data <- data[, donor_cols]
    
    # Rename columns for better readability
    col_names <- names(display_data)
    for (i in seq_along(col_names)) {
      if (startsWith(col_names[i], "Donors_")) {
        fiscal_year <- substr(col_names[i], 8, nchar(col_names[i]))
        col_names[i] <- fiscal_year
      }
    }
    names(display_data) <- col_names
    
    # Rename the total column
    names(display_data)[names(display_data) == "Total_Donors"] <- "Total"
    
    # Create datatable
    create_datatable(display_data)
  })
  
  # Donor Amount Table
  output$donorAmountTable <- renderDT({
    data <- process_donor_levels_data()
    
    if (nrow(data) == 0 || ncol(data) <= 1) {
      return(datatable(data.frame(Message = "No data available for the selected filters."),
                       options = list(dom = 't')))
    }
    
    # Extract only amount columns
    amount_cols <- c("Donor_Level", grep("^Amount_", names(data), value = TRUE), "Total_Amount")
    display_data <- data[, amount_cols]
    
    # Rename columns for better readability
    col_names <- names(display_data)
    for (i in seq_along(col_names)) {
      if (startsWith(col_names[i], "Amount_")) {
        fiscal_year <- substr(col_names[i], 8, nchar(col_names[i]))
        col_names[i] <- fiscal_year
      }
    }
    names(display_data) <- col_names
    
    # Rename the total column
    names(display_data)[names(display_data) == "Total_Amount"] <- "Total"
    
    # All columns except Donor_Level should be formatted as currency
    currency_cols <- which(names(display_data) != "Donor_Level") - 1
    
    # Create datatable
    create_datatable(display_data, currency_cols)
  })
  
  # Gift distribution summary for the Summary Statistics tab
  output$giftDistSummary <- renderUI({
    # Get selected fiscal years or most recent if none selected
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Use the most recent year if no specific filters selected
    most_recent_year <- tail(fiscal_years, 1)
    
    # Process the gift distribution data for the summary
    dist_data <- process_gift_distribution_data()
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Gift Range</th>',
                         '<th class="text-right"># of Gifts</th>',
                         '<th class="text-right">Amount</th>',
                         '</tr></thead><tbody>')
    
    # If multiple years are selected, use Total columns
    if (length(input$yearFilter) > 1 || length(fiscal_years) > 1) {
      # Add rows for each gift range using totals
      for(i in 1:nrow(dist_data)) {
        row <- dist_data[i, ]
        gift_count <- row$Total_Gifts
        amount <- format_currency(row$Total_Amount)
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$Gift_Range, '</td>',
                             '<td class="text-right">', gift_count, '</td>',
                             '<td class="text-right">', amount, '</td>',
                             '</tr>')
      }
      
      # Add totals row
      total_gifts <- sum(dist_data$Total_Gifts, na.rm = TRUE)
      total_amount <- sum(dist_data$Total_Amount, na.rm = TRUE)
    } else {
      # Use only the most recent year data
      gift_count_col <- paste0("Number_of_Gifts_", most_recent_year)
      amount_col <- paste0("Total_Amount_", most_recent_year)
      
      # Check if columns exist before proceeding
      if(!(gift_count_col %in% names(dist_data) && amount_col %in% names(dist_data))) {
        return(HTML("<p>No data available for the selected fiscal year.</p>"))
      }
      
      # Add rows for each gift range
      for(i in 1:nrow(dist_data)) {
        row <- dist_data[i, ]
        gift_count <- if(gift_count_col %in% names(row)) row[[gift_count_col]] else 0
        amount <- if(amount_col %in% names(row)) format_currency(row[[amount_col]]) else "$0"
        
        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$Gift_Range, '</td>',
                             '<td class="text-right">', gift_count, '</td>',
                             '<td class="text-right">', amount, '</td>',
                             '</tr>')
      }
      
      # Add totals row
      total_gifts <- sum(dist_data[[gift_count_col]], na.rm = TRUE)
      total_amount <- sum(dist_data[[amount_col]], na.rm = TRUE)
    }
    
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Total</th>',
                         '<th class="text-right">', total_gifts, '</th>',
                         '<th class="text-right">', format_currency(total_amount), '</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
  })
  
  # Fund Split Summary
  output$fundSplitSummary <- renderUI({
    # Get primary constituency codes
    const_codes <- sort(unique(filtered_data()$`Primary Constituency Code`))
    
    # Get selected fiscal years or most recent if none selected
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Use the most recent year for display if no specific filters selected
    most_recent_year <- tail(fiscal_years, 1)
    
    # Process the fund split data for summary (top 5 constituencies)
    fund_data <- process_fund_split_data() %>%
      arrange(desc(Total)) %>%
      head(5)
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Constituency</th>',
                         '<th class="text-right">Total Amount</th>',
                         '<th class="text-right">% of Total</th>',
                         '</tr></thead><tbody>')
    
    # Calculate grand total for percentage calculation
    grand_total <- sum(fund_data$Total, na.rm = TRUE)
    
    # Add rows for each constituency
    for(i in 1:nrow(fund_data)) {
      row <- fund_data[i, ]
      amount <- format_currency(row$Total)
      percent <- sprintf("%.1f%%", (row$Total / grand_total) * 100)
      
      html_table <- paste0(html_table, '<tr>',
                           '<td>', row$`Primary Constituency Code`, '</td>',
                           '<td class="text-right">', amount, '</td>',
                           '<td class="text-right">', percent, '</td>',
                           '</tr>')
    }
    
    # Add totals row
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Total (Top 5)</th>',
                         '<th class="text-right">', format_currency(grand_total), '</th>',
                         '<th class="text-right">100.0%</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
  })
  
  # Fund Analysis Summary
  output$fundAnalysisSummary <- renderUI({
    # Get selected fiscal years
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Process the fund analysis data for summary (top 5 funds)
    fund_data <- process_fund_analysis_data() %>%
      arrange(desc(Total)) %>%
      head(5)
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Fund</th>',
                         '<th class="text-right">Total Amount</th>',
                         '<th class="text-right">% of Total</th>',
                         '</tr></thead><tbody>')
    
    # Calculate grand total for percentage calculation
    grand_total <- sum(fund_data$Total, na.rm = TRUE)
    overall_total <- sum(process_fund_analysis_data()$Total, na.rm = TRUE)
    
    # Add rows for each fund
    for(i in 1:nrow(fund_data)) {
      row <- fund_data[i, ]
      amount <- format_currency(row$Total)
      percent <- sprintf("%.1f%%", (row$Total / overall_total) * 100)
      
      html_table <- paste0(html_table, '<tr>',
                           '<td>', row$`Fund Description`, '</td>',
                           '<td class="text-right">', amount, '</td>',
                           '<td class="text-right">', percent, '</td>',
                           '</tr>')
    }
    
    # Add totals row
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Total (Top 5)</th>',
                         '<th class="text-right">', format_currency(grand_total), '</th>',
                         '<th class="text-right">', sprintf("%.1f%%", (grand_total / overall_total) * 100), '</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
  })
  
  # Constituents Summary
  output$constituentsSummary <- renderUI({
    # Get selected fiscal years
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Process the unique constituents data
    const_data <- process_unique_constituents_data()
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Constituency</th>',
                         '<th class="text-right">Unique Constituents</th>',
                         '<th class="text-right">% of Total</th>',
                         '</tr></thead><tbody>')
    
    # Calculate total for percentage calculation
    total_constituents <- sum(const_data$Total, na.rm = TRUE)
    
    # Add rows for each constituency code
    const_data <- const_data %>% arrange(desc(Total))
    for(i in 1:nrow(const_data)) {
      row <- const_data[i, ]
      percent <- sprintf("%.1f%%", (row$Total / total_constituents) * 100)
      
      html_table <- paste0(html_table, '<tr>',
                           '<td>', row$`Primary Constituency Code`, '</td>',
                           '<td class="text-right">', format(row$Total, big.mark = ","), '</td>',
                           '<td class="text-right">', percent, '</td>',
                           '</tr>')
    }
    
    # Add totals row
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Total</th>',
                         '<th class="text-right">', format(total_constituents, big.mark = ","), '</th>',
                         '<th class="text-right">100.0%</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
  })
  
  # Average Gift Size Summary
  output$avgGiftSummary <- renderUI({
    # Get selected fiscal years
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Process the average gift size data
    avg_data <- process_avg_gift_data()
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Gift Type</th>',
                         '<th class="text-right">Average Gift Size</th>',
                         '</tr></thead><tbody>')
    
    # Add rows for each gift type
    for(i in 1:nrow(avg_data)) {
      row <- avg_data[i, ]
      
      html_table <- paste0(html_table, '<tr>',
                           '<td>', row$`Gift Type`, '</td>',
                           '<td class="text-right">', format_currency(row$`Overall Avg`), '</td>',
                           '</tr>')
    }
    
    # Calculate overall average
    overall_avg <- filtered_data() %>%
      summarise(avg = sum(`Fund Split Amount`, na.rm = TRUE) / n()) %>%
      pull(avg)
    
    # Add overall average row
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Overall Average</th>',
                         '<th class="text-right">', format_currency(overall_avg), '</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
  })
  
  # Top Donors Summary
  output$topDonorsSummary <- renderUI({
    # Get selected fiscal years
    fiscal_years <- sort(unique(filtered_data()$`Fiscal Year`))
    
    if (length(fiscal_years) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Process the top donors data (top 5)
    donors_data <- process_top_donors_data() %>%
      head(5)
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Donor</th>',
                         '<th class="text-right">Total Contribution</th>',
                         '<th class="text-right">% of Total</th>',
                         '</tr></thead><tbody>')
    
    # Calculate grand total for percentage calculation
    grand_total <- sum(donors_data$Total, na.rm = TRUE)
    overall_total <- sum(process_top_donors_data()$Total, na.rm = TRUE)
    
    # Add rows for each donor
    for(i in 1:nrow(donors_data)) {
      row <- donors_data[i, ]
      amount <- format_currency(row$Total)
      percent <- sprintf("%.1f%%", (row$Total / overall_total) * 100)
      
      html_table <- paste0(html_table, '<tr>',
                           '<td>', row$Name, '</td>',
                           '<td class="text-right">', amount, '</td>',
                           '<td class="text-right">', percent, '</td>',
                           '</tr>')
    }
    
    # Add totals row
    html_table <- paste0(html_table, '<tr class="info">',
                         '<th>Total (Top 5)</th>',
                         '<th class="text-right">', format_currency(grand_total), '</th>',
                         '<th class="text-right">', sprintf("%.1f%%", (grand_total / overall_total) * 100), '</th>',
                         '</tr></tbody></table>')
    
    HTML(html_table)
  })
  
  # Full Data Summary
  output$fullDataSummary <- renderUI({
    # Get data
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(HTML("<p>No data available for the selected filters.</p>"))
    }
    
    # Calculate summary statistics
    total_gifts <- nrow(data)
    total_amount <- sum(data$`Fund Split Amount`, na.rm = TRUE)
    total_constituents <- n_distinct(data$`Constituent ID`)
    avg_gift_size <- total_amount / total_gifts
    
    # Get fiscal year range
    fiscal_years <- sort(unique(data$`Fiscal Year`))
    year_range <- ifelse(length(fiscal_years) > 1, 
                         paste(fiscal_years[1], "to", tail(fiscal_years, 1)),
                         fiscal_years[1])
    
    # Create a summary table HTML
    html_table <- '<table class="table table-striped table-bordered">'
    html_table <- paste0(html_table, '<thead><tr>',
                         '<th>Metric</th>',
                         '<th class="text-right">Value</th>',
                         '</tr></thead><tbody>')
    
    # Add summary rows
    html_table <- paste0(html_table, '<tr><td>Fiscal Year(s)</td><td class="text-right">', year_range, '</td></tr>')
    html_table <- paste0(html_table, '<tr><td>Total Number of Gifts</td><td class="text-right">', format(total_gifts, big.mark = ","), '</td></tr>')
    html_table <- paste0(html_table, '<tr><td>Total Gift Amount</td><td class="text-right">', format_currency(total_amount), '</td></tr>')
    html_table <- paste0(html_table, '<tr><td>Unique Constituents</td><td class="text-right">', format(total_constituents, big.mark = ","), '</td></tr>')
    html_table <- paste0(html_table, '<tr><td>Average Gift Size</td><td class="text-right">', format_currency(avg_gift_size), '</td></tr>')
    
    html_table <- paste0(html_table, '</tbody></table>')
    
    HTML(html_table)
  })
  
  # Render tables
  output$summaryTable <- renderDT({
    data <- process_summary_data()
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$fundSplitTable <- renderDT({
    data <- process_fund_split_data()
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$fundAnalysisTable <- renderDT({
    data <- process_fund_analysis_data()
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$uniqueConstituentsTable <- renderDT({
    create_datatable(process_unique_constituents_data())
  })
  
  output$topDonorsTable <- renderDT({
    data <- process_top_donors_data()
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  output$avgGiftTable <- renderDT({
    data <- process_avg_gift_data()
    currency_cols <- which(sapply(data, is.numeric)) - 1
    create_datatable(data, currency_cols)
  })
  
  # Gift Count Table renderer
  output$giftCountTable <- renderDT({
    data <- process_gift_distribution_data()
    
    # Get column names for gift counts
    gift_count_cols <- grep("^Number_of_Gifts_", names(data), value = TRUE)
    
    # Create a display data frame with just the count columns
    display_data <- data %>%
      select(Gift_Range, all_of(gift_count_cols), Total_Gifts)
    
    # Rename columns to remove prefixes and use cleaner names
    new_names <- names(display_data)
    
    # Clean up gift count column names
    for (col in gift_count_cols) {
      year <- gsub("Number_of_Gifts_", "", col)
      new_col_name <- year
      new_names[which(names(display_data) == col)] <- new_col_name
    }
    
    # Rename total column
    new_names[which(names(display_data) == "Total_Gifts")] <- "Total"
    
    # Apply new column names
    names(display_data) <- new_names
    
    # Determine which columns should be right-aligned (numbers)
    number_cols <- which(names(display_data) != "Gift_Range") - 1
    
    # Create the datatable with appropriate formatting
    datatable(
      display_data,
      caption = "Number of Gifts by Gift Range",
      options = list(
        pageLength = 15,  # Show all gift ranges on one page
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'lfrtip',
        columnDefs = list(
          list(targets = number_cols, className = 'dt-right')
        ),
        language = list(search = "Search:")
      ),
      class = 'cell-border stripe',
      filter = 'none',
      selection = 'none'
    )
  })
  
  # Gift Amount Table renderer
  output$giftAmountTable <- renderDT({
    data <- process_gift_distribution_data()
    
    # Get column names for amounts
    amount_cols <- grep("^Total_Amount_", names(data), value = TRUE)
    
    # Create a display data frame with just the amount columns
    display_data <- data %>%
      select(Gift_Range, all_of(amount_cols), Total_Amount)
    
    # Format currency values
    for (col in c(amount_cols, "Total_Amount")) {
      display_data[[col]] <- format_currency(display_data[[col]])
    }
    
    # Rename columns to remove prefixes and use cleaner names
    new_names <- names(display_data)
    
    # Clean up amount column names
    for (col in amount_cols) {
      year <- gsub("Total_Amount_", "", col)
      new_col_name <- year
      new_names[which(names(display_data) == col)] <- new_col_name
    }
    
    # Rename total column
    new_names[which(names(display_data) == "Total_Amount")] <- "Total"
    
    # Apply new column names
    names(display_data) <- new_names
    
    # All columns except Gift_Range should be right-aligned
    currency_cols <- which(names(display_data) != "Gift_Range") - 1
    
    # Create the data table with appropriate formatting
    datatable(
      display_data,
      caption = "Gift Amount by Gift Range",
      options = list(
        pageLength = 15,  # Show all gift ranges on one page
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'lfrtip',
        columnDefs = list(
          list(targets = currency_cols, className = 'dt-right')
        ),
        language = list(search = "Search:")
      ),
      class = 'cell-border stripe',
      filter = 'none',
      selection = 'none'
    )
  })
  
  output$fullDataTable <- renderDT({
    data <- filtered_data() %>% arrange(`Fiscal Year`)
    currency_cols <- which(names(data) %in% c("Fund Split Amount", "Gift Amount", 
                                              "Gift Receipt Amount", "Gift Pledge Balance")) - 1
    create_datatable(data, currency_cols)
  })
  
  # Download handlers
  downloads <- list(
    summary = create_download_handlers("summary_data", process_summary_data, session),
    fundSplit = create_download_handlers("fund_split_data", process_fund_split_data, session),
    fundAnalysis = create_download_handlers("fund_analysis_data", process_fund_analysis_data, session),
    constituents = create_download_handlers("unique_constituents_data", process_unique_constituents_data, session),
    topDonors = create_download_handlers("top_donors_data", process_top_donors_data, session),
    avgGift = create_download_handlers("avg_gift_size_data", process_avg_gift_data, session),
    giftDist = create_download_handlers("gift_distribution_data", process_gift_distribution_data, session),
    donorLevels <- create_download_handlers("donor_levels_data", process_donor_levels_data, session),
    fullData = create_download_handlers("full_data", filtered_data, session)
  )
  
  # Assign download handlers to outputs
  mapply(function(prefix, handlers) {
    output[[paste0("download", prefix, "_csv")]] <- handlers$csv
    output[[paste0("download", prefix, "_excel")]] <- handlers$excel
  },
  c("Summary", "FundSplit", "FundAnalysis", "Constituents", "TopDonors", "AvgGift", "GiftDist", "DonorLevels", "FullData"),
  downloads)
}

# Run the application
shinyApp(ui = ui, server = server)