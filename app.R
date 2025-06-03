# Simplified OODAE Gift Report Dashboard
# All functionality consolidated into a single file for better readability

# Load required libraries
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(writexl)
library(plotly)
library(forcats)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Simple configuration - only what we actually use
CONFIG <- list(
  theme = "readable",
  constituency_hierarchy = c("UMMC Alumni", "UMMC Affilate", "Organization", "Individuals", "Other"),
  constituency_mapping = list(
    "UMMC Alumni" = "Alumni",
    "UMMC Affilate" = "Board Member",
    "Organization" = "Corporation",
    "Organization" = "Estate / Trust",
    "UMMC Affilate" = "Faculty/Staff",
    "Organization" = "Foundation",
    "Individuals" = "Friend",
    "Organization" = "Organization",
    "Other" = "Peer-to-Peer Fundraiser",
    "UMMC Alumni" = "Resident/Fellow",
    "UMMC Affilate" = "Student",
    "Individuals" = "Tribute",
    "UMMC Affilate" = "University Of Mississippi Medical Center",
    "UMMC Affilate" = "The MIND Center Community Advisory Board",
    "UMMC Affilate" = "UMMC Advisory Council",
    "UMMC Affilate" = "CCRI Campaign Committee Member",
    "Other" = "CCRI",
    "Other" = "Guardian Society Member",
    "UMMC Affilate" = "UM Foundation Board"
  ),
  gift_type_mapping = list(
    "Cash" = "Cash",
    "Cash" = "Recurring Gift Pay-Cash",
    "Stock/Property" = "Stock/Property",
    "Planned Gift" = "Planned Gift",
    "Pledge" = "Pledge"
  ),
  gift_ranges = list(
    labels = c("$150,000+", "$75,000 - $149,999", "$40,000 - $74,999", "$20,000 - $39,999",
               "$10,000 - $19,999", "$5,000 - $9,999", "$2,500 - $4,999", "$1,000 - $2,499",
               "$500 - $999", "$100 - $499", "Under $100"),
    thresholds = c(150000, 75000, 40000, 20000, 10000, 5000, 2500, 1000, 500, 100, 0)
  )
)

# =============================================================================
# DATA LOADING AND PREPARATION
# =============================================================================

# Primary constituency selection function
get_primary_code <- function(codes, hierarchy) {
  for (h in hierarchy) {
    if (h %in% codes) return(h)
  }
  NA
}

# Load and prepare data
load_data <- function() {
  # Load main data
  full_data <- read_csv("data/AllGifts.CSV",
                       col_types = cols(.default = "c",
                                        "Key Indicator" = "f",
                                        "Constituency Code" = "f",
                                        "Gift Date" = col_date("%m/%d/%Y"),
                                        "Gift Type" = "f",
                                        "Gift Subtype" = "f",
                                        "Gift Amount" = col_number(),
                                        "Gift Receipt Amount" = col_number(),
                                        "Gift Pledge Balance" = col_number(),
                                        "Fund Split Amount" = col_number())) %>%
    # Apply mappings
    mutate(
      `Constituency Code` = fct_recode(`Constituency Code`, !!!CONFIG$constituency_mapping),
      `Gift Type` = fct_recode(`Gift Type`, !!!CONFIG$gift_type_mapping)
    ) %>%
    group_by(`Constituent ID`) %>%
    mutate(
      `Primary Constituency Code` = get_primary_code(`Constituency Code`, CONFIG$constituency_hierarchy),
      `Fiscal Year` = ifelse(
        as.numeric(format(as.Date(`Gift Date`), "%m")) >= 7,
        as.numeric(format(as.Date(`Gift Date`), "%Y")) + 1,
        as.numeric(format(as.Date(`Gift Date`), "%Y"))
      ) %>% as.character()
    ) %>%
    select(-`Constituency Code`) %>%
    distinct() %>%
    ungroup() %>%
    mutate(`Primary Constituency Code` = factor(`Primary Constituency Code`, 
                                              levels = CONFIG$constituency_hierarchy, 
                                              ordered = TRUE)) %>%
    filter(`Gift Amount` != 0)
  
  # Load fund list
  fund_list <- read_csv("data/FundList.CSV")
  full_data <- merge(full_data, fund_list, by = "Fund ID", all.x = TRUE) %>% 
    select(-`Campaign ID`) %>% 
    rename("Campaign ID" = "Fund Default Campaign ID")
  
  # Add last updated info
  data_file_info <- file.info("data/AllGifts.CSV")
  attr(full_data, "last_updated") <- data_file_info$mtime
  
  full_data
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Format currency
format_currency <- function(x, abbreviate = TRUE) {
  if (is.na(x) || x == 0) return("$0")
  
  if (abbreviate && x >= 1000000) {
    paste0("$", round(x / 1000000, 1), "M")
  } else if (abbreviate && x >= 1000) {
    paste0("$", round(x / 1000, 1), "K")
  } else {
    paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
  }
}

# Format percentage
format_percent <- function(x) {
  if (is.na(x)) return("0%")
  paste0(round(x, 1), "%")
}

# Categorize gift amounts
categorize_gift <- function(amount, labels = CONFIG$gift_ranges$labels, thresholds = CONFIG$gift_ranges$thresholds) {
  for (i in seq_along(thresholds)) {
    if (amount >= thresholds[i]) {
      return(labels[i])
    }
  }
  labels[length(labels)]
}

# Apply filters to data
apply_data_filters <- function(data, campaign = NULL, gift_types = NULL, years = NULL, timeframe = "fiscal") {
  filtered <- data
  
  if (!is.null(campaign)) {
    filtered <- filtered %>% filter(`Campaign ID` == campaign)
  }
  
  if (length(gift_types) > 0) {
    filtered <- filtered %>% filter(`Gift Type` %in% gift_types)
  }
  
  if (length(years) > 0) {
    if (timeframe == "calendar") {
      filtered <- filtered %>% filter(format(as.Date(`Gift Date`), "%Y") %in% years)
    } else {
      filtered <- filtered %>% filter(`Fiscal Year` %in% years)
    }
  }
  
  filtered
}

# =============================================================================
# DATA PROCESSING FUNCTIONS
# =============================================================================

# Core function to process data by group with year breakdown
process_data_by_group <- function(data, group_cols, value_col = "Fund Split Amount", timeframe = "fiscal") {
  if (nrow(data) == 0) return(data.frame())
  
  # Get years based on timeframe
  if (timeframe == "calendar") {
    years <- sort(unique(format(as.Date(data$`Gift Date`), "%Y")))
    year_col <- "Calendar_Year"
    data <- data %>% mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
  } else {
    years <- sort(unique(data$`Fiscal Year`))
    year_col <- "Fiscal Year"
  }
  
  # Group and pivot data
  result <- data %>%
    group_by(across(all_of(c(group_cols, year_col)))) %>%
    summarise(Total_Value = sum(!!sym(value_col), na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = !!sym(year_col), values_from = Total_Value, values_fill = 0)
  
  # Add total column
  year_cols <- intersect(names(result), years)
  if (length(year_cols) > 0) {
    result <- result %>%
      mutate(Total = rowSums(select(., all_of(year_cols)), na.rm = TRUE)) %>%
      arrange(desc(Total))
  }
  
  # Reorder columns: group columns, then years, then total
  if ("Total" %in% names(result)) {
    result <- result %>% select(all_of(group_cols), all_of(years), Total)
  } else {
    result <- result %>% select(all_of(group_cols), all_of(years))
  }
  
  result
}

# Summary statistics
calculate_summary_stats <- function(data, timeframe = "fiscal") {
  if (nrow(data) == 0) {
    return(data.frame(
      Metric = c("Total Gifts", "Total Amount", "Unique Constituents", "Average Gift Size"),
      Value = c("0", "$0", "0", "$0")
    ))
  }
  
  # Process by Gift Type with year breakdown
  gift_type_data <- process_data_by_group(data, "Gift Type", timeframe = timeframe)
  
  # Add totals row for each year
  if (nrow(gift_type_data) > 0) {
    # Convert factor columns to character to avoid assignment issues
    if (is.factor(gift_type_data$`Gift Type`)) {
      gift_type_data$`Gift Type` <- as.character(gift_type_data$`Gift Type`)
    }
    
    # Create totals row with same structure as gift_type_data
    totals_row <- gift_type_data[1, ]  # Copy structure
    totals_row[1, "Gift Type"] <- "TOTAL"
    
    # Calculate totals for each column
    for (col_name in names(gift_type_data)) {
      if (col_name != "Gift Type" && col_name %in% names(gift_type_data)) {
        if (is.numeric(gift_type_data[[col_name]])) {
          totals_row[[col_name]] <- sum(gift_type_data[[col_name]], na.rm = TRUE)
        }
      }
    }
    
    # Add totals row
    gift_type_data <- rbind(gift_type_data, totals_row)
  }
  
  # Also create simple summary for display
  total_gifts <- nrow(data)
  total_amount <- sum(data$`Fund Split Amount`, na.rm = TRUE)
  unique_constituents <- n_distinct(data$`Constituent ID`)
  avg_gift <- total_amount / total_gifts
  
  # Return the detailed breakdown
  list(
    summary_table = data.frame(
      Metric = c("Total Gifts", "Total Amount", "Unique Constituents", "Average Gift Size"),
      Value = c(
        formatC(total_gifts, format = "f", digits = 0, big.mark = ","),
        format_currency(total_amount),
        formatC(unique_constituents, format = "f", digits = 0, big.mark = ","),
        format_currency(avg_gift)
      )
    ),
    detailed_data = gift_type_data
  )
}

# Fund split analysis
calculate_fund_split <- function(data, timeframe = "fiscal") {
  if (nrow(data) == 0) return(list(summary_table = data.frame(), detailed_data = data.frame()))
  
  result <- process_data_by_group(data, "Primary Constituency Code", timeframe = timeframe)
  
  # Create summary table with percentages
  summary_table <- result %>%
    mutate(`% of Total` = (`Total` / sum(`Total`, na.rm = TRUE)) * 100) %>%
    select(`Primary Constituency Code`, `Total`, `% of Total`) %>%
    mutate(
      `Total` = sapply(`Total`, format_currency),
      `% of Total` = sapply(`% of Total`, format_percent)
    )
  
  # Add totals row to detailed data
  if (nrow(result) > 0) {
    # Convert factor columns to character to avoid assignment issues
    if (is.factor(result$`Primary Constituency Code`)) {
      result$`Primary Constituency Code` <- as.character(result$`Primary Constituency Code`)
    }
    
    # Create totals row with same structure as result
    totals_row <- result[1, ]  # Copy structure
    totals_row[1, "Primary Constituency Code"] <- "TOTAL"
    
    # Calculate totals for each column
    for (col_name in names(result)) {
      if (col_name != "Primary Constituency Code" && col_name %in% names(result)) {
        if (is.numeric(result[[col_name]])) {
          totals_row[[col_name]] <- sum(result[[col_name]], na.rm = TRUE)
        }
      }
    }
    
    # Add totals row
    detailed_result <- rbind(result, totals_row)
  } else {
    detailed_result <- result
  }
  
  # Format currency columns (all except the first group column)
  currency_cols <- names(detailed_result)[!names(detailed_result) %in% c("Primary Constituency Code")]
  for (col in currency_cols) {
    if (is.numeric(detailed_result[[col]])) {
      detailed_result[[col]] <- sapply(detailed_result[[col]], format_currency)
    }
  }
  
  list(summary_table = summary_table, detailed_data = detailed_result)
}

# Fund analysis
calculate_fund_analysis <- function(data, timeframe = "fiscal") {
  if (nrow(data) == 0) return(list(summary_table = data.frame(), detailed_data = data.frame()))
  
  result <- process_data_by_group(data, c("Fund ID", "Fund Description"), timeframe = timeframe)
  
  # Create summary table for top 5 funds
  summary_table <- result %>%
    head(5) %>%
    mutate(`% of Total` = (`Total` / sum(result$`Total`, na.rm = TRUE)) * 100) %>%
    select(`Fund Description`, `Total`, `% of Total`) %>%
    mutate(
      `Total` = sapply(`Total`, format_currency),
      `% of Total` = sapply(`% of Total`, format_percent)
    )
  
  # Format currency columns for detailed data
  currency_cols <- names(result)[!names(result) %in% c("Fund ID", "Fund Description")]
  for (col in currency_cols) {
    if (is.numeric(result[[col]])) {
      result[[col]] <- sapply(result[[col]], format_currency)
    }
  }
  
  list(summary_table = summary_table, detailed_data = result)
}

# Unique constituents
calculate_unique_constituents <- function(data, timeframe = "fiscal") {
  if (nrow(data) == 0) return(data.frame())
  
  # Get years
  if (timeframe == "calendar") {
    years <- sort(unique(format(as.Date(data$`Gift Date`), "%Y")))
    year_col <- "Calendar_Year"
    data <- data %>% mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
  } else {
    years <- sort(unique(data$`Fiscal Year`))
    year_col <- "Fiscal Year"
  }
  
  # Count unique constituents by year
  yearly_data <- data %>%
    group_by(`Primary Constituency Code`, !!sym(year_col)) %>%
    summarise(Unique_Constituents = n_distinct(`Constituent ID`), .groups = 'drop') %>%
    pivot_wider(names_from = !!sym(year_col), values_from = Unique_Constituents, values_fill = 0)
  
  # Calculate total unique constituents
  total_unique <- data %>%
    group_by(`Primary Constituency Code`) %>%
    summarise(Total = n_distinct(`Constituent ID`), .groups = 'drop')
  
  # Combine
  result <- yearly_data %>%
    left_join(total_unique, by = "Primary Constituency Code") %>%
    select(`Primary Constituency Code`, all_of(years), Total) %>%
    arrange(desc(Total))
  
  # Add totals row for each year
  if (nrow(result) > 0) {
    # Convert factor columns to character to avoid assignment issues
    if (is.factor(result$`Primary Constituency Code`)) {
      result$`Primary Constituency Code` <- as.character(result$`Primary Constituency Code`)
    }
    
    # Create totals row with same structure as result
    totals_row <- result[1, ]  # Copy structure
    totals_row[1, "Primary Constituency Code"] <- "TOTAL"
    
    # Calculate totals for each column
    for (col_name in names(result)) {
      if (col_name != "Primary Constituency Code" && col_name %in% names(result)) {
        if (is.numeric(result[[col_name]])) {
          totals_row[[col_name]] <- sum(result[[col_name]], na.rm = TRUE)
        }
      }
    }
    
    # Add totals row
    result <- rbind(result, totals_row)
  }
  
  result
}

# Average gift size analysis
calculate_avg_gift_analysis <- function(data, timeframe = "fiscal") {
  if (nrow(data) == 0) return(list(by_constituency = data.frame(), by_gift_type = data.frame()))
  
  # Get years
  if (timeframe == "calendar") {
    years <- sort(unique(format(as.Date(data$`Gift Date`), "%Y")))
    year_col <- "Calendar_Year"
    data <- data %>% mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
  } else {
    years <- sort(unique(data$`Fiscal Year`))
    year_col <- "Fiscal Year"
  }
  
  # Calculate averages by constituency and year
  constituency_avgs <- data %>%
    group_by(`Primary Constituency Code`, !!sym(year_col)) %>%
    summarise(Avg_Gift_Size = mean(`Fund Split Amount`, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = !!sym(year_col), values_from = Avg_Gift_Size, values_fill = 0)
  
  # Calculate overall average by constituency
  constituency_overall <- data %>%
    group_by(`Primary Constituency Code`) %>%
    summarise(`Overall Avg` = mean(`Fund Split Amount`, na.rm = TRUE), .groups = 'drop')
  
  # Combine constituency data
  constituency_result <- constituency_avgs %>%
    left_join(constituency_overall, by = "Primary Constituency Code") %>%
    select(`Primary Constituency Code`, all_of(years), `Overall Avg`)
  
  # Calculate averages by gift type and year
  gift_type_avgs <- data %>%
    group_by(`Gift Type`, !!sym(year_col)) %>%
    summarise(Avg_Gift_Size = mean(`Fund Split Amount`, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = !!sym(year_col), values_from = Avg_Gift_Size, values_fill = 0)
  
  # Calculate overall average by gift type
  gift_type_overall <- data %>%
    group_by(`Gift Type`) %>%
    summarise(`Overall Avg` = mean(`Fund Split Amount`, na.rm = TRUE), .groups = 'drop')
  
  # Combine gift type data
  gift_type_result <- gift_type_avgs %>%
    left_join(gift_type_overall, by = "Gift Type") %>%
    select(`Gift Type`, all_of(years), `Overall Avg`)
  
  # Format currency columns for both tables
  currency_cols <- c(years, "Overall Avg")
  
  # Format constituency table
  for (col in currency_cols) {
    if (col %in% names(constituency_result) && is.numeric(constituency_result[[col]])) {
      constituency_result[[col]] <- sapply(constituency_result[[col]], format_currency)
    }
  }
  
  # Format gift type table
  for (col in currency_cols) {
    if (col %in% names(gift_type_result) && is.numeric(gift_type_result[[col]])) {
      gift_type_result[[col]] <- sapply(gift_type_result[[col]], format_currency)
    }
  }
  
  list(by_constituency = constituency_result, by_gift_type = gift_type_result)
}

# Top donors
calculate_top_donors <- function(data, n = 50, timeframe = "fiscal") {
  if (nrow(data) == 0) return(data.frame())
  
  # Check if Name column exists, if not use Constituent ID
  if (!"Name" %in% names(data)) {
    data$Name <- data$`Constituent ID`  # Fallback to ID if Name not available
  }
  
  result <- process_data_by_group(data, c("Constituent ID", "Name", "Primary Constituency Code"), timeframe = timeframe) %>%
    head(n)
  
  # Format currency columns
  currency_cols <- names(result)[!names(result) %in% c("Constituent ID", "Name", "Primary Constituency Code")]
  for (col in currency_cols) {
    if (is.numeric(result[[col]])) {
      result[[col]] <- sapply(result[[col]], format_currency)
    }
  }
  
  result
}

# Gift size distribution
calculate_gift_distribution <- function(data, timeframe = "fiscal") {
  if (nrow(data) == 0) return(list(gift_counts = data.frame(), gift_amounts = data.frame()))
  
  # Get years
  if (timeframe == "calendar") {
    years <- sort(unique(format(as.Date(data$`Gift Date`), "%Y")))
    year_col <- "Calendar_Year"
    data <- data %>% mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
  } else {
    years <- sort(unique(data$`Fiscal Year`))
    year_col <- "Fiscal Year"
  }
  
  # Categorize gifts and count by year
  data_with_ranges <- data %>%
    mutate(`Gift Range` = sapply(`Fund Split Amount`, categorize_gift)) %>%
    mutate(`Gift Range` = factor(`Gift Range`, levels = CONFIG$gift_ranges$labels))
  
  # Count gifts and amounts by range and year
  base_data <- data_with_ranges %>%
    group_by(`Gift Range`, !!sym(year_col)) %>%
    summarise(
      Number_of_Gifts = n(),
      Total_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Create gift counts table
  gift_counts <- base_data %>%
    select(`Gift Range`, !!sym(year_col), Number_of_Gifts) %>%
    pivot_wider(
      names_from = !!sym(year_col),
      values_from = Number_of_Gifts,
      values_fill = 0
    ) %>%
    # Calculate total
    mutate(Total = rowSums(select(., all_of(years)), na.rm = TRUE)) %>%
    select(`Gift Range`, all_of(years), Total)
  
  # Create gift amounts table
  gift_amounts <- base_data %>%
    select(`Gift Range`, !!sym(year_col), Total_Amount) %>%
    pivot_wider(
      names_from = !!sym(year_col),
      values_from = Total_Amount,
      values_fill = 0
    ) %>%
    # Calculate total
    mutate(Total = rowSums(select(., all_of(years)), na.rm = TRUE)) %>%
    select(`Gift Range`, all_of(years), Total)
  
  # Format currency columns in amounts table
  currency_cols <- c(years, "Total")
  for (col in currency_cols) {
    if (col %in% names(gift_amounts) && is.numeric(gift_amounts[[col]])) {
      gift_amounts[[col]] <- sapply(gift_amounts[[col]], format_currency)
    }
  }
  
  list(gift_counts = gift_counts, gift_amounts = gift_amounts)
}

# Donor levels
calculate_donor_levels <- function(data, timeframe = "fiscal") {
  if (nrow(data) == 0) return(list(donor_counts = data.frame(), donor_amounts = data.frame()))
  
  # Calculate total giving per donor across all years
  constituent_totals <- data %>%
    group_by(`Constituent ID`) %>%
    summarise(`Total Given` = sum(`Fund Split Amount`, na.rm = TRUE), .groups = 'drop') %>%
    mutate(`Donor Level` = sapply(`Total Given`, categorize_gift)) %>%
    mutate(`Donor Level` = factor(`Donor Level`, levels = CONFIG$gift_ranges$labels))
  
  # Create donor counts table
  donor_counts <- constituent_totals %>%
    group_by(`Donor Level`) %>%
    summarise(`Number of Donors` = n(), .groups = 'drop') %>%
    mutate(`Percentage of Donors` = (`Number of Donors` / sum(`Number of Donors`)) * 100) %>%
    mutate(`Percentage of Donors` = sapply(`Percentage of Donors`, format_percent))
  
  # Create donor amounts table
  donor_amounts <- constituent_totals %>%
    group_by(`Donor Level`) %>%
    summarise(
      `Total Amount` = sum(`Total Given`, na.rm = TRUE),
      `Average per Donor` = mean(`Total Given`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(`Percentage of Amount` = (`Total Amount` / sum(`Total Amount`)) * 100) %>%
    mutate(
      `Total Amount` = sapply(`Total Amount`, format_currency),
      `Average per Donor` = sapply(`Average per Donor`, format_currency),
      `Percentage of Amount` = sapply(`Percentage of Amount`, format_percent)
    )
  
  list(donor_counts = donor_counts, donor_amounts = donor_amounts)
}

# =============================================================================
# UI COMPONENTS
# =============================================================================

# Filter panel
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
  
  # Include CSS
  tags$head(
    tags$style(HTML("
      .well { margin-bottom: 20px; }
      .page-header { text-align: center; }
      .info-box { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 0.375rem; }
      .download-buttons { margin: 15px 0; }
    "))
  ),
  
  # Title
  fluidRow(
    column(12,
      div(class = "page-header",
        style = "margin: 20px 0;",
        h2("Campaign Data Analysis"),
        div(class = "text-muted", style = "font-size: 12px; font-style: italic;",
          textOutput("lastUpdatedText")
        )
      )
    )
  ),
  
  # Main content
  navbarPage(
    title = NULL, id = "mainNav",
    
    # Data Tables Tab
    tabPanel("Data Tables",
      div(id = "dataTabFilter"),
      
      tabsetPanel(
        id = "dataTabset",
        tabPanel("Summary Statistics", 
          fluidRow(column(12, withSpinner(DT::dataTableOutput("summaryTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadSummary", "Download CSV", class = "btn btn-primary btn-sm")
          )))
        ),
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
        tabPanel("Unique Constituents",
          fluidRow(column(12, withSpinner(DT::dataTableOutput("constituentsTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadConstituents", "Download CSV", class = "btn btn-primary btn-sm")
          )))
        ),
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
        tabPanel("Top Donors",
          fluidRow(column(12, withSpinner(DT::dataTableOutput("topDonorsTable")))),
          fluidRow(column(12, div(class = "download-buttons text-right", style = "margin-top: 10px;",
            downloadButton("downloadTopDonors", "Download CSV", class = "btn btn-primary btn-sm")
          )))
        ),
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
  
  # Load data once
  full_data <- load_data()
  
  # Display last updated time
  output$lastUpdatedText <- renderText({
    last_updated <- attr(full_data, "last_updated")
    if (!is.null(last_updated)) {
      paste("Data last updated:", format(last_updated, "%B %d, %Y at %I:%M %p"))
    } else {
      ""
    }
  })
  
  # Get unique values for filters
  campaigns <- unique(full_data$`Campaign ID`)
  gift_types <- levels(full_data$`Gift Type`)
  fiscal_years <- sort(unique(full_data$`Fiscal Year`))
  calendar_years <- sort(unique(format(as.Date(full_data$`Gift Date`), "%Y")))
  
  # Insert filter panels
  insertUI(
    selector = "#dataTabFilter",
    ui = create_filter_panel("dataTab", campaigns, gift_types, fiscal_years)
  )
  
  insertUI(
    selector = "#vizFilter", 
    ui = create_filter_panel("viz", campaigns, gift_types, fiscal_years)
  )
  
  # Update year choices based on timeframe
  observe({
    timeframe <- input$dataTab_timeframe
    if (!is.null(timeframe) && length(timeframe) > 0) {
      years <- if (timeframe == "calendar") calendar_years else fiscal_years
      updateSelectInput(session, "dataTab_year", choices = years)
    }
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    # Check if required inputs are available
    req(input$dataTab_campaign, input$dataTab_timeframe)
    
    apply_data_filters(
      full_data,
      campaign = input$dataTab_campaign,
      gift_types = input$dataTab_giftType,
      years = input$dataTab_year,
      timeframe = input$dataTab_timeframe
    )
  })
  
  # Data table outputs
  output$summaryTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    summary_data <- calculate_summary_stats(filtered_data(), timeframe = timeframe)
    
    # Use the detailed data with year breakdown
    data <- summary_data$detailed_data
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data) <- new_names
    } else {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data) <- new_names
    }
    
    currency_cols <- which(sapply(data, is.numeric)) - 1
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE) %>%
      DT::formatCurrency(currency_cols + 1, currency = "$", digits = 0)
  })
  
  output$fundSplitSummaryTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_fund_split(filtered_data(), timeframe = timeframe)
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data$summary_table)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data$summary_table) <- new_names
    } else {
      old_names <- names(data$summary_table)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data$summary_table) <- new_names
    }
    
    DT::datatable(data$summary_table, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fundSplitDetailTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_fund_split(filtered_data(), timeframe = timeframe)
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data$detailed_data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data$detailed_data) <- new_names
    } else {
      old_names <- names(data$detailed_data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data$detailed_data) <- new_names
    }
    
    DT::datatable(data$detailed_data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fundAnalysisSummaryTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_fund_analysis(filtered_data(), timeframe = timeframe)
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data$summary_table)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data$summary_table) <- new_names
    } else {
      old_names <- names(data$summary_table)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data$summary_table) <- new_names
    }
    
    DT::datatable(data$summary_table, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fundAnalysisDetailTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_fund_analysis(filtered_data(), timeframe = timeframe)
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data$detailed_data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data$detailed_data) <- new_names
    } else {
      old_names <- names(data$detailed_data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data$detailed_data) <- new_names
    }
    
    DT::datatable(data$detailed_data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$constituentsTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_unique_constituents(filtered_data(), timeframe = timeframe)
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data) <- new_names
    } else {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data) <- new_names
    }
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$avgGiftConstituencyTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_avg_gift_analysis(filtered_data(), timeframe = timeframe)$by_constituency
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data) <- new_names
    } else {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data) <- new_names
    }
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$avgGiftTypeTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_avg_gift_analysis(filtered_data(), timeframe = timeframe)$by_gift_type
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data) <- new_names
    } else {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data) <- new_names
    }
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$topDonorsTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_top_donors(filtered_data(), timeframe = timeframe)
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data) <- new_names
    } else {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data) <- new_names
    }
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$giftCountTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_gift_distribution(filtered_data(), timeframe = timeframe)$gift_counts
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data) <- new_names
    } else {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data) <- new_names
    }
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$giftAmountTable <- DT::renderDataTable({
    timeframe <- if (!is.null(input$dataTab_timeframe)) input$dataTab_timeframe else "fiscal"
    data <- calculate_gift_distribution(filtered_data(), timeframe = timeframe)$gift_amounts
    
    # Add FY/CY prefix to year columns
    if (timeframe == "fiscal") {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("FY ", old_names[i])
        }
      }
      names(data) <- new_names
    } else {
      old_names <- names(data)
      new_names <- old_names
      for (i in seq_along(old_names)) {
        if (grepl("^\\d{4}$", old_names[i])) {
          new_names[i] <- paste0("CY ", old_names[i])
        }
      }
      names(data) <- new_names
    }
    
    DT::datatable(data, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
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
  
  output$fullDataTable <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  # Download handlers
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

# Run the application
shinyApp(ui = ui, server = server) 