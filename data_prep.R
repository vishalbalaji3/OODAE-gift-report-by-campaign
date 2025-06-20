# =============================================================================
# DATA PREPARATION SCRIPT
# =============================================================================
# This script loads, cleans, and processes raw gift data for the OODAE Gift
# Report Dashboard. It can be run independently to prepare data for various
# reporting tools (Shiny, Quarto, etc.).
# 
# Usage: source("data_prep.R") or Rscript data_prep.R
# =============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(forcats)
})

# Source configuration and helper functions
source("config.R")
source("helpers.R")

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Load and clean the main gifts data
#' 
#' @return Data frame with cleaned gift data
load_gifts_data <- function() {
  log_message("Loading raw gifts data...")
  
  # Check if file exists
  if (!file.exists(PATHS$raw_gifts)) {
    stop("Gifts data file not found: ", PATHS$raw_gifts)
  }
  
  # Load main data with appropriate column types
  gifts_data <- read_csv(PATHS$raw_gifts,
                        col_types = cols(
                          .default = "c",
                          "Key Indicator" = "f",
                          "Constituency Code" = "f",
                          "Gift Date" = col_date(PREP_CONFIG$date_format),
                          "Gift Type" = "f",
                          "Gift Subtype" = "f",
                          "Gift Amount" = col_number(),
                          "Gift Receipt Amount" = col_number(),
                          "Gift Pledge Balance" = col_number(),
                          "Fund Split Amount" = col_number()
                        ),
                        show_col_types = FALSE)
  
  log_message(paste("Loaded", nrow(gifts_data), "gift records"))
  return(gifts_data)
}

#' Load and clean the fund list data
#' 
#' @return Data frame with fund information
load_funds_data <- function() {
  log_message("Loading fund list data...")
  
  # Check if file exists
  if (!file.exists(PATHS$raw_funds)) {
    stop("Fund list file not found: ", PATHS$raw_funds)
  }
  
  funds_data <- read_csv(PATHS$raw_funds, show_col_types = FALSE)
  
  log_message(paste("Loaded", nrow(funds_data), "fund records"))
  return(funds_data)
}

# =============================================================================
# DATA CLEANING FUNCTIONS
# =============================================================================

#' Apply constituency and gift type mappings
#' 
#' @param data Raw gifts data frame
#' @return Data frame with mapped values
apply_mappings <- function(data) {
  log_message("Applying constituency and gift type mappings...")
  
  # Apply constituency mapping
  data <- data %>%
    mutate(`Constituency Code` = fct_recode(`Constituency Code`, !!!CONFIG$constituency_mapping))
  
  # Apply gift type mapping
  data <- data %>%
    mutate(`Gift Type` = fct_recode(`Gift Type`, !!!CONFIG$gift_type_mapping))
  
  log_message("Mappings applied successfully")
  return(data)
}

#' Calculate fiscal years and primary constituency codes
#' 
#' @param data Gifts data frame with mappings applied
#' @return Data frame with calculated fields
calculate_derived_fields <- function(data) {
  log_message("Calculating derived fields (fiscal year, primary constituency)...")
  
  data <- data %>%
    group_by(`Constituent ID`) %>%
    mutate(
      # Calculate fiscal year (starts in July)
      `Fiscal Year` = ifelse(
        as.numeric(format(as.Date(`Gift Date`), "%m")) >= PREP_CONFIG$fiscal_year_start_month,
        as.numeric(format(as.Date(`Gift Date`), "%Y")) + 1,
        as.numeric(format(as.Date(`Gift Date`), "%Y"))
      ) %>% as.character(),
      
      # Determine primary constituency code based on hierarchy
      `Primary Constituency Code` = get_primary_code(`Constituency Code`, CONFIG$constituency_hierarchy)
    ) %>%
    ungroup()
  
  log_message("Derived fields calculated successfully")
  return(data)
}

#' Clean and filter the data
#' 
#' @param data Data frame with derived fields
#' @return Cleaned and filtered data frame
clean_and_filter_data <- function(data) {
  log_message("Cleaning and filtering data...")
  
  initial_rows <- nrow(data)
  
  data <- data %>%
    # Remove old constituency code column
    select(-`Constituency Code`) %>%
    # Remove duplicates
    distinct() %>%
    # Set up proper factor levels for primary constituency
    mutate(`Primary Constituency Code` = factor(`Primary Constituency Code`, 
                                               levels = CONFIG$constituency_hierarchy, 
                                               ordered = TRUE))
  
  # Filter out zero-amount gifts if configured to do so
  if (PREP_CONFIG$exclude_zero_gifts) {
    data <- data %>% filter(`Gift Amount` != PREP_CONFIG$min_gift_amount)
  }
  
  final_rows <- nrow(data)
  removed_rows <- initial_rows - final_rows
  
  log_message(paste("Data cleaning complete. Removed", removed_rows, "rows,", final_rows, "rows remaining"))
  return(data)
}

# =============================================================================
# DATA INTEGRATION FUNCTIONS
# =============================================================================

#' Merge gifts data with fund information
#' 
#' @param gifts_data Cleaned gifts data
#' @param funds_data Fund list data
#' @return Integrated data frame
integrate_fund_data <- function(gifts_data, funds_data) {
  log_message("Integrating gifts data with fund information...")
  
  # Merge with fund list
  integrated_data <- merge(gifts_data, funds_data, by = "Fund ID", all.x = TRUE)
  
  # Clean up campaign ID columns
  integrated_data <- integrated_data %>%
    select(-`Campaign ID`) %>%
    rename("Campaign ID" = "Fund Default Campaign ID")
  
  log_message("Fund data integration complete")
  return(integrated_data)
}

# =============================================================================
# DATA PROCESSING FUNCTIONS
# =============================================================================

#' Core function to process data by group with year breakdown
#' 
#' @param data Data frame containing gift data
#' @param group_cols Character vector of columns to group by
#' @param value_col Character, column to sum (default: "Fund Split Amount")
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return Processed data frame with year columns and totals
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
  
  return(result)
}

# =============================================================================
# ANALYSIS FUNCTIONS
# =============================================================================

#' Calculate campaign overview statistics
#' 
#' @param data Processed gift data
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return List with summary table and detailed data
calculate_campaign_overview <- function(data, timeframe = "fiscal") {
  if (nrow(data) == 0) {
    return(list(
      summary_table = data.frame(
        Metric = c("Total Gifts", "Total Amount", "Unique Constituents", "Average Gift Size"),
        Value = c("0", "$0", "0", "$0")
      ),
      detailed_data = data.frame()
    ))
  }
  
  # Process by Gift Type with year breakdown
  gift_type_data <- process_data_by_group(data, "Gift Type", timeframe = timeframe)
  
  # Also create simple summary for display
  total_gifts <- nrow(data)
  total_amount <- sum(data$`Fund Split Amount`, na.rm = TRUE)
  unique_constituents <- n_distinct(data$`Constituent ID`)
  avg_gift <- if(total_gifts > 0) total_amount / total_gifts else 0
  
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

#' Calculate giving analysis by constituency
#' 
#' @param data Processed gift data
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return List with summary table and detailed data
calculate_giving_by_constituency <- function(data, timeframe = "fiscal") {
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
  
  # Note: Currency formatting will be handled by DataTables for proper sorting
  
  list(summary_table = summary_table, detailed_data = result)
}

#' Calculate fund analysis
#' 
#' @param data Processed gift data
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return List with summary table and detailed data
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
  
  # Note: Currency formatting will be handled by DataTables for proper sorting
  
  list(summary_table = summary_table, detailed_data = result)
}

#' Calculate constituency breakdown analysis
#' 
#' @param data Processed gift data
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return Data frame with unique constituent counts by year
calculate_constituency_breakdown <- function(data, timeframe = "fiscal") {
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
  
  result
}

#' Calculate average gift size insights
#' 
#' @param data Processed gift data
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return List with by_constituency and by_gift_type data frames
calculate_average_gift_insights <- function(data, timeframe = "fiscal") {
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
  
  # Note: Currency formatting will be handled by DataTables for proper sorting
  
  list(by_constituency = constituency_result, by_gift_type = gift_type_result)
}

#' Calculate top donors
#' 
#' @param data Processed gift data
#' @param n Number of top donors to return
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return Data frame with top donors
calculate_top_donors <- function(data, n = 50, timeframe = "fiscal") {
  if (nrow(data) == 0) return(data.frame())
  
  # Check if Name column exists, if not use Constituent ID
  if (!"Name" %in% names(data)) {
    data$Name <- data$`Constituent ID`  # Fallback to ID if Name not available
  }
  
  result <- process_data_by_group(data, c("Constituent ID", "Name", "Primary Constituency Code"), timeframe = timeframe) %>%
    head(n)
  
  # Note: Currency formatting will be handled by DataTables for proper sorting
  
  result
}

#' Calculate gift range analysis
#' 
#' @param data Processed gift data
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return List with gift_counts and gift_amounts data frames
calculate_gift_range_analysis <- function(data, timeframe = "fiscal") {
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
  
  # Note: Currency formatting will be handled by DataTables for proper sorting
  
  list(gift_counts = gift_counts, gift_amounts = gift_amounts)
}

#' Calculate donor levels analysis
#' 
#' @param data Processed gift data
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return List with donor_counts and donor_amounts data frames
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
      `Percentage of Amount` = sapply(`Percentage of Amount`, format_percent)
    )
  
  # Note: Currency formatting will be handled by DataTables for proper sorting
  
  list(donor_counts = donor_counts, donor_amounts = donor_amounts)
}

# =============================================================================
# MAIN DATA PREPARATION FUNCTION
# =============================================================================

#' Main function to prepare all data
#' 
#' @param force_refresh Logical, whether to force data refresh even if current
#' @return Processed data frame ready for analysis
prepare_data <- function(force_refresh = FALSE) {
  log_message("Starting data preparation process...")
  
  # Check if we need to refresh the data
  if (!force_refresh && is_processed_data_current()) {
    log_message("Processed data is current, loading from cache...")
    return(readRDS(PATHS$processed_data))
  }
  
  # Check that required raw data files exist
  file_checks <- check_data_files()
  if (!all(file_checks)) {
    missing_files <- names(file_checks)[!file_checks]
    stop("Missing required data files: ", paste(missing_files, collapse = ", "))
  }
  
  # Load raw data
  gifts_data <- load_gifts_data()
  funds_data <- load_funds_data()
  
  # Apply data cleaning and processing steps
  gifts_data <- gifts_data %>%
    apply_mappings() %>%
    calculate_derived_fields() %>%
    clean_and_filter_data()
  
  # Integrate with fund data
  processed_data <- integrate_fund_data(gifts_data, funds_data)
  
  # Add metadata attributes
  attr(processed_data, "processed_at") <- Sys.time()
  attr(processed_data, "gifts_file_time") <- file.info(PATHS$raw_gifts)$mtime
  attr(processed_data, "funds_file_time") <- file.info(PATHS$raw_funds)$mtime
  
  # Save processed data
  log_message("Saving processed data...")
  saveRDS(processed_data, PATHS$processed_data)
  
  # Save last updated timestamp
  writeLines(as.character(Sys.time()), PATHS$last_updated)
  
  log_message("Data preparation complete!")
  return(processed_data)
}

# =============================================================================
# SCRIPT EXECUTION
# =============================================================================

# If this script is run directly (not sourced), prepare the data
if (!interactive() && identical(commandArgs(trailingOnly = TRUE), character(0))) {
  cat("Running data preparation script...\n")
  tryCatch({
    prepare_data(force_refresh = TRUE)
    cat("Data preparation completed successfully!\n")
  }, error = function(e) {
    cat("Error in data preparation:", e$message, "\n")
    quit(status = 1)
  })
} 