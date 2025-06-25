# =============================================================================
# HELPER FUNCTIONS
# =============================================================================
# This file contains utility functions used throughout the OODAE Gift Report
# Dashboard. These functions handle formatting, categorization, and other 
# common operations.
# =============================================================================

# Source configuration
source("config.R")

# =============================================================================
# FORMATTING FUNCTIONS
# =============================================================================

#' Format currency values with optional abbreviation
#' 
#' @param x Numeric value to format
#' @param abbreviate Logical, whether to use K/M abbreviations for large numbers
#' @return Character string with formatted currency
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

#' Format percentage values
#' 
#' @param x Numeric value to format as percentage
#' @return Character string with formatted percentage
format_percent <- function(x) {
  if (is.na(x)) return("0%")
  paste0(round(x, 1), "%")
}

# =============================================================================
# CATEGORIZATION FUNCTIONS
# =============================================================================

#' Categorize gift amounts into predefined ranges
#' 
#' @param amount Numeric gift amount
#' @param labels Character vector of range labels (from config)
#' @param thresholds Numeric vector of range thresholds (from config)
#' @return Character string indicating the gift range category
categorize_gift <- function(amount, labels = CONFIG$gift_ranges$labels, thresholds = CONFIG$gift_ranges$thresholds) {
  for (i in seq_along(thresholds)) {
    if (amount >= thresholds[i]) {
      return(labels[i])
    }
  }
  labels[length(labels)]
}

#' Determine primary constituency code based on hierarchy
#' 
#' @param codes Character vector of constituency codes for a constituent
#' @param hierarchy Character vector defining the priority order
#' @return Character string of the primary constituency code, or NA if none match
get_primary_code <- function(codes, hierarchy) {
  for (h in hierarchy) {
    if (h %in% codes) return(h)
  }
  NA
}

# =============================================================================
# DATA FILTERING FUNCTIONS
# =============================================================================

#' Apply filters to gift data based on user selections
#' 
#' @param data Data frame containing gift data
#' @param campaign Character, campaign ID to filter by (optional)
#' @param gift_types Character vector, gift types to include (optional)
#' @param years Character vector, years to include (optional)
#' @param timeframe Character, either "fiscal" or "calendar" year
#' @return Filtered data frame
apply_data_filters <- function(data, campaign = NULL, gift_types = NULL, years = NULL, timeframe = "fiscal") {
  filtered <- data
  
  # Filter by campaign if specified
  if (!is.null(campaign) && campaign != "ALL") {
    filtered <- filtered %>% filter(`Campaign ID` == campaign)
  }
  
  # Filter by gift types if specified
  if (length(gift_types) > 0) {
    filtered <- filtered %>% filter(`Gift Type` %in% gift_types)
  }
  
  # Filter by years if specified
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
# VALIDATION FUNCTIONS
# =============================================================================

#' Check if required data files exist
#' 
#' @return Logical vector indicating which files exist
check_data_files <- function() {
  files_exist <- c(
    gifts = file.exists(PATHS$raw_gifts),
    funds = file.exists(PATHS$raw_funds)
  )
  return(files_exist)
}

#' Check if processed data is up to date
#' 
#' @return Logical indicating if processed data exists and is newer than raw data
is_processed_data_current <- function() {
  # Check if processed data exists
  if (!file.exists(PATHS$processed_data)) {
    return(FALSE)
  }
  
  # Get modification times
  processed_time <- file.info(PATHS$processed_data)$mtime
  raw_gifts_time <- file.info(PATHS$raw_gifts)$mtime
  raw_funds_time <- file.info(PATHS$raw_funds)$mtime
  
  # Processed data is current if it's newer than both raw files
  return(processed_time > raw_gifts_time && processed_time > raw_funds_time)
}

# =============================================================================
# LOGGING FUNCTIONS
# =============================================================================

#' Write a log message with timestamp
#' 
#' @param message Character string to log
#' @param level Character, log level ("INFO", "WARNING", "ERROR")
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] ", level, ": ", message)
  
  # Create log directory if it doesn't exist
  log_dir <- dirname(PATHS$log_file)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  # Write to log file
  cat(log_entry, "\n", file = PATHS$log_file, append = TRUE)
  
  # Also print to console
  cat(log_entry, "\n")
} 