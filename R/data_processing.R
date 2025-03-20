# Data processing functions

# Process summary data (Gift Type by Year)
process_summary_data <- function(filtered_data, time_period = "fiscal") {
  filtered_data <- filtered_data %>%
    mutate(`Fund Split Amount` = as.numeric(`Fund Split Amount`))
  
  process_data_by_group(filtered_data, group_cols = "Gift Type", time_period = time_period)
}

# Process fund split data (Constituency Code by Year)
process_fund_split_data <- function(filtered_data, time_period = "fiscal") {
  process_data_by_group(filtered_data, group_cols = "Primary Constituency Code", time_period = time_period)
}

# Process fund analysis data (Fund by Year)
process_fund_analysis_data <- function(filtered_data, time_period = "fiscal") {
  process_data_by_group(filtered_data, group_cols = c("Fund ID", "Fund Description"), time_period = time_period)
}

# Process unique constituents data (Unique constituents by Constituency and Year)
process_unique_constituents_data <- function(filtered_data, time_period = "fiscal") {
  # Get years based on time period
  if (time_period == "calendar") {
    years <- sort(unique(format(as.Date(filtered_data$`Gift Date`), "%Y")))
    year_col <- "Calendar_Year"
    # Add a Calendar_Year column
    filtered_data <- filtered_data %>%
      mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
  } else {
    years <- sort(unique(filtered_data$`Fiscal Year`))
    year_col <- "Fiscal Year"
  }
  
  # Get yearly counts
  yearly_data <- filtered_data %>%
    group_by(`Primary Constituency Code`, !!sym(year_col)) %>%
    summarise(Unique_Constituents = n_distinct(`Constituent ID`, na.rm = TRUE),
              .groups = 'drop') %>%
    pivot_wider(names_from = !!sym(year_col),
                values_from = Unique_Constituents,
                values_fill = 0)
  
  # Calculate total unique constituents across all years for each constituency code
  total_unique <- filtered_data %>%
    group_by(`Primary Constituency Code`) %>%
    summarise(Total = n_distinct(`Constituent ID`, na.rm = TRUE),
              .groups = 'drop')
  
  # Combine yearly data with total unique constituents
  yearly_data %>%
    left_join(total_unique, by = "Primary Constituency Code") %>%
    select(`Primary Constituency Code`, all_of(years), Total)
}

# Process top donors data
process_top_donors_data <- function(filtered_data, time_period = "fiscal") {
  process_data_by_group(filtered_data, 
                        group_cols = c("Constituent ID", "Key Indicator", "Name"),
                        time_period = time_period)
}

# Process average gift size data
process_avg_gift_data <- function(filtered_data, time_period = "fiscal") {
  # Get years based on time period
  if (time_period == "calendar") {
    years <- sort(unique(format(as.Date(filtered_data$`Gift Date`), "%Y")))
    year_col <- "Calendar_Year"
    # Add a Calendar_Year column
    filtered_data <- filtered_data %>%
      mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
  } else {
    years <- sort(unique(filtered_data$`Fiscal Year`))
    year_col <- "Fiscal Year"
  }
  
  # Calculate averages by year
  yearly_avgs <- filtered_data %>%
    group_by(`Gift Type`, !!sym(year_col)) %>%
    summarise(Avg_Gift_Size = sum(`Fund Split Amount`, na.rm = TRUE) / n(),
              .groups = 'drop') %>%
    pivot_wider(names_from = !!sym(year_col), 
                values_from = Avg_Gift_Size,
                values_fill = 0)
  
  # Calculate overall average
  `Overall Avg` <- filtered_data %>%
    group_by(`Gift Type`) %>%
    summarise(`Overall Avg` = sum(`Fund Split Amount`, na.rm = TRUE) / n(),
              .groups = 'drop')
  
  # Combine yearly averages with overall average
  yearly_avgs %>%
    left_join(`Overall Avg`, by = "Gift Type") %>%
    select(`Gift Type`, all_of(years), `Overall Avg`)
}

# Process gift distribution data
process_gift_distribution_data <- function(filtered_data, time_period = "fiscal") {
  # Get years based on time period
  if (time_period == "calendar") {
    years <- sort(unique(format(as.Date(filtered_data$`Gift Date`), "%Y")))
    year_col <- "Calendar_Year"
    # Add a Calendar_Year column
    filtered_data <- filtered_data %>%
      mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
  } else {
    years <- sort(unique(filtered_data$`Fiscal Year`))
    year_col <- "Fiscal Year"
  }
  
  # Get range labels and thresholds from config
  range_labels <- config$distribution_ranges$labels
  range_thresholds <- config$distribution_ranges$thresholds
  
  # Create a function that assigns a label based on amount
  assign_range_label <- function(amount) {
    for (i in seq_along(range_thresholds)) {
      if (amount >= range_thresholds[i]) {
        return(range_labels[i])
      }
    }
    return(tail(range_labels, 1)) # Return the last label as default
  }
  
  filtered_data %>%
    mutate(
      # Use the function to assign labels
      Gift_Range = sapply(`Fund Split Amount`, assign_range_label),
      # Convert to factor to maintain order
      Gift_Range = factor(Gift_Range, levels = range_labels)
    ) %>%
    group_by(Gift_Range, !!sym(year_col)) %>%
    summarise(
      Number_of_Gifts = n(),
      Total_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_wider(
      names_from = !!sym(year_col),
      values_from = c(Number_of_Gifts, Total_Amount),
      values_fill = 0
    ) %>%
    # Calculate totals across all years
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
}

# Process donor levels summary
process_donor_levels_summary <- function(filtered_data, time_period = "fiscal") {
  # Get range labels and thresholds from config
  range_labels <- config$distribution_ranges$labels
  range_thresholds <- config$distribution_ranges$thresholds
  
  # Create a function that assigns a label based on amount
  assign_range_label <- function(amount) {
    for (i in seq_along(range_thresholds)) {
      if (amount >= range_thresholds[i]) {
        return(range_labels[i])
      }
    }
    return(tail(range_labels, 1)) # Return the last label as default
  }
  
  # Calculate total giving per donor across all selected years
  aggregate_donor_data <- filtered_data %>%
    group_by(`Constituent ID`, `Name`) %>%
    summarise(
      Total_Giving = sum(`Fund Split Amount`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Categorize donors by giving level
    mutate(
      Donor_Level = sapply(Total_Giving, assign_range_label),
      Donor_Level = factor(Donor_Level, levels = range_labels)
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
}

# Process donor levels data by year
process_donor_levels_data <- function(filtered_data, time_period = "fiscal") {
  # Get years based on time period
  if (time_period == "calendar") {
    years <- sort(unique(format(as.Date(filtered_data$`Gift Date`), "%Y")))
    year_col <- "Calendar_Year"
    # Add a Calendar_Year column
    filtered_data <- filtered_data %>%
      mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
  } else {
    years <- sort(unique(filtered_data$`Fiscal Year`))
    year_col <- "Fiscal Year"
  }
  
  # Process data by year
  year_data <- filtered_data %>%
    group_by(`Constituent ID`, !!sym(year_col)) %>%
    summarise(
      Year_Amount = sum(`Fund Split Amount`, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Get range labels and thresholds from config
  range_labels <- config$distribution_ranges$labels
  range_thresholds <- config$distribution_ranges$thresholds
  
  # Create a function that assigns a label based on amount
  assign_range_label <- function(amount) {
    for (i in seq_along(range_thresholds)) {
      if (amount >= range_thresholds[i]) {
        return(range_labels[i])
      }
    }
    return(tail(range_labels, 1)) # Return the last label as default
  }
  
  # Calculate total giving per donor across all selected years
  total_giving <- year_data %>%
    group_by(`Constituent ID`) %>%
    summarise(
      Total_Giving = sum(Year_Amount, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Categorize donors by giving level
    mutate(
      Donor_Level = sapply(Total_Giving, assign_range_label),
      Donor_Level = factor(Donor_Level, levels = range_labels)
    )
  
  # Join the donor levels back to the fiscal year data
  donor_by_year <- year_data %>%
    left_join(total_giving %>% select(`Constituent ID`, Donor_Level), by = "Constituent ID")
  
  # Count donors and sum amounts by level and year
  donor_counts_by_year <- donor_by_year %>%
    group_by(Donor_Level, !!sym(year_col)) %>%
    summarise(
      Donors = n_distinct(`Constituent ID`),
      Amount = sum(Year_Amount, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Create prefix based on time period
  prefix <- ifelse(time_period == "calendar", "CY", "FY")
  
  # Pivot to get years as columns for donors
  donor_counts_pivot <- donor_counts_by_year %>%
    select(Donor_Level, !!sym(year_col), Donors) %>%
    pivot_wider(
      names_from = !!sym(year_col),
      values_from = Donors,
      values_fill = 0,
      names_prefix = paste0("Donors_", prefix, "_")
    )
  
  # Pivot to get years as columns for amounts
  amount_pivot <- donor_counts_by_year %>%
    select(Donor_Level, !!sym(year_col), Amount) %>%
    pivot_wider(
      names_from = !!sym(year_col),
      values_from = Amount,
      values_fill = 0,
      names_prefix = paste0("Amount_", prefix, "_")
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
}