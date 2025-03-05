# Data processing functions

# Process summary data (Gift Type by Fiscal Year)
process_summary_data <- function(filtered_data) {
  filtered_data <- filtered_data %>%
    mutate(`Fund Split Amount` = as.numeric(`Fund Split Amount`))
  
  process_data_by_group(filtered_data, group_cols = "Gift Type")
}

# Process fund split data (Constituency Code by Fiscal Year)
process_fund_split_data <- function(filtered_data) {
  process_data_by_group(filtered_data, group_cols = "Primary Constituency Code")
}

# Process fund analysis data (Fund by Fiscal Year)
process_fund_analysis_data <- function(filtered_data) {
  process_data_by_group(filtered_data, group_cols = c("Fund ID", "Fund Description"))
}

# Process unique constituents data (Unique constituents by Constituency and Fiscal Year)
process_unique_constituents_data <- function(filtered_data) {
  fiscal_years <- sort(unique(filtered_data$`Fiscal Year`))
  
  # Get yearly counts
  yearly_data <- filtered_data %>%
    group_by(`Primary Constituency Code`, `Fiscal Year`) %>%
    summarise(Unique_Constituents = n_distinct(`Constituent ID`, na.rm = TRUE),
              .groups = 'drop') %>%
    pivot_wider(names_from = `Fiscal Year`,
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
    select(`Primary Constituency Code`, all_of(fiscal_years), Total)
}

# Process top donors data
process_top_donors_data <- function(filtered_data) {
  process_data_by_group(filtered_data, 
                        group_cols = c("Constituent ID", "Key Indicator", "Name"))
}

# Process average gift size data
process_avg_gift_data <- function(filtered_data) {
  fiscal_years <- sort(unique(filtered_data$`Fiscal Year`))
  
  # Calculate averages by fiscal year
  yearly_avgs <- filtered_data %>%
    group_by(`Gift Type`, `Fiscal Year`) %>%
    summarise(Avg_Gift_Size = sum(`Fund Split Amount`, na.rm = TRUE) / n(),
              .groups = 'drop') %>%
    pivot_wider(names_from = `Fiscal Year`, 
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
    select(`Gift Type`, all_of(fiscal_years), `Overall Avg`)
}

# Process gift distribution data
process_gift_distribution_data <- function(filtered_data) {
  fiscal_years <- sort(unique(filtered_data$`Fiscal Year`))
  
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
}

# Process donor levels summary
process_donor_levels_summary <- function(filtered_data) {
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
  
  # Calculate total giving per donor across all selected fiscal years
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

# Process donor levels data by fiscal year
process_donor_levels_data <- function(filtered_data) {
  fiscal_years <- sort(unique(filtered_data$`Fiscal Year`))
  
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
  
  # Process data by fiscal year
  fiscal_year_data <- filtered_data %>%
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
      Donor_Level = sapply(Total_Giving, assign_range_label),
      Donor_Level = factor(Donor_Level, levels = range_labels)
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
}

# Process donor levels data by fiscal year
process_donor_levels_data <- function(filtered_data) {
  fiscal_years <- sort(unique(filtered_data$`Fiscal Year`))
  
  # Process data by fiscal year
  fiscal_year_data <- filtered_data %>%
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
}