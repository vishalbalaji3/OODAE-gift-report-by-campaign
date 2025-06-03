library(forcats)    # Needed for fct_recode

# Source the config file to get configuration values
source("config.R")

# Get constituency hierarchy from config
cons_code_hierarchy <- config$constituency$hierarchy

get_primary_code <- function(codes, hierarchy) {
  for (h in hierarchy) {
    if (h %in% codes) {
      return(h)
    }
  }
  NA
}

full_data <- read_csv("data/AllGifts.CSV",
                     col_types = cols(.default = "c",
                                      "Key Indicator" = "f",
                                      "Constituency Code" = "f",
                                      "Gift Date" = col_date(
                                        config$date_format$input
                                      ),
                                      "Gift Type" = "f",
                                      "Gift Subtype" = "f",
                                      "Gift Amount" = col_number(),
                                      "Gift Receipt Amount" = col_number(),
                                      "Gift Pledge Balance" = col_number(),
                                      "Fund Split Amount" = col_number()
                     )) %>%
  # Use constituency mapping from config
  mutate(`Constituency Code` = fct_recode(`Constituency Code`, 
                                          !!!config$constituency$mapping),
         # Use gift type mapping from config
         `Gift Type` = fct_recode(`Gift Type`, !!!config$gift_type$mapping)) %>%
  group_by(`Constituent ID`) %>%
  mutate(
    `Primary Constituency Code` = get_primary_code(`Constituency Code`, 
                                                   cons_code_hierarchy),
    `Fiscal Year` = as.Date(                        # Convert to Date
      paste0(                                     # Create year string
        ifelse(
          as.numeric(format(as.Date(`Gift Date`), "%m")) >= 7,
          as.numeric(format(as.Date(`Gift Date`), "%Y")) + 1,
          as.numeric(format(as.Date(`Gift Date`), "%Y"))
        ),
        # Add month and day for date conversion
        "-01-01"
      )
    )
  ) %>%
  # Format to show only year
  mutate(`Fiscal Year` = format(`Fiscal Year`, "%Y")) %>%
  select(-`Constituency Code`) %>%
  distinct() %>%
  ungroup() %>%
  mutate(`Primary Constituency Code` = factor(`Primary Constituency Code`, 
                                              levels = cons_code_hierarchy, 
                                              ordered = TRUE)) %>%
  filter(`Gift Amount` != 0)

fund_list <- read_csv("data/FundList.CSV")
full_data <- merge(full_data, fund_list, by = "Fund ID", all.x = TRUE) %>% 
  select(-`Campaign ID`) %>% 
  rename("Campaign ID" = "Fund Default Campaign ID")

# Get the last modified date of the data file
data_file_info <- file.info("data/AllGifts.CSV")
data_last_modified <- data_file_info$mtime
attr(full_data, "last_updated") <- data_last_modified

# Create FullData for backward compatibility
FullData <- full_data
