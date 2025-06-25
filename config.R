# =============================================================================
# CONFIGURATION FILE
# =============================================================================
# This file contains all configuration settings for the OODAE Gift Report
# Dashboard. Modify these settings to customize behavior without changing
# the main application logic.
# =============================================================================

# Dashboard appearance configuration
CONFIG <- list(
  # Shiny theme for the dashboard
  theme = "readable",
  
  # Hierarchy for determining primary constituency code
  # When a constituent has multiple codes, the first match in this list is used
  # NA represents NULL/empty values and should be sorted last
  constituency_hierarchy = c("UMMC Alumni", "UMMC Affilate", "Organization", "Individuals", NA_character_),
  
  # Mapping from raw constituency codes to standardized categories
  # This consolidates similar constituency types into broader categories
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
  
  # Mapping from raw gift types to standardized categories
  # This consolidates similar gift types for analysis purposes
  gift_type_mapping = list(
    "Cash" = "Cash",
    "Cash" = "Recurring Gift Pay-Cash",
    "Stock/Property" = "Stock/Property",
    "Planned Gift" = "Planned Gift",
    "Pledge" = "Pledge"
  ),
  
  # Gift amount ranges for categorization and analysis
  # Labels and thresholds must be in descending order by amount
  gift_ranges = list(
    labels = c("$150,000+", "$75,000 - $149,999", "$40,000 - $74,999", "$20,000 - $39,999",
               "$10,000 - $19,999", "$5,000 - $9,999", "$2,500 - $4,999", "$1,000 - $2,499",
               "$500 - $999", "$100 - $499", "Under $100"),
    thresholds = c(150000, 75000, 40000, 20000, 10000, 5000, 2500, 1000, 500, 100, 0)
  )
)

# File paths configuration
PATHS <- list(
  # Raw data files
  raw_gifts = "data/AllGifts.CSV",
  raw_funds = "data/FundList.CSV",
  
  # Processed data files
  processed_data = "data/processed/clean_data.rds",
  last_updated = "data/processed/last_updated.txt",
  
  # Log file for data preparation
  log_file = "data/processed/data_prep.log"
)

# Data preparation configuration
PREP_CONFIG <- list(
  # Date format in the raw data
  date_format = "%m/%d/%Y",
  
  # Fiscal year starts in July (month 7)
  fiscal_year_start_month = 7,
  
  # Minimum gift amount to include in analysis
  min_gift_amount = 0,
  
  # Whether to exclude zero-amount gifts
  exclude_zero_gifts = TRUE
) 