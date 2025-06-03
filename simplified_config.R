# Simplified Configuration for OODAE Gift Report
# This replaces the complex config.R with essential settings only

# Essential configuration
config <- list(
  # UI theme
  theme = "readable",
  
  # Date format for data input
  date_format = "%m/%d/%Y",
  
  # Constituency hierarchy (simplified)
  constituencies = c("UMMC Alumni", "UMMC Affiliate", "Organization", "Individuals", "Other"),
  
  # Gift types (simplified)
  gift_types = c("Cash", "Stock/Property", "Planned Gift", "Pledge"),
  
  # Common gift ranges for analysis
  gift_ranges = c("Under $100", "$100-$499", "$500-$999", "$1,000-$2,499", 
                  "$2,500-$4,999", "$5,000-$9,999", "$10,000-$19,999", 
                  "$20,000-$39,999", "$40,000-$74,999", "$75,000-$149,999", "$150,000+"),
  
  # Simple formatting functions
  format_currency = function(x) {
    ifelse(x >= 1000000, 
           paste0("$", round(x/1000000, 1), "M"),
           ifelse(x >= 1000,
                  paste0("$", round(x/1000, 1), "K"),
                  paste0("$", format(x, big.mark = ",", digits = 0))))
  },
  
  format_number = function(x) {
    format(x, big.mark = ",", digits = 0)
  },
  
  format_percentage = function(x) {
    paste0(round(x * 100, 1), "%")
  }
)

# Data mapping functions (simplified)
standardize_constituency <- function(raw_constituency) {
  case_when(
    raw_constituency %in% c("Alumni", "Resident/Fellow") ~ "UMMC Alumni",
    raw_constituency %in% c("Board Member", "Faculty/Staff", "Student", 
                           "University Of Mississippi Medical Center") ~ "UMMC Affiliate",
    raw_constituency %in% c("Corporation", "Estate / Trust", "Foundation", "Organization") ~ "Organization",
    raw_constituency %in% c("Friend", "Tribute") ~ "Individuals",
    TRUE ~ "Other"
  )
}

standardize_gift_type <- function(raw_gift_type) {
  case_when(
    raw_gift_type %in% c("Cash", "Recurring Gift Pay-Cash") ~ "Cash",
    raw_gift_type == "Stock/Property" ~ "Stock/Property",
    raw_gift_type == "Planned Gift" ~ "Planned Gift",
    raw_gift_type == "Pledge" ~ "Pledge",
    TRUE ~ "Other"
  )
}

create_gift_range <- function(amount) {
  cut(amount, 
      breaks = c(0, 100, 500, 1000, 2500, 5000, 10000, 20000, 40000, 75000, 150000, Inf),
      labels = config$gift_ranges,
      right = FALSE)
} 