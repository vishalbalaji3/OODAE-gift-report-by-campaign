# Configuration settings for the OODAE Gift Report application

config <- list(

  # ========== Shiny Theme CONFIGURATION ==========

  ui = list(
    theme = "readable"  # Specify your desired theme here
  ),

  # ========== DATA CONFIGURATION ==========
  # Constituency code hierarchy (determines primary constituency when a
  # constituent has multiple codes)
  # The order matters - first match in the list becomes the primary code
  constituency = list(
    hierarchy = c("UMMC Alumni", "UMMC Affilate", "Organization",
                  "Individuals", "Other"),
    # Mapping from raw values to standardized categories
    # For fct_recode: NEW_VALUE = OLD_VALUE
    mapping = list(
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
    )
  ),

  # Gift type mapping
  gift_type = list(
    # For fct_recode: NEW_VALUE = OLD_VALUE
    mapping = list(
      "Cash" = "Cash",
      "Cash" = "Recurring Gift Pay-Cash",
      "Stock/Property" = "Stock/Property",
      "Planned Gift" = "Planned Gift",
      "Pledge" = "Pledge"
    )
  ),

  # Gift and donor distribution ranges
  distribution_ranges = list(
    # Range labels - IMPORTANT: Order matters (highest to lowest)
    labels = c(
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
    ),
    # Range thresholds (must match the labels above)
    thresholds = c(150000, 75000, 40000, 20000, 10000, 5000, 2500, 1000,
                   500, 100, 0)
  ),

  # Date formatting
  date_format = list(
    input = "%m/%d/%Y",       # Format for reading dates from CSV
    # Format for displaying dates in the UI
    display = "%B %d, %Y at %I:%M %p"
  ),

  # ========== FORMATTING CONFIGURATION ==========

  # Data formatting
  format = list(
    # Currency formatting
    currency = list(
      # Whether to abbreviate millions (e.g., $1.2M)
      abbreviateMillions = TRUE,
      # Whether to abbreviate thousands (e.g., $1.2K)
      abbreviateThousands = TRUE,
      abbreviateThreshold = 1e6,    # Threshold for M abbreviation (1,000,000)
      thousandsThreshold = 1e3,     # Threshold for K abbreviation (1,000)
      millionsFormat = "$%.1fM",    # Format for millions values
      thousandsFormat = "$%.1fK",   # Format for thousands values
      standardFormat = "$%s",       # Format for standard values
      decimalPlaces = 0,            # Number of decimal places
      # Whether to use commas as thousands separators
      useThousandsSeparator = TRUE
    ),

    # Percentage formatting
    percentage = list(
      decimalPlaces = 1,           # Number of decimal places
      addPercentSign = TRUE        # Whether to add % sign
    ),

    # Numeric formatting
    numeric = list(
      # Whether to use commas as thousands separators
      useThousandsSeparator = TRUE,
      decimalPlaces = 0,             # Number of decimal places
      # Whether to abbreviate millions (e.g., 1.2M)
      abbreviateMillions = FALSE,
      # Whether to abbreviate thousands (e.g., 1.2K)
      abbreviateThousands = FALSE,
      abbreviateThreshold = 1e6,     # Threshold for M abbreviation (1,000,000)
      thousandsThreshold = 1e3,      # Threshold for K abbreviation (1,000)
      millionsFormat = "%.1fM",      # Format for millions values
      thousandsFormat = "%.1fK",     # Format for thousands values
      standardFormat = "%s"          # Format for standard values
    )
  )
)
