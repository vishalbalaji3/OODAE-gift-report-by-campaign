# Configuration settings for the OODAE Gift Report application

# ========== DATA CONFIGURATION ==========

# Constituency code hierarchy (determines primary constituency when a constituent has multiple codes)
# The order matters - first match in the list becomes the primary code
config <- list(
  constituency = list(
    hierarchy = c("UMMC Alumni", "UMMC Affilate", "Organization", "Individuals", "Other"),
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
    thresholds = c(150000, 75000, 40000, 20000, 10000, 5000, 2500, 1000, 500, 100, 0)
  ),
  
  # Date formatting
  date_format = list(
    input = "%m/%d/%Y",       # Format for reading dates from CSV
    display = "%B %d, %Y at %I:%M %p"  # Format for displaying dates in the UI
  )
)

# ========== UI CONFIGURATION ==========

# UMMC Medical/Clinical Theme (Subtle Version)
config$ui <- list(
  # Colors
  colors = list(
    primary = "#00529B",      # UMMC Blue (used minimally)
    secondary = "#0082C0",    # Lighter blue for secondary elements
    tertiary = "#7BAFD4",     # Pale blue for subtle accents
    background = "#FFFFFF",   # White background
    panel = "#F8F8F8",        # Very light gray panel background
    border = "#DFE3E8",       # Light gray border
    header = "#333333",       # Dark gray for headers (not blue)
    text = "#444444",         # Dark gray for text
    link = "#0082C0",         # Light blue for links
    info = "#5BC0DE",         # Info blue
    success = "#00AA8D",      # Medical teal for success
    warning = "#FFC107",      # Warning amber
    danger = "#D9534F",       # Medical red for danger/alerts
    highlight = "#F5F7F9"     # Very light gray for highlights/hover states
  ),
  
  # Typography
  typography = list(
    fontFamily = "'Segoe UI', 'Helvetica Neue', Arial, sans-serif",
    fontSize = "14px",
    headerFontWeight = "600",
    lineHeight = "1.5"
  ),
  
  # Spacing
  spacing = list(
    padding = "18px",
    margin = "22px",
    borderRadius = "6px"
  ),
  
  # Button styling
  button = list(
    style = paste0(
      "background-color: #00529B; ",
      "color: white; ",
      "border-color: #00529B; ",
      "border-radius: 6px; ",
      "font-weight: 500; ",
      "padding: 8px 16px; ",
      "transition: all 0.3s ease;"
    ),
    hoverStyle = paste0(
      "background-color: #003F77; ",
      "border-color: #003F77; ",
      "box-shadow: 0 2px 5px rgba(0, 82, 155, 0.3);"
    )
  ),
  
  # Panel styling (more subtle)
  panel = list(
    style = paste0(
      "background-color: #F8F8F8; ",
      "padding: 18px; ",
      "border-radius: 6px; ", 
      "margin-bottom: 22px; ",
      "border: 1px solid #DFE3E8; ",
      "box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);"
    ),
    headerStyle = paste0(
      "color: #444444; ",
      "border-bottom: 1px solid #DFE3E8; ",
      "padding-bottom: 10px; ",
      "margin-bottom: 15px; ",
      "font-weight: 600;"
    )
  ),
  
  # Table styling
  table = list(
    pageLength = 10,
    scrollX = TRUE,
    autoWidth = FALSE,
    dom = 'lfrtip',
    class = 'cell-border stripe hover',
    headerStyle = paste0(
      "background-color: #00529B; ",
      "color: white; ",
      "font-weight: 500;"
    ),
    rowStyle = paste0(
      "border-bottom: 1px solid #E1EDF7;"
    ),
    alternateRowStyle = paste0(
      "background-color: #F8FCFF;"
    ),
    hoverStyle = paste0(
      "background-color: #E9F2FB !important;"
    )
  ),
  
  # Tab styling
  tabs = list(
    style = paste0(
      "border-bottom: 2px solid #D0E4F5; ",
      "margin-bottom: 20px;"
    ),
    tabStyle = paste0(
      "background-color: #F0F7FC; ",
      "border: 1px solid #D0E4F5; ",
      "border-bottom: none; ", 
      "border-radius: 6px 6px 0 0; ",
      "margin-right: 4px; ",
      "padding: 10px 15px;"
    ),
    activeTabStyle = paste0(
      "background-color: white; ",
      "border-bottom: 2px solid #00529B; ",
      "font-weight: 600; ",
      "color: #00529B;"
    )
  )
)

# ========== FORMATTING CONFIGURATION ==========

# Data formatting
config$format <- list(
  # Currency formatting
  currency = list(
    abbreviateThreshold = 1e6,  # When to use abbreviations (e.g., $1.2M)
    abbreviateFormat = "$%.1fM", # Format for abbreviated values
    standardFormat = "$%s",      # Format for standard values
    decimalPlaces = 0,           # Number of decimal places
    useThousandsSeparator = TRUE # Whether to use commas as thousands separators
  ),
  
  # Percentage formatting
  percentage = list(
    decimalPlaces = 1,           # Number of decimal places
    addPercentSign = TRUE        # Whether to add % sign
  )
)