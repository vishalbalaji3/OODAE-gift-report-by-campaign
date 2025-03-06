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

# ========== THEME CONFIGURATION ==========
# This section centralizes all style variables that will be used in component styles

# Theme variables - Colors and typography
config$theme <- list(
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
  
  # Spacing - using original exact values
  spacing = list(
    padding = "18px",
    margin = "22px",
    borderRadius = "6px"
  )
)

# ========== UI CONFIGURATION ==========
# Store the CSS strings, but referencing the central theme variables

# UI component styles
config$ui <- list(
  # Colors - maintained from original for backward compatibility
  colors = config$theme$colors,
  
  # Typography - maintained from original for backward compatibility
  typography = config$theme$typography,
  
  # Spacing - maintained from original for backward compatibility
  spacing = config$theme$spacing,
  
  # Button styling - with references to theme
  button = list(
    style = paste0(
      "background-color: ", config$theme$colors$primary, "; ",
      "color: white; ",
      "border-color: ", config$theme$colors$primary, "; ",
      "border-radius: ", config$theme$spacing$borderRadius, "; ",
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
  
  # Panel styling - with references to theme
  panel = list(
    style = paste0(
      "background-color: ", config$theme$colors$panel, "; ",
      "padding: ", config$theme$spacing$padding, "; ",
      "border-radius: ", config$theme$spacing$borderRadius, "; ", 
      "margin-bottom: ", config$theme$spacing$margin, "; ",
      "border: 1px solid ", config$theme$colors$border, "; ",
      "box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);"
    ),
    headerStyle = paste0(
      "color: ", config$theme$colors$header, "; ",
      "border-bottom: 1px solid ", config$theme$colors$border, "; ",
      "padding-bottom: 10px; ",
      "margin-bottom: 15px; ",
      "font-weight: ", config$theme$typography$headerFontWeight, ";"
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
      "background-color: ", config$theme$colors$primary, "; ",
      "color: white; ",
      "font-weight: 500;"
    ),
    rowStyle = paste0(
      "border-bottom: 1px solid ", config$theme$colors$tertiary, ";"
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
      "border-radius: ", config$theme$spacing$borderRadius, " ", config$theme$spacing$borderRadius, " 0 0; ",
      "margin-right: 4px; ",
      "padding: 10px 15px;"
    ),
    activeTabStyle = paste0(
      "background-color: white; ",
      "border-bottom: 2px solid ", config$theme$colors$primary, "; ",
      "font-weight: 600; ",
      "color: ", config$theme$colors$primary, ";"
    )
  )
)

# Generate the complete CSS for use in ui.R
config$completeCSS <- function() {
  theme <- config$theme
  
  paste0("
    /* Base styling */
    body {
      font-family: ", theme$typography$fontFamily, ";
      font-size: ", theme$typography$fontSize, ";
      line-height: ", theme$typography$lineHeight, ";
      background-color: white;
      color: ", theme$colors$text, ";
    }
    
    /* Header styling */
    h1, h2, h3, h4, h5, h6 {
      color: ", theme$colors$header, ";
      font-weight: ", theme$typography$headerFontWeight, ";
    }
    
    /* Link styling */
    a {
      color: ", theme$colors$link, ";
    }
    a:hover {
      color: ", theme$colors$secondary, ";
    }
    
    /* Panel styling */
    .panel {
      ", config$ui$panel$style, "
    }
    .panel-heading {
      ", config$ui$panel$headerStyle, "
    }
    
    /* Button styling - more subtle */
    .btn-default {
      background-color: white;
      color: ", theme$colors$text, ";
      border-color: ", theme$colors$border, ";
      border-radius: ", theme$spacing$borderRadius, ";
      font-weight: 500;
      padding: 6px 12px;
      transition: all 0.3s ease;
    }
    .btn-default:hover, .btn-default:focus, .btn-default:active {
      background-color: ", theme$colors$highlight, ";
      border-color: ", theme$colors$border, ";
      color: ", theme$colors$text, ";
    }
    
    /* Tab styling - more subtle */
    .nav-tabs {
      border-bottom: 1px solid ", theme$colors$border, ";
      margin-bottom: 20px;
    }
    .nav-tabs > li > a {
      background-color: ", theme$colors$panel, ";
      border: 1px solid ", theme$colors$border, ";
      border-bottom: none;
      border-radius: 4px 4px 0 0;
      margin-right: 4px;
      padding: 10px 15px;
      color: ", theme$colors$text, ";
    }
    .nav-tabs > li > a:hover, .nav-tabs > li > a:focus {
      background-color: ", theme$colors$highlight, " !important;
      border-color: ", theme$colors$border, ";
      color: ", theme$colors$text, " !important;
    }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
      background-color: white !important;
      border: 1px solid ", theme$colors$border, ";
      border-bottom: 2px solid ", theme$colors$secondary, ";
      font-weight: 600;
      color: ", theme$colors$text, " !important;
    }
    
    /* Table styling with !important to override theme */
    .dataTable thead th {
      background-color: ", theme$colors$panel, " !important;
      color: ", theme$colors$text, " !important;
      font-weight: 600 !important;
      border-bottom: 2px solid ", theme$colors$border, " !important;
    }
    .dataTable tbody tr {
      border-bottom: 1px solid ", theme$colors$border, " !important;
      background-color: white !important;
    }
    .dataTable.stripe tbody tr.odd {
      background-color: ", theme$colors$panel, " !important;
    }
    .dataTable.hover tbody tr:hover, .dataTable.hover tbody tr.odd:hover {
      background-color: ", theme$colors$highlight, " !important;
    }
    
    /* Info row styling (for summary sections) */
    tr.info, tr.info th, tr.info td {
      background-color: ", theme$colors$highlight, " !important;
      font-weight: bold !important;
    }
    
    /* Custom filter section */
    .filters-section {
      background-color: ", theme$colors$panel, ";
      border: 1px solid ", theme$colors$border, ";
      border-radius: ", theme$spacing$borderRadius, ";
      padding: ", theme$spacing$padding, ";
      margin-bottom: ", theme$spacing$margin, ";
    }
    
    /* Override any remaining bootstrap theme colors */
    .shiny-output-error { color: #d9534f; }
    .bg-primary { background-color: ", theme$colors$secondary, " !important; }
    .text-primary { color: ", theme$colors$secondary, " !important; }
  ")
}

# ========== FORMATTING CONFIGURATION ==========

# Data formatting
config$format <- list(
  # Currency formatting
  currency = list(
    abbreviateMillions = TRUE,    # Whether to abbreviate millions (e.g., $1.2M)
    abbreviateThousands = TRUE,   # Whether to abbreviate thousands (e.g., $1.2K)
    abbreviateThreshold = 1e6,    # Threshold for M abbreviation (1,000,000)
    thousandsThreshold = 1e3,     # Threshold for K abbreviation (1,000)
    millionsFormat = "$%.1fM",    # Format for millions values
    thousandsFormat = "$%.1fK",   # Format for thousands values
    standardFormat = "$%s",       # Format for standard values
    decimalPlaces = 0,            # Number of decimal places
    useThousandsSeparator = TRUE  # Whether to use commas as thousands separators
  ),
  
  # Percentage formatting
  percentage = list(
    decimalPlaces = 1,           # Number of decimal places
    addPercentSign = TRUE        # Whether to add % sign
  )
)