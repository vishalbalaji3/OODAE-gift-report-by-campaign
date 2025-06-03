# OODAE Gift Report Dashboard

A Shiny web application for analyzing gift data by campaign, with a simplified and consolidated architecture for easy maintenance and deployment.

## Project Structure

```
OODAE-gift-report-by-campaign/
├── app.R                                # Main Shiny application (consolidated UI/Server)
├── config.R                             # Configuration settings and mappings
├── helpers.R                            # Utility functions for formatting and validation
├── data_prep.R                          # Data preparation and analysis functions
├── data/                                # Data directory
│   ├── AllGifts.CSV                     # Main gift data (raw)
│   ├── FundList.CSV                     # Fund information (raw)
│   └── processed/                       # Processed data cache
│       ├── clean_data.rds               # Processed data (auto-generated)
│       ├── last_updated.txt             # Processing timestamp
│       └── data_prep.log                # Processing log
├── OODAE-gift-report-by-campaign.Rproj  # R Project file
├── .gitignore                           # Git ignore rules
├── rsconnect/                           # Deployment configuration
└── README.md                            # This file
```

## Architecture Overview

This application uses a **simplified, consolidated architecture** with all components in the root directory:

- **`config.R`**: Configuration settings, mappings, and file paths
- **`helpers.R`**: Utility functions for formatting, validation, and logging
- **`data_prep.R`**: Data loading, cleaning, processing, and analysis functions
- **`app.R`**: Complete Shiny application with UI and server logic

### Benefits of This Structure

1. **Simplicity**: All code in one location for easy navigation and maintenance
2. **Reusability**: Data preparation can be used for Quarto reports, PowerBI, or other tools
3. **Performance**: Data is processed once and cached, not on every dashboard load
4. **Maintainability**: Clear separation between data logic and presentation logic
5. **Deployment**: Single file structure simplifies deployment to ShinyApps.io
6. **Testing**: Each component can be tested and validated independently
7. **Scalability**: Easy to extend with new analysis functions or data sources

## Key Features

### Data Analysis Views
1. **Summary Statistics** - Gift type breakdown with year-over-year analysis
2. **Fund Split by Constituency** - Analysis by donor constituency groups
3. **Fund Analysis** - Performance by specific funds
4. **Unique Constituents** - Donor counts by year and constituency
5. **Average Gift Size** - Analysis by constituency and gift type
6. **Top Donors** - Top 50 donors by contribution amount
7. **Gift Size Distribution** - Distribution analysis by gift amount ranges
8. **Donor Levels** - Donor segmentation by total giving levels
9. **Full Data** - Complete filtered dataset view

### Advanced Features
- **Smart Caching**: Data is only reprocessed when source files change
- **Data Last Updated Display**: Shows actual file modification time (not processing time)
- **Dashboard Refresh Time**: Shows when the dashboard was last refreshed
- **Comprehensive Logging**: All data processing steps are logged with timestamps
- **Data Validation**: Automatic checks for required files and data integrity
- **Flexible Configuration**: Easy to modify mappings and settings without code changes
- **Export Functionality**: CSV downloads for all data tables with proper formatting
- **Dual Timeframe Support**: Switch between Fiscal Year and Calendar Year analysis

## Requirements

### R Packages
```r
# Core Shiny packages
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)

# Data manipulation
library(dplyr)
library(tidyr)
library(readr)
library(forcats)

# Export functionality
library(writexl)

# Optional: Visualization
library(plotly)
```

## Installation & Setup

### 1. Clone or Download Repository
```bash
git clone <repository-url>
cd OODAE-gift-report-by-campaign
```

### 2. Install Required Packages
```r
install.packages(c("shiny", "shinythemes", "shinycssloaders", 
                  "dplyr", "tidyr", "readr", "DT", "writexl", 
                  "plotly", "forcats"))
```

### 3. Prepare Data
Ensure your data files are in the `data/` directory:
- `data/AllGifts.CSV` - Main gift data
- `data/FundList.CSV` - Fund information

### 4. Run Data Preparation (Optional)
The data will be prepared automatically when the app starts, but you can run it manually:
```r
source("data_prep.R")
# This will process and cache the data
```

### 5. Launch Application
```r
shiny::runApp()
```

## Data Last Updated Logic

The dashboard displays two important timestamps:

1. **Data last updated**: The actual file modification time of `AllGifts.CSV`
   - This shows when your data file was last modified/uploaded
   - Uses `file.info(PATHS$raw_gifts)$mtime` to get the true file timestamp
   
2. **Dashboard refreshed**: When the Shiny application was last started
   - This shows when the dashboard was last reloaded/refreshed
   - Uses `Sys.time()` from when the server function starts

This provides transparency about both data freshness and dashboard state.

## Data Preparation Workflow

### Automatic Processing
1. **File Check**: Verifies that required data files exist
2. **Cache Check**: Determines if processed data is current
3. **Data Loading**: Loads raw CSV files with appropriate column types
4. **Data Cleaning**: Applies mappings, calculates derived fields, removes duplicates
5. **Data Integration**: Merges gifts data with fund information
6. **Caching**: Saves processed data as `.rds` for fast loading
7. **Logging**: Records all steps with timestamps

### Manual Data Refresh
```r
# Force refresh of all data
source("data_prep.R")
prepare_data(force_refresh = TRUE)

# Or run the script directly
Rscript data_prep.R
```

## Configuration

### Data Mappings (config.R)
- **Constituency Hierarchy**: Priority order for determining primary constituency
- **Constituency Mapping**: Maps raw codes to standardized categories
- **Gift Type Mapping**: Standardizes gift type classifications
- **Gift Ranges**: Predefined ranges for donor level analysis

### File Paths (config.R)
- Raw data file locations
- Processed data cache locations
- Log file location

### Processing Settings (config.R)
- Date formats
- Fiscal year start month (July = month 7)
- Data filtering rules

## Usage

### For Dashboard Users
1. **Launch Application**: `shiny::runApp()`
2. **Check Data Freshness**: Review the "Data last updated" timestamp in the header
3. **Apply Filters**: Use the filter panel to narrow down data
4. **Explore Tabs**: Navigate between different analysis views
5. **Download Data**: Use download buttons to export results
6. **Switch Timeframes**: Toggle between Fiscal Year and Calendar Year views

### For Data Analysts
```r
# Load processed data for external analysis
source("data_prep.R")
data <- prepare_data()

# Use analysis functions directly
source("helpers.R")
summary_stats <- calculate_summary_stats(data, timeframe = "fiscal")
fund_analysis <- calculate_fund_analysis(data, timeframe = "fiscal")
```

### For Report Developers
```r
# Use data preparation for Quarto/R Markdown reports
source("config.R")
source("helpers.R") 
source("data_prep.R")

# Get clean data for your reports
clean_data <- prepare_data()

# Use formatting functions
formatted_amount <- format_currency(1500000)  # Returns "$1.5M"
formatted_percent <- format_percent(23.7)     # Returns "23.7%"
```

## Data Requirements

### AllGifts.CSV
Expected columns:
- `Constituent ID`, `Gift Date`, `Gift Amount`, `Fund Split Amount`
- `Gift Type`, `Gift Subtype`, `Constituency Code`
- `Fund ID`, `Key Indicator`, `Gift Receipt Amount`, `Gift Pledge Balance`

### FundList.CSV  
Expected columns:
- `Fund ID`, `Fund Description`, `Fund Default Campaign ID`

## Troubleshooting

### Common Issues

1. **Data Files Not Found**
   - Verify files exist in `data/` directory
   - Check file names match exactly (case-sensitive)

2. **Processing Errors**
   - Check `data/processed/data_prep.log` for detailed error messages
   - Verify data format matches expected structure

3. **Performance Issues**
   - Large datasets may require more processing time initially
   - Subsequent loads will be faster due to caching

4. **Memory Issues**
   - For very large datasets, consider increasing R memory limits
   - Process data outside of Shiny if needed

5. **Data Last Updated Not Showing**
   - Verify `AllGifts.CSV` exists in the `data/` directory
   - Check file permissions if timestamp appears incorrect

### Debug Mode
```r
# Enable detailed logging
source("helpers.R")
log_message("Debug info here", "DEBUG")

# Check data file status
file_checks <- check_data_files()
print(file_checks)

# Check if processed data is current
is_current <- is_processed_data_current()
print(is_current)

# Check file modification time
file_info <- file.info("data/AllGifts.CSV")
print(file_info$mtime)
```

## Development & Extension

### Adding New Analysis Functions
1. Add function to `data_prep.R` in the analysis functions section
2. Update `app.R` to include new UI elements and server logic
3. Test the function independently before integrating

### Modifying Configuration
1. Update mappings and settings in `config.R`
2. Delete cached data to force refresh: `unlink("data/processed/clean_data.rds")`
3. Restart application to apply changes

### Using with Other Tools
```r
# For Quarto documents
source("data_prep.R")
data <- prepare_data()
# Use data in your Quarto analysis

# For PowerBI or Tableau
# Export processed data to CSV
write.csv(prepare_data(), "processed_data.csv", row.names = FALSE)
```

## Notes

- **Fiscal Year**: July 1 to June 30 (FY 2024 = July 1, 2023 to June 30, 2024)
- **Caching**: Processed data is automatically cached and updated when source files change
- **Logging**: All processing steps are logged to `data/processed/data_prep.log`
- **Performance**: Initial load may take time for large datasets, subsequent loads are fast
- **Data Timestamps**: Dashboard shows actual file modification time, not processing time

## Contributing

When making changes:
1. Test data preparation independently: `source("data_prep.R")`
2. Verify app functionality: `shiny::runApp()`
3. Update documentation as needed
4. Consider impact on other potential users of the data preparation pipeline 