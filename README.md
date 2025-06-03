# OODAE Gift Report Dashboard

A Shiny web application for analyzing gift data by campaign, providing comprehensive reporting and visualization capabilities.

## Project Structure

```
OODAE-gift-report-by-campaign/
├── app.R                                 # Main Shiny application (consolidated)
├── data/                                # Data directory
│   ├── AllGifts.CSV                     # Main gift data
│   ├── FundList.CSV                     # Fund information
│   └── TEMP_GIF.CSV                     # Additional gift data
├── OODAE-gift-report-by-campaign.Rproj # R Project file
├── .gitignore                           # Git ignore rules
├── rsconnect/                           # Deployment configuration
└── README.md                            # This file
```

## Overview

This simplified Shiny application consolidates all functionality into a single `app.R` file for better maintainability and deployment. The application provides:

- **Data Tables Tab**: Multiple analysis views including summary statistics, fund splits, constituent analysis, and more
- **Filter System**: Dynamic filtering by campaign, gift type, year, and timeframe (fiscal vs calendar)
- **Export Functionality**: CSV downloads for all data tables
- **Responsive Design**: Clean, modern UI with Bootstrap theming

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

### Filter Options
- **Campaign ID**: Select specific campaigns
- **Gift Type**: Filter by Cash, Stock/Property, Planned Gift, or Pledge
- **Time Period**: Choose between Fiscal Year or Calendar Year views
- **Year Selection**: Multi-select specific years for analysis

## Requirements

### R Packages
```r
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(writexl)
library(plotly)
library(forcats)
```

## Installation & Setup

1. **Clone or download this repository**
2. **Ensure data files are in the `data/` directory**:
   - `AllGifts.CSV` - Main gift data
   - `FundList.CSV` - Fund information
3. **Install required R packages**:
   ```r
   install.packages(c("shiny", "shinythemes", "shinycssloaders", 
                     "dplyr", "tidyr", "readr", "DT", "writexl", 
                     "plotly", "forcats"))
   ```
4. **Run the application**:
   ```r
   shiny::runApp()
   ```

## Data Requirements

### AllGifts.CSV
Expected columns include:
- `Constituent ID`, `Gift Date`, `Gift Amount`, `Fund Split Amount`
- `Gift Type`, `Gift Subtype`, `Constituency Code`
- `Fund ID`, `Key Indicator`

### FundList.CSV  
Expected columns include:
- `Fund ID`, `Fund Description`, `Fund Default Campaign ID`

## Configuration

The application includes built-in configuration for:
- **Constituency Hierarchy**: UMMC Alumni, UMMC Affiliate, Organization, Individuals, Other
- **Gift Type Mapping**: Standardized gift type categories
- **Gift Amount Ranges**: Predefined ranges for analysis and reporting

## Usage

1. **Start the application** using `shiny::runApp()`
2. **Select filters** at the top of the Data Tables tab
3. **Navigate between tabs** to view different analysis perspectives
4. **Download data** using the download buttons on each tab
5. **Adjust timeframe** between Fiscal Year and Calendar Year as needed

## Notes

- **Fiscal Year**: Runs from July 1 to June 30 (e.g., FY 2024 = July 1, 2023 to June 30, 2024)
- **Data Updates**: The application shows the last modified date of the AllGifts.CSV file
- **Performance**: Large datasets may take a moment to process when filters change
- **Export Format**: All downloads are in CSV format with formatted currency values

## Development

This is a consolidated, single-file Shiny application designed for:
- **Easy deployment** to RStudio Connect or Shiny Server
- **Simple maintenance** with all code in one location  
- **Clear structure** with well-organized functions and documentation

For modifications, all functionality is contained within `app.R` including:
- Configuration settings
- Data loading and processing functions
- UI components and server logic
- Download handlers and reactive functions 