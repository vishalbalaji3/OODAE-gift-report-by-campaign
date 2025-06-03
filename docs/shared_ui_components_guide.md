# Shared UI Components Guide

This guide explains how to use the shared UI components in the R Shiny application to maintain consistent styling and reduce code duplication.

## Overview

All shared UI components are defined in `R/ui_components.R` and styled with `www/shared_components.css`. These components provide a consistent look and feel across all modules while making the code more maintainable.

## Available Components

### 1. Filter Panel Component

**Function:** `create_filter_panel(id_prefix, full_data = NULL)`

Creates a standardized filter panel with campaign, gift type, time period, and year selection.

```r
# Usage in UI
create_filter_panel("dataTabFilter")
```

**Parameters:**
- `id_prefix`: Unique prefix for input IDs
- `full_data`: Optional data source (defaults to FullData)

### 2. Card Panel Component

**Function:** `create_card_panel(title, content, status = "default", collapsible = FALSE, collapsed = FALSE)`

Creates a Bootstrap panel with consistent styling and optional collapsible functionality.

```r
# Basic usage
create_card_panel(
  title = "Summary Statistics",
  content = uiOutput("summary"),
  status = "info"
)

# Collapsible panel
create_card_panel(
  title = "Advanced Options",
  content = advancedOptionsUI(),
  status = "warning",
  collapsible = TRUE,
  collapsed = TRUE
)
```

**Parameters:**
- `title`: Panel title
- `content`: Panel body content
- `status`: Panel color theme ("default", "primary", "success", "info", "warning", "danger")
- `collapsible`: Whether panel can be collapsed
- `collapsed`: Initial collapsed state

### 3. Info Box Component

**Function:** `create_info_box(title, value, icon = NULL, color = "primary", width = 3)`

Creates an information display box with optional icon.

```r
# Basic info box
create_info_box(
  title = "Total Gifts",
  value = "1,234",
  color = "primary",
  icon = "gift"
)
```

**Parameters:**
- `title`: Box title/label
- `value`: Main value to display
- `icon`: Font Awesome icon name (optional)
- `color`: Color theme
- `width`: Bootstrap column width (1-12)

### 4. Stats Row Component

**Function:** `create_stats_row(stats_list)`

Creates a row of statistics boxes from a list.

```r
# Usage
stats <- list(
  list(title = "Total Gifts", value = "1,234", color = "primary", icon = "gift"),
  list(title = "Total Amount", value = "$456K", color = "success", icon = "dollar-sign"),
  list(title = "Avg Gift", value = "$123", color = "info", icon = "calculator")
)

create_stats_row(stats)
```

### 5. Enhanced Download Buttons

**Function:** `create_download_buttons(ns, additional_buttons = NULL)`

Creates standardized download buttons with optional additional buttons.

```r
# Basic usage
create_download_buttons(ns)

# With additional buttons
create_download_buttons(
  ns,
  additional_buttons = actionButton(ns("print"), "Print", class = "btn btn-info btn-sm")
)
```

### 6. Enhanced Data Table

**Function:** `create_datatable(data, currency_cols = NULL, scroll_x = TRUE, page_length = 25, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'))`

Creates a feature-rich data table with consistent styling.

```r
# Basic usage
create_datatable(data)

# With currency formatting
currency_cols <- which(sapply(data, is.numeric)) - 1
create_datatable(data, currency_cols = currency_cols)
```

### 7. Loading Wrapper

**Function:** `create_loading_wrapper(output_element, text = "Loading...", type = "dots")`

Wraps UI elements with loading spinners.

```r
# Usage
create_loading_wrapper(
  DTOutput(ns("table")),
  text = "Loading data...",
  type = "pulse"
)
```

**Spinner types:** "dots", "circle", "bounce", "pulse"

### 8. Table Wrapper

**Function:** `create_table_wrapper(table_output, title = NULL, subtitle = NULL)`

Wraps tables with optional titles and consistent styling.

```r
# Usage
create_table_wrapper(
  DTOutput(ns("table")),
  title = "Donor Analysis",
  subtitle = "Filtered results for selected criteria"
)
```

### 9. Alert Component

**Function:** `create_alert(message, type = "info", dismissible = TRUE)`

Creates Bootstrap alerts for messages and notifications.

```r
# Usage
create_alert("No data available for the selected filters.", type = "warning")
create_alert("Data loaded successfully!", type = "success", dismissible = FALSE)
```

### 10. Progress Bar Component

**Function:** `create_progress_bar(value, max_value = 100, label = NULL, color = "primary")`

Creates animated progress bars.

```r
# Usage
create_progress_bar(
  value = 75,
  max_value = 100,
  label = "Processing...",
  color = "success"
)
```

### 11. Module UI Template

**Function:** `create_module_ui(title, summary_output, table_output, download_ns, additional_content = NULL, show_summary = TRUE)`

Creates a standardized module UI structure.

```r
# Usage in module UI function
moduleNameUI <- function(id) {
  ns <- NS(id)
  
  create_module_ui(
    title = "Module Title",
    summary_output = uiOutput(ns("summary")),
    table_output = DTOutput(ns("table")),
    download_ns = ns,
    additional_content = list(
      # Optional additional UI elements
    )
  )
}
```

## Helper Functions

### Formatting Functions

```r
# Format numbers with abbreviations
format_number(1234567, abbreviate = TRUE)  # Returns "1.2M"

# Format currency
format_currency(1234567)  # Returns "$1.2M"

# Get time period label
get_time_period_label("fiscal")  # Returns "Fiscal Year"
```

### Responsive Utilities

```r
# Create responsive columns
create_responsive_column(
  content = "Column content",
  xs = 12,  # Full width on extra small screens
  md = 6    # Half width on medium screens and up
)
```

### Download Handlers

```r
# Create download handlers in server function
downloads <- create_download_handlers("filename_base", data_reactive, session)
output$download_csv <- downloads$csv
output$download_excel <- downloads$excel
```

## Best Practices

### 1. Consistent Color Usage

Use these standard colors for consistency:
- **Primary (blue):** Main actions, titles
- **Success (green):** Positive values, success messages
- **Info (light blue):** Information, neutral content
- **Warning (orange):** Warnings, attention items
- **Danger (red):** Errors, negative values

### 2. Module Structure

Use the standardized module structure:

```r
# UI Function
moduleNameUI <- function(id) {
  ns <- NS(id)
  
  create_module_ui(
    title = "Module Title",
    summary_output = uiOutput(ns("summary")),
    table_output = DTOutput(ns("table")),
    download_ns = ns
  )
}

# Server Function
moduleNameServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive data processing
    processed_data <- reactive({
      # Process data
    })
    
    # Summary output
    output$summary <- renderUI({
      if (length(fiscal_years()) == 0) {
        return(create_alert("No data available.", type = "warning"))
      }
      # Create summary
    })
    
    # Table output
    output$table <- renderDT({
      create_datatable(processed_data())
    })
    
    # Download handlers
    downloads <- create_download_handlers("module_data", processed_data, session)
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}
```

### 3. Error Handling

Use alerts for user-friendly error messages:

```r
# Instead of HTML messages
return(create_alert("No data available for the selected filters.", type = "warning"))

# For success messages
create_alert("Data exported successfully!", type = "success", dismissible = TRUE)
```

### 4. Responsive Design

The components are designed to be mobile-friendly:
- Info boxes stack on small screens
- Download buttons become full-width on mobile
- Tables are horizontally scrollable

## Migration Guide

To migrate existing modules to use shared components:

### Before (Old Style)
```r
tagList(
  fluidRow(
    column(12,
      div(class = "panel panel-default",
        div(class = "panel-heading", h4("Title")),
        div(class = "panel-body", uiOutput(ns("summary")))
      )
    )
  ),
  div(class = "table-responsive",
    withSpinner(DTOutput(ns("table")))
  ),
  fluidRow(
    column(12,
      div(class = "text-right",
        downloadButton(ns("download_csv"), "CSV"),
        downloadButton(ns("download_excel"), "Excel")
      )
    )
  )
)
```

### After (Using Shared Components)
```r
create_module_ui(
  title = "Title",
  summary_output = uiOutput(ns("summary")),
  table_output = DTOutput(ns("table")),
  download_ns = ns
)
```

## Styling Customization

The CSS file `www/shared_components.css` provides extensive styling. Key features:
- Hover effects on cards and info boxes
- Smooth transitions and animations
- Print-friendly styles
- Mobile responsiveness
- Accessibility enhancements

To customize colors or styles, modify the CSS file while maintaining the class structure.

## Troubleshooting

### Common Issues

1. **CSS not loading:** Ensure `includeCSS("www/shared_components.css")` is in your UI
2. **Components not rendering:** Check that `R/ui_components.R` is sourced in `global.R`
3. **Download buttons not working:** Ensure download handlers are properly assigned in server

### Dependencies

The shared components require these packages:
- shiny
- DT
- shinycssloaders
- writexl (for Excel downloads)

All dependencies should already be installed as part of the main application. 