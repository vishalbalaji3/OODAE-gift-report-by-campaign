# Visualization utilities

# Common UI elements and helper functions for visualizations

# Time period selector UI element
timePeriodSelectorUI <- function(id) {
  ns <- NS(id)
  
  radioButtons(ns("timeframe"), "Select Time Period:",
               choices = c("Fiscal Year (Jul-Jun)" = "fiscal", 
                           "Calendar Year (Jan-Dec)" = "calendar"),
               selected = "fiscal",
               inline = TRUE)
}

# Helper to format visualization dates according to time period selection
process_date_by_timeframe <- function(data, timeframe = "fiscal") {
  if(timeframe == "calendar") {
    # Create calendar year grouping
    data %>% mutate(Year_Group = format(as.Date(`Gift Date`), "%Y"))
  } else {
    # Use existing fiscal year
    data %>% mutate(Year_Group = `Fiscal Year`)
  }
}

# Common plotly theme to ensure consistent look across visualizations
apply_viz_theme <- function(p) {
  p %>%
    layout(
      font = list(family = "Arial", size = 12),
      legend = list(orientation = "h", y = -0.2),
      margin = list(l = 50, r = 20, t = 50, b = 50),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12, family = "Arial"),
        bordercolor = "darkgray"
      )
    )
}

# Create standardized visualization download handlers
create_viz_download_handlers <- function(id, plot_output, data_reactive, session) {
  output_name <- function(ext) paste0(id, "_", Sys.Date(), ext)
  
  list(
    png = downloadHandler(
      filename = function() output_name(".png"),
      content = function(file) {
        # Export plotly to PNG
        p <- plotly_build(plot_output())
        export(p, file = file)
      }
    ),
    csv = downloadHandler(
      filename = function() output_name(".csv"),
      content = function(file) write.csv(data_reactive(), file, row.names = FALSE)
    )
  )
}