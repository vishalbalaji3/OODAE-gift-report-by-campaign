# Donor Retention Visualization Module

# UI Function
donorRetentionUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading",
                     h4("Donor Retention by Giving Level")
                 ),
                 div(class = "panel-body",
                     # Summary metrics above the visualization
                     uiOutput(ns("retentionSummary")),

                     # The main visualization output
                     plotlyOutput(ns("retentionPlot"), height = "500px")
                 )
             )
      )
    )
  )
}

# Server Function
donorRetentionServer <- function(id, filtered_data, fiscal_years, summary_stats, time_period = reactive("fiscal")) {
  moduleServer(id, function(input, output, session) {

    # Process data for retention analysis
    processed_data <- reactive({
      req(filtered_data())
      req(length(fiscal_years()) >= 2) # Need at least 2 years for retention analysis

      # Get the filtered data
      data <- filtered_data()

      # Explicitly ensure we have proper year field based on selected time period
      if (time_period() == "calendar") {
        # Add Calendar_Year column if it doesn't exist
        if (!"Calendar_Year" %in% names(data)) {
          data <- data %>%
            mutate(Calendar_Year = format(as.Date(`Gift Date`), "%Y"))
        }
        year_field <- "Calendar_Year"
      } else {
        year_field <- "Fiscal Year"
      }

      # Get the sorted selected years
      selected_years <- fiscal_years()

      # Check if we have at least 2 years and data
      if(nrow(data) == 0 || length(selected_years) < 2) {
        return(data.frame(
          Donor_Level = character(),
          Year_Pair = character(),
          Total_Donors = integer(),
          Retained_Donors = integer(),
          Retention_Rate = numeric(),
          stringsAsFactors = FALSE
        ))
      }

      # Check if years are consecutive
      selected_years_numeric <- as.numeric(selected_years)
      is_consecutive <- all(diff(selected_years_numeric) == 1)

      # If years are not consecutive, we'll display a warning but still calculate retention
      # between the earliest and latest selected years for simplicity

      # Get range labels and thresholds from config
      range_labels <- config$distribution_ranges$labels
      range_thresholds <- config$distribution_ranges$thresholds

      # Create a function that assigns a label based on amount
      assign_range_label <- function(amount) {
        for (i in seq_along(range_thresholds)) {
          if (amount >= range_thresholds[i]) {
            return(range_labels[i])
          }
        }
        return(tail(range_labels, 1)) # Return the last label as default
      }

      # Create a list to store retention data
      retention_data_list <- list()

      # Determine which years to use
      if (is_consecutive) {
        # For consecutive years, use all consecutive pairs
        # Get all years in the filtered data
        all_years <- sort(unique(data[[year_field]]))
        # Filter to only include the selected years
        years_to_use <- all_years[all_years %in% selected_years]
        # Process each consecutive pair
        for (i in 1:(length(years_to_use)-1)) {
          year1 <- years_to_use[i]
          year2 <- years_to_use[i+1]

          # Get donors for year1 with explicit use of year_field
          donors_year1 <- data %>%
            filter(!!sym(year_field) == year1) %>%
            group_by(`Constituent ID`) %>%
            summarise(Total_Giving = sum(as.numeric(`Fund Split Amount`), na.rm = TRUE),
                      .groups = 'drop') %>%
            mutate(Donor_Level = sapply(Total_Giving, assign_range_label),
                   Donor_Level = factor(Donor_Level, levels = range_labels))

          # Get donors for year2 with explicit use of year_field
          donors_year2 <- data %>%
            filter(!!sym(year_field) == year2) %>%
            select(`Constituent ID`) %>%
            distinct()

          # Calculate retention
          retained <- donors_year1 %>%
            inner_join(donors_year2, by = "Constituent ID") %>%
            group_by(Donor_Level) %>%
            summarise(Retained_Donors = n(), .groups = 'drop')

          # Count total donors by level in year1
          total_by_level <- donors_year1 %>%
            group_by(Donor_Level) %>%
            summarise(Total_Donors = n(), .groups = 'drop')

          # Determine label format based on time period
          period_prefix <- ifelse(time_period() == "calendar", "CY ", "FY ")

          # Combine and calculate retention rate with properly formatted year labels
          combined <- total_by_level %>%
            left_join(retained, by = "Donor_Level") %>%
            mutate(Retained_Donors = replace_na(Retained_Donors, 0),
                   Retention_Rate = Retained_Donors / Total_Donors * 100,
                   Year_Pair = paste0(period_prefix, year1, " to ", period_prefix, year2))

          retention_data_list[[length(retention_data_list) + 1]] <- combined
        }
      } else {
        # For non-consecutive years, calculate retention from earliest to latest selected year
        first_year <- min(selected_years)
        last_year <- max(selected_years)

        # Get donors for first year
        donors_first_year <- data %>%
          filter(!!sym(year_field) == first_year) %>%
          group_by(`Constituent ID`) %>%
          summarise(Total_Giving = sum(as.numeric(`Fund Split Amount`), na.rm = TRUE),
                    .groups = 'drop') %>%
          mutate(Donor_Level = sapply(Total_Giving, assign_range_label),
                 Donor_Level = factor(Donor_Level, levels = range_labels))

        # Get donors for last year
        donors_last_year <- data %>%
          filter(!!sym(year_field) == last_year) %>%
          select(`Constituent ID`) %>%
          distinct()

        # Calculate retention
        retained <- donors_first_year %>%
          inner_join(donors_last_year, by = "Constituent ID") %>%
          group_by(Donor_Level) %>%
          summarise(Retained_Donors = n(), .groups = 'drop')

        # Count total donors by level in first year
        total_by_level <- donors_first_year %>%
          group_by(Donor_Level) %>%
          summarise(Total_Donors = n(), .groups = 'drop')

        # Determine label format based on time period
        period_prefix <- ifelse(time_period() == "calendar", "CY ", "FY ")

        # Combine and calculate retention rate with properly formatted year labels
        combined <- total_by_level %>%
          left_join(retained, by = "Donor_Level") %>%
          mutate(Retained_Donors = replace_na(Retained_Donors, 0),
                 Retention_Rate = Retained_Donors / Total_Donors * 100,
                 Year_Pair = paste0(period_prefix, first_year, " to ", period_prefix, last_year))

        retention_data_list[[length(retention_data_list) + 1]] <- combined
      }

      # Combine all retention data
      retention_data <- bind_rows(retention_data_list)

      # Make sure all donor levels are represented
      levels_template <- expand.grid(
        Donor_Level = factor(range_labels, levels = range_labels),
        Year_Pair = unique(retention_data$Year_Pair),
        stringsAsFactors = FALSE
      )

      retention_data <- levels_template %>%
        left_join(retention_data, by = c("Donor_Level", "Year_Pair")) %>%
        mutate(
          Total_Donors = replace_na(Total_Donors, 0),
          Retained_Donors = replace_na(Retained_Donors, 0),
          Retention_Rate = ifelse(Total_Donors > 0, Retained_Donors / Total_Donors * 100, 0)
        )

      return(retention_data)
    })

    # Calculate overall retention metrics
    retention_metrics <- reactive({
      req(processed_data())
      data <- processed_data()

      if(nrow(data) == 0) {
        return(list(
          overall_retention = 0,
          highest_level = NA,
          highest_rate = 0,
          lowest_level = NA,
          lowest_rate = 100
        ))
      }

      # If we're using aggregated data for the plot, we should use the same for metrics
      # First, check if we have more than one year pair
      if (length(unique(data$Year_Pair)) > 1) {
        # Aggregate the data the same way we do for the plot
        aggregated_data <- data %>%
          group_by(Donor_Level) %>%
          summarise(
            Total_Donors = sum(Total_Donors, na.rm = TRUE),
            Retained_Donors = sum(Retained_Donors, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            Retention_Rate = ifelse(Total_Donors > 0, Retained_Donors / Total_Donors * 100, 0)
          )

        # Use this aggregated data for metrics
        metrics_data <- aggregated_data
      } else {
        # Just use the original data if only one year pair
        metrics_data <- data
      }

      # Calculate overall retention rate
      overall_donors <- sum(metrics_data$Total_Donors, na.rm = TRUE)
      overall_retained <- sum(metrics_data$Retained_Donors, na.rm = TRUE)
      overall_retention <- if(overall_donors > 0) overall_retained / overall_donors * 100 else 0

      # Find highest and lowest retention rates by level (filtering out levels with few donors)
      meaningful_data <- metrics_data %>% filter(Total_Donors >= 5)

      if(nrow(meaningful_data) > 0) {
        highest_idx <- which.max(meaningful_data$Retention_Rate)
        lowest_idx <- which.min(meaningful_data$Retention_Rate)

        highest_level <- meaningful_data$Donor_Level[highest_idx]
        highest_rate <- meaningful_data$Retention_Rate[highest_idx]
        lowest_level <- meaningful_data$Donor_Level[lowest_idx]
        lowest_rate <- meaningful_data$Retention_Rate[lowest_idx]
      } else {
        highest_level <- NA
        highest_rate <- 0
        lowest_level <- NA
        lowest_rate <- 0
      }

      list(
        overall_retention = overall_retention,
        highest_level = highest_level,
        highest_rate = highest_rate,
        lowest_level = lowest_level,
        lowest_rate = lowest_rate
      )
    })

    # Retention Summary
    output$retentionSummary <- renderUI({
      data <- processed_data()
      metrics <- retention_metrics()
      years <- fiscal_years()

      if(nrow(data) == 0) {
        time_type <- ifelse(time_period() == "calendar", "calendar", "fiscal")
        return(HTML(paste0("<p>No data available for the selected filters. Retention analysis requires at least two consecutive ",
                           time_type, " years of data.</p>")))
      }

      # Check if years selected are consecutive
      years_numeric <- as.numeric(years)
      is_consecutive <- all(diff(years_numeric) == 1)

      # Create HTML with tooltips
      html <- paste0(
        '<div class="row" style="margin-bottom: 20px;">',

        '<div class="col-md-4">',
        '<div class="panel panel-info" data-toggle="tooltip" title="Average percentage of donors who gave in consecutive years">',
        '<div class="panel-heading text-center"><strong>Overall Retention Rate</strong></div>',
        '<div class="panel-body text-center"><h3>', sprintf("%.1f%%", metrics$overall_retention), '</h3></div>',
        '</div></div>',

        '<div class="col-md-4">',
        '<div class="panel panel-success" data-toggle="tooltip" title="Donor level with the highest retention rate">',
        '<div class="panel-heading text-center"><strong>Highest Retention</strong></div>',
        '<div class="panel-body text-center"><h4>', metrics$highest_level, '</h4><h4>', sprintf("%.1f%%", metrics$highest_rate), '</h4></div>',
        '</div></div>',

        '<div class="col-md-4">',
        '<div class="panel panel-warning" data-toggle="tooltip" title="Donor level with the lowest retention rate">',
        '<div class="panel-heading text-center"><strong>Lowest Retention</strong></div>',
        '<div class="panel-body text-center"><h4>', metrics$lowest_level, '</h4><h4>', sprintf("%.1f%%", metrics$lowest_rate), '</h4></div>',
        '</div></div>',

        '</div>'
      )

      # Add note about consecutive years if needed
      if (!is_consecutive && length(years) > 1) {
        html <- paste0(
          html,
          '<div class="alert alert-info">',
          '<strong>Note:</strong> For non-consecutive years (like ', years[1], ' and ', years[length(years)], '), ',
          'retention is calculated between the earliest and latest selected years. ',
          'This shows donors who gave in both the first and last years selected, regardless of giving patterns in between.',
          '</div>'
        )
      }

      # Initialize tooltips via JavaScript
      html <- paste0(
        html,
        '<script>$(function () { $("[data-toggle=\'tooltip\']").tooltip(); });</script>'
      )

      HTML(html)
    })

    # Retention Plot
    output$retentionPlot <- renderPlotly({
      data <- processed_data()

      if(nrow(data) == 0) {
        # Return an empty plot with a message
        return(plot_ly() %>%
                 layout(title = "No data available for the selected filters. Retention analysis requires at least two consecutive years of data."))
      }

      # Create a color gradient for retention rates
      color_palette <- colorRampPalette(c("#67a9cf", "#1c9099", "#016c59"))(100)

      # Get the current time period
      current_time_period <- time_period()

      # Get current years
      years <- fiscal_years()

      # Time period label
      time_label <- ifelse(current_time_period == "fiscal",
                           "Fiscal Year",
                           "Calendar Year")

      # Create a descriptive title based on years and time period
      period_prefix <- ifelse(current_time_period == "calendar", "CY ", "FY ")

      if (length(years) <= 2) {
        plot_title <- "Donor Retention by Giving Level"
      } else {
        plot_title <- paste("Donor Retention by Giving Level -",
                            paste0(period_prefix, years[1]), "to",
                            paste0(period_prefix, tail(years, 1)))
      }

      # Aggregate data if multiple year pairs exist
      if (length(unique(data$Year_Pair)) > 1) {
        plot_data <- data %>%
          group_by(Donor_Level) %>%
          summarise(
            Total_Donors = sum(Total_Donors, na.rm = TRUE),
            Retained_Donors = sum(Retained_Donors, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(
            Retention_Rate = ifelse(Total_Donors > 0, Retained_Donors / Total_Donors * 100, 0),
            Year_Pair = "All Years"
          )
      } else {
        plot_data <- data
      }

      # Filter out levels with very few donors to avoid misleading rates
      plot_data <- plot_data %>%
        filter(Total_Donors >= 5)

      # Order the data by donor level
      plot_data$Donor_Level <- factor(plot_data$Donor_Level, levels = config$distribution_ranges$labels)
      plot_data <- plot_data %>% arrange(Donor_Level)

      # Create hover text
      hover_text <- paste0(
        "Giving Level: ", plot_data$Donor_Level,
        "<br>Retention Rate: ", sprintf("%.1f%%", plot_data$Retention_Rate),
        "<br>Donors: ", format(plot_data$Total_Donors, big.mark = ","),
        "<br>Retained: ", format(plot_data$Retained_Donors, big.mark = ",")
      )

      # Create the bar chart - explicitly disable text display
      p <- plot_ly(plot_data,
                   x = ~Donor_Level,
                   y = ~Retention_Rate,
                   type = 'bar',
                   marker = list(
                     color = ~Retention_Rate,
                     colorscale = list(c(0, 1), c('#67a9cf', '#016c59')),
                     showscale = TRUE,
                     colorbar = list(title = "Retention %")
                   ),
                   hoverinfo = 'text',
                   text = hover_text,
                   textposition = 'none')

      # Format the layout without bar labels
      p %>% layout(
        title = plot_title,
        xaxis = list(
          title = "Donor Giving Level",
          tickangle = -45
        ),
        yaxis = list(
          title = "Retention Rate (%)",
          range = c(0, max(100, max(plot_data$Retention_Rate) * 1.1)) # Make sure y-axis goes to at least 100%
        ),
        showlegend = FALSE,
        margin = list(b = 120) # Add margin at bottom for long x-axis labels
      )
    })
  })
}
