# Donor Pyramid Visualization Module

# UI Function
donorPyramidUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading",
                     h4("Donor Pyramid")
                 ),
                 div(class = "panel-body",
                     # Summary metrics above the visualization
                     uiOutput(ns("pyramidSummary")),

                     # The main visualization output
                     plotlyOutput(ns("pyramidPlot"), height = "500px")
                 )
             )
      )
    )
  )
}

# Server Function
donorPyramidServer <- function(id, filtered_data, fiscal_years, summary_stats, time_period = reactive("fiscal")) {
  moduleServer(id, function(input, output, session) {

    # Process the data for the donor pyramid
    processed_data <- reactive({
      req(filtered_data())

      # Get the filtered data
      data <- filtered_data()

      # If no data, return empty dataframe with proper structure
      if(nrow(data) == 0) {
        return(data.frame(
          Donor_Level = character(),
          Donors = integer(),
          Amount = numeric(),
          Donor_Percent = numeric(),
          Amount_Percent = numeric(),
          stringsAsFactors = FALSE
        ))
      }

      # Get range labels and thresholds from config
      range_labels <- config$distribution_ranges$labels
      range_thresholds <- config$distribution_ranges$thresholds

      # Calculate total giving per donor across selected time period
      donor_data <- data %>%
        group_by(`Constituent ID`) %>%
        summarise(
          Total_Giving = sum(as.numeric(`Fund Split Amount`), na.rm = TRUE),
          .groups = 'drop'
        )

      # Create a function that assigns a label based on amount
      assign_range_label <- function(amount) {
        for (i in seq_along(range_thresholds)) {
          if (amount >= range_thresholds[i]) {
            return(range_labels[i])
          }
        }
        return(tail(range_labels, 1)) # Return the last label as default
      }

      # Apply donor level categorization
      donor_data <- donor_data %>%
        mutate(
          Donor_Level = sapply(Total_Giving, assign_range_label),
          Donor_Level = factor(Donor_Level, levels = range_labels)
        )

      # Count donors and sum amounts by level
      pyramid_data <- donor_data %>%
        group_by(Donor_Level) %>%
        summarise(
          Donors = n(),
          Amount = sum(Total_Giving, na.rm = TRUE),
          .groups = 'drop'
        )

      # Make sure all levels are included even if zero donors
      levels_template <- data.frame(
        Donor_Level = factor(range_labels, levels = range_labels)
      )

      pyramid_data <- levels_template %>%
        left_join(pyramid_data, by = "Donor_Level") %>%
        mutate(
          Donors = replace_na(Donors, 0),
          Amount = replace_na(Amount, 0)
        )

      # Calculate percentages only if there are donors
      if(sum(pyramid_data$Donors) > 0) {
        pyramid_data <- pyramid_data %>%
          mutate(
            Donor_Percent = Donors / sum(Donors) * 100,
            Amount_Percent = if(sum(Amount) > 0) Amount / sum(Amount) * 100 else 0
          )
      } else {
        # Add zero percentages if no donors
        pyramid_data <- pyramid_data %>%
          mutate(
            Donor_Percent = 0,
            Amount_Percent = 0
          )
      }

      return(pyramid_data)
    })

    # Summary metrics
    output$pyramidSummary <- renderUI({
      data <- processed_data()

      if(nrow(data) == 0 || sum(data$Donors, na.rm = TRUE) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }

      # Create summary metrics
      total_donors <- sum(data$Donors, na.rm = TRUE)
      total_amount <- sum(data$Amount, na.rm = TRUE)

      # For average gift calculation, we need the original data
      # to count actual gifts rather than donors
      filtered_data_value <- filtered_data()
      gift_count <- nrow(filtered_data_value)
      avg_gift_size <- if(gift_count > 0) sum(as.numeric(filtered_data_value$`Fund Split Amount`), na.rm = TRUE) / gift_count else 0

      # Get the top donor levels - with error handling
      top_tier_donors <- 0
      top_tier_percent <- 0

      if(length(levels(data$Donor_Level)) > 0 && total_donors > 0) {
        top_levels <- head(levels(data$Donor_Level), min(2, length(levels(data$Donor_Level))))
        top_tier_donors <- sum(data$Donors[data$Donor_Level %in% top_levels], na.rm = TRUE)
        top_tier_percent <- (top_tier_donors / total_donors) * 100
      }

      # Format metrics
      formatted_amount <- format_currency(total_amount)
      formatted_avg <- format_currency(avg_gift_size)
      formatted_donors <- format_number(total_donors)

      # Create HTML with tooltips
      html <- paste0(
        '<div class="row" style="margin-bottom: 20px;">',

        '<div class="col-md-3 col-sm-6">',
        '<div class="panel panel-info" data-toggle="tooltip" title="Total number of unique donors across all giving levels">',
        '<div class="panel-heading text-center"><strong>Total Donors</strong></div>',
        '<div class="panel-body text-center"><h3>', formatted_donors, '</h3></div>',
        '</div></div>',

        '<div class="col-md-3 col-sm-6">',
        '<div class="panel panel-success" data-toggle="tooltip" title="Total amount of all gifts from all donors">',
        '<div class="panel-heading text-center"><strong>Total Amount</strong></div>',
        '<div class="panel-body text-center"><h3>', formatted_amount, '</h3></div>',
        '</div></div>',

        '<div class="col-md-3 col-sm-6">',
        '<div class="panel panel-warning" data-toggle="tooltip" title="Average amount per gift (total amount divided by number of gifts)">',
        '<div class="panel-heading text-center"><strong>Average Gift Size</strong></div>',
        '<div class="panel-body text-center"><h3>', formatted_avg, '</h3></div>',
        '</div></div>',

        '<div class="col-md-3 col-sm-6">',
        '<div class="panel panel-primary" data-toggle="tooltip" title="Percentage of donors in the top giving levels">',
        '<div class="panel-heading text-center"><strong>Top Tier Donors</strong></div>',
        '<div class="panel-body text-center"><h3>', sprintf("%.1f%%", top_tier_percent), '</h3></div>',
        '</div></div>',

        '</div>'
      )

      # Initialize tooltips via JavaScript
      html <- paste0(
        html,
        '<script>$(function () { $("[data-toggle=\'tooltip\']").tooltip(); });</script>'
      )

      HTML(html)
    })

    # Donor Pyramid Plot
    output$pyramidPlot <- renderPlotly({
      data <- processed_data()

      if(nrow(data) == 0 || sum(data$Donors, na.rm = TRUE) == 0) {
        # Return an empty plot with a message
        return(plot_ly() %>%
                 layout(title = "No data available for the selected filters"))
      }

      # Create color palette
      colors <- colorRampPalette(c("#08306b", "#4292c6", "#9ecae1"))(nrow(data))

      # Get the current time period
      current_time_period <- time_period()

      # Get current years
      years <- fiscal_years()

      # Time period label
      time_label <- ifelse(current_time_period == "fiscal",
                           "Fiscal Year",
                           "Calendar Year")

      # Create a dynamic title based on years
      if (length(years) == 0) {
        plot_title <- "Donor Pyramid"
      } else if (length(years) == 1) {
        plot_title <- paste("Donor Pyramid -", years[1], time_label)
      } else if (all(diff(as.numeric(years)) == 1)) {
        # Consecutive years
        plot_title <- paste("Donor Pyramid -",
                            years[1], "to", tail(years, 1),
                            paste0(time_label, "s"))
      } else {
        # Non-consecutive years
        plot_title <- paste("Donor Pyramid -",
                            paste(years, collapse = ", "),
                            paste0(time_label, "s"))
      }

      # Create the funnel chart for pyramid visualization
      plot_ly(data) %>%
        add_trace(
          type = "funnel",
          y = ~Donor_Level,
          x = ~Donors,
          marker = list(color = colors),
          textposition = "inside",
          textinfo = "value",
          hoverinfo = "text",
          hovertext = ~paste(
            "Level:", Donor_Level,
            "<br>Donors:", format_number(Donors),
            "<br>Amount:", format_currency(Amount),
            "<br>% of Donors:", sprintf("%.1f%%", Donor_Percent),
            "<br>% of Amount:", sprintf("%.1f%%", Amount_Percent)
          )
        ) %>%
        layout(
          title = plot_title,
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = levels(data$Donor_Level)
          ),
          showlegend = FALSE,
          margin = list(l = 130, r = 20, t = 50, b = 50)  # Increased left margin for labels
        )
    })
  })
}
