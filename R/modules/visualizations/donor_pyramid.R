# Donor Pyramid Visualization Module

# UI Function
donorPyramidUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading", 
                     h4("Donor Pyramid"),
                     # Fix alignment of time period selector
                     div(style = "position: absolute; right: 15px; top: 5px;",
                         radioButtons(ns("timeframe"), "Time Period:",
                                      choices = c("Fiscal Year" = "fiscal", 
                                                  "Calendar Year" = "calendar"),
                                      selected = "fiscal",
                                      inline = TRUE)
                     )
                 ),
                 div(class = "panel-body",
                     # Summary metrics above the visualization
                     uiOutput(ns("pyramidSummary")),
                     
                     # The main visualization output
                     plotlyOutput(ns("pyramidPlot"), height = "500px")
                 )
             )
      )
    ),
    
    # Download options
    fluidRow(
      column(12,
             div(class = "btn-group", style = "margin-top: 20px; margin-bottom: 30px;",
                 downloadButton(ns("download_png"), "Download PNG", class = "btn-primary"),
                 downloadButton(ns("download_csv"), "Download Data", class = "btn-primary")
             )
      )
    )
  )
}

# Server Function
donorPyramidServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Process the data for the donor pyramid
    processed_data <- reactive({
      # Get the filtered data
      data <- filtered_data()
      
      # If no data, return empty dataframe
      if(nrow(data) == 0) {
        return(data.frame(
          Donor_Level = character(),
          Donors = integer(),
          Amount = numeric(),
          stringsAsFactors = FALSE
        ))
      }
      
      # Process dates based on selected time period
      if(input$timeframe == "calendar") {
        # For calendar year, create a new grouping column
        data <- data %>%
          mutate(Year_Group = format(as.Date(`Gift Date`), "%Y"))
      } else {
        # For fiscal year, use the existing column
        data <- data %>%
          mutate(Year_Group = `Fiscal Year`)
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
          Amount = replace_na(Amount, 0),
          # Calculate percentages
          Donor_Percent = if(sum(Donors) > 0) Donors / sum(Donors) * 100 else 0,
          Amount_Percent = if(sum(Amount) > 0) Amount / sum(Amount) * 100 else 0
        )
      
      return(pyramid_data)
    })
    
    # Summary metrics
    output$pyramidSummary <- renderUI({
      data <- processed_data()
      
      if(nrow(data) == 0 || sum(data$Donors) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }
      
      # Create summary metrics
      total_donors <- sum(data$Donors)
      total_amount <- sum(data$Amount)
      
      # For average gift calculation, we need the original data
      # to count actual gifts rather than donors
      gift_count <- nrow(filtered_data())
      avg_gift_size <- if(gift_count > 0) sum(as.numeric(filtered_data()$`Fund Split Amount`), na.rm = TRUE) / gift_count else 0
      
      # Get the top 2 donor levels
      top_levels <- head(levels(data$Donor_Level), 2)
      top_tier_donors <- sum(data$Donors[data$Donor_Level %in% top_levels])
      top_tier_percent <- if(total_donors > 0) top_tier_donors / total_donors * 100 else 0
      
      # Format metrics
      formatted_amount <- format_currency(total_amount)
      formatted_avg <- format_currency(avg_gift_size)
      
      # Create HTML with tooltips
      html <- paste0(
        '<div class="row" style="margin-bottom: 20px;">',
        
        '<div class="col-md-3 col-sm-6">',
        '<div class="panel panel-info" data-toggle="tooltip" title="Total number of unique donors across all giving levels">',
        '<div class="panel-heading text-center"><strong>Total Donors</strong></div>',
        '<div class="panel-body text-center"><h3>', format(total_donors, big.mark = ","), '</h3></div>',
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
        '<div class="panel panel-primary" data-toggle="tooltip" title="Percentage of donors in the top two giving levels (', 
        paste(top_levels, collapse = ' and '), ')">',
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
      
      if(nrow(data) == 0 || sum(data$Donors) == 0) {
        # Return an empty plot with a message
        return(plot_ly() %>% 
                 layout(title = "No data available for the selected filters"))
      }
      
      # Create color palette
      colors <- colorRampPalette(c("#08306b", "#4292c6", "#9ecae1"))(nrow(data))
      
      # Time period label for title
      time_label <- ifelse(input$timeframe == "fiscal", 
                           "Fiscal Year", 
                           "Calendar Year")
      
      # Create the pyramid plot (horizontal bar chart)
      plot_ly(data, 
              type = "bar",
              x = ~Donors,
              y = ~Donor_Level,
              orientation = 'h',
              marker = list(color = colors),
              hoverinfo = "text",
              hovertext = ~paste(
                "Level:", Donor_Level,
                "<br>Donors:", format(Donors, big.mark = ","),
                "<br>Amount:", format_currency(Amount),
                "<br>% of Donors:", sprintf("%.1f%%", Donor_Percent),
                "<br>% of Amount:", sprintf("%.1f%%", Amount_Percent)
              ),
              name = "Donors"
      ) %>%
        layout(
          title = paste("Donor Pyramid -", time_label),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = rev(levels(data$Donor_Level))
          ),
          xaxis = list(
            title = "Number of Donors",
            showgrid = TRUE
          ),
          showlegend = FALSE,
          margin = list(l = 50, r = 20, t = 50, b = 50)
        )
    })
    
    # Download handlers
    output$download_png <- downloadHandler(
      filename = function() {
        paste0("donor_pyramid_", Sys.Date(), ".png")
      },
      content = function(file) {
        # Export to PNG using webshot package
        htmlwidgets::saveWidget(plotly_build(output$pyramidPlot), 
                                file = "temp.html",
                                selfcontained = TRUE)
        
        if (requireNamespace("webshot", quietly = TRUE)) {
          webshot::webshot("temp.html", file = file)
          if (file.exists("temp.html")) file.remove("temp.html")
        } else {
          message("Please install the webshot package for PNG export")
        }
      }
    )
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("donor_pyramid_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Write data to CSV
        write.csv(processed_data(), file, row.names = FALSE)
      }
    )
  })
}