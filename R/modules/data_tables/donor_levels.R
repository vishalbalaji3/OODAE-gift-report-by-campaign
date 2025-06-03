# Donor Levels Module

# UI Function
donorLevelsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading",
                     h4("Donor Levels Summary")),
                 div(class = "panel-body",
                     uiOutput(ns("donorLevelsSummary")))
             )
      )
    ),

    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading",
                     h4("Number of Donors by Level")),
                 div(class = "panel-body",
                     withSpinner(DTOutput(ns("donorCountTable"))))
             )
      )
    ),

    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading",
                     h4("Donation Amount by Level")),
                 div(class = "panel-body",
                     withSpinner(DTOutput(ns("donorAmountTable"))))
             )
      )
    ),

    create_download_buttons(ns)
  )
}

# Server Function
donorLevelsServer <- function(id, filtered_data, fiscal_years, summary_stats, time_period = reactive("fiscal")) {
  moduleServer(id, function(input, output, session) {

    # Reactive expression for processed data
    processed_data <- reactive({
      process_donor_levels_data(filtered_data(), time_period = time_period())
    })

    # Donor Levels Summary
    output$donorLevelsSummary <- renderUI({
      # Get donor level data
      donor_data <- process_donor_levels_summary(filtered_data(), time_period = time_period())

      if (nrow(donor_data) == 0) {
        return(HTML("<p>No data available for the selected filters.</p>"))
      }

      # Create a summary table HTML
      html_table <- '<table class="table table-striped table-bordered">'
      html_table <- paste0(html_table, '<thead><tr>',
                           '<th>Donor Level</th>',
                           '<th class="text-right"># of Donors</th>',
                           '<th class="text-right">Total Amount</th>',
                           '<th class="text-right">% of Total</th>',
                           '</tr></thead><tbody>')

      # Calculate grand total for percentage calculation
      grand_total <- sum(donor_data$Total_Amount, na.rm = TRUE)
      total_donors <- sum(donor_data$Number_of_Donors, na.rm = TRUE)

      # Add rows for each donor level
      for(i in 1:nrow(donor_data)) {
        row <- donor_data[i, ]

        # Format donor count with commas
        formatted_donors <- format_number(row$Number_of_Donors)

        percent <- sprintf("%.1f%%", (row$Total_Amount / grand_total) * 100)

        html_table <- paste0(html_table, '<tr>',
                             '<td>', row$Donor_Level, '</td>',
                             '<td class="text-right">', formatted_donors, '</td>',
                             '<td class="text-right">', format_currency(row$Total_Amount), '</td>',
                             '<td class="text-right">', percent, '</td>',
                             '</tr>')
      }

      # Add totals row with formatted number
      formatted_total_donors <- format_number(total_donors)

      html_table <- paste0(html_table, '<tr class="info">',
                           '<th>Total</th>',
                           '<th class="text-right">', formatted_total_donors, '</th>',
                           '<th class="text-right">', format_currency(grand_total), '</th>',
                           '<th class="text-right">100.0%</th>',
                           '</tr></tbody></table>')

      HTML(html_table)
    })

    # Donor Count Table
    output$donorCountTable <- renderDT({
      data <- processed_data()

      if (nrow(data) == 0 || ncol(data) <= 1) {
        return(datatable(data.frame(Message = "No data available for the selected filters."),
                         options = list(dom = 't')))
      }

      # Determine prefix based on time period
      prefix <- ifelse(time_period() == "calendar", "CY", "FY")

      # Extract only donor count columns
      donor_cols <- c("Donor_Level", grep(paste0("^Donors_", prefix), names(data), value = TRUE), "Total_Donors")
      display_data <- data[, donor_cols]

      # Rename columns for better readability
      col_names <- names(display_data)
      for (i in seq_along(col_names)) {
        if (startsWith(col_names[i], paste0("Donors_", prefix, "_"))) {
          year <- substr(col_names[i], nchar(paste0("Donors_", prefix, "_")) + 1, nchar(col_names[i]))
          col_names[i] <- paste0(prefix, " ", year)
        }
      }
      names(display_data) <- col_names

      # Rename the total column
      names(display_data)[names(display_data) == "Total_Donors"] <- "Total"

      # Create datatable
      create_datatable(display_data)
    })

    # Donor Amount Table
    output$donorAmountTable <- renderDT({
      data <- processed_data()

      if (nrow(data) == 0 || ncol(data) <= 1) {
        return(datatable(data.frame(Message = "No data available for the selected filters."),
                         options = list(dom = 't')))
      }

      # Determine prefix based on time period
      prefix <- ifelse(time_period() == "calendar", "CY", "FY")

      # Extract only amount columns
      amount_cols <- c("Donor_Level", grep(paste0("^Amount_", prefix), names(data), value = TRUE), "Total_Amount")
      display_data <- data[, amount_cols]

      # Rename columns for better readability
      col_names <- names(display_data)
      for (i in seq_along(col_names)) {
        if (startsWith(col_names[i], paste0("Amount_", prefix, "_"))) {
          year <- substr(col_names[i], nchar(paste0("Amount_", prefix, "_")) + 1, nchar(col_names[i]))
          col_names[i] <- paste0(prefix, " ", year)
        }
      }
      names(display_data) <- col_names

      # Rename the total column
      names(display_data)[names(display_data) == "Total_Amount"] <- "Total"

      # All columns except Donor_Level should be formatted as currency
      currency_cols <- which(names(display_data) != "Donor_Level") - 1

      # Create datatable
      create_datatable(display_data, currency_cols)
    })

    # Create download handlers
    downloads <- create_download_handlers("donor_levels_data", processed_data, session)

    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}
