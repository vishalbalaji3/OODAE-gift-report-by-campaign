# Simplified OODAE Gift Report Application
# This version consolidates functionality while maintaining core features

# Load required libraries
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(readr)
library(writexl)

# Simple configuration
app_config <- list(
  theme = "readable",
  currency_format = function(x) paste0("$", format(x, big.mark = ",", nsmall = 0, scientific = FALSE)),
  date_format = "%m/%d/%Y"
)

# Data preparation (simplified)
prepare_data <- function() {
  # Load your data here - adjust path as needed
  if (file.exists("data/gift_data.csv")) {
    data <- read_csv("data/gift_data.csv")
  } else {
    # Create sample data for demonstration
    data <- data.frame(
      date = Sys.Date() - sample(1:365, 100, replace = TRUE),
      amount = sample(c(50, 100, 500, 1000, 5000), 100, replace = TRUE),
      donor_name = paste("Donor", 1:100),
      constituency = sample(c("Alumni", "Faculty", "Friends", "Corporation"), 100, replace = TRUE),
      fund = sample(c("General Fund", "Scholarship", "Research", "Building"), 100, replace = TRUE)
    )
  }
  return(data)
}

# UI Definition
ui <- fluidPage(
  theme = shinythemes::shinytheme(app_config$theme),
  
  # Add custom CSS for value boxes
  tags$head(
    tags$style(HTML("
      .value-box {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 15px;
        margin: 5px;
        text-align: center;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .value-box h4 {
        margin: 0 0 10px 0;
        color: #495057;
        font-size: 14px;
        font-weight: bold;
      }
      .value-box .value {
        font-size: 24px;
        font-weight: bold;
        color: #007bff;
        margin: 0;
      }
    "))
  ),
  
  titlePanel("OODAE Gift Report - Simplified"),
  
  # Simple filter panel
  wellPanel(
    fluidRow(
      column(3, dateRangeInput("date_range", "Date Range:", 
                              start = Sys.Date() - 365, end = Sys.Date())),
      column(3, selectInput("constituency_filter", "Constituency:", 
                           choices = NULL, multiple = TRUE)),
      column(3, selectInput("fund_filter", "Fund:", 
                           choices = NULL, multiple = TRUE)),
      column(3, br(), actionButton("reset_filters", "Reset Filters", class = "btn-warning"))
    )
  ),
  
  # Main content tabs
  tabsetPanel(
    tabPanel("Summary", 
      fluidRow(
        column(3, 
          div(class = "value-box",
            h4("Total Gifts"),
            div(class = "value", textOutput("total_gifts", inline = TRUE))
          )
        ),
        column(3, 
          div(class = "value-box",
            h4("Total Donors"),
            div(class = "value", textOutput("total_donors", inline = TRUE))
          )
        ),
        column(3, 
          div(class = "value-box",
            h4("Average Gift"),
            div(class = "value", textOutput("avg_gift", inline = TRUE))
          )
        ),
        column(3, 
          div(class = "value-box",
            h4("Largest Gift"),
            div(class = "value", textOutput("largest_gift", inline = TRUE))
          )
        )
      ),
      br(),
      DT::dataTableOutput("summary_table")
    ),
    
    tabPanel("Detailed Data", 
      DT::dataTableOutput("detailed_table"),
      br(),
      downloadButton("download_data", "Download Excel", class = "btn-primary")
    ),
    
    tabPanel("Analysis",
      fluidRow(
        column(6, plotOutput("constituency_plot")),
        column(6, plotOutput("fund_plot"))
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Load data
  raw_data <- prepare_data()
  
  # Update filter choices
  updateSelectInput(session, "constituency_filter", 
                   choices = unique(raw_data$constituency))
  updateSelectInput(session, "fund_filter", 
                   choices = unique(raw_data$fund))
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- raw_data
    
    # Date filter
    data <- data[data$date >= input$date_range[1] & data$date <= input$date_range[2], ]
    
    # Constituency filter
    if (!is.null(input$constituency_filter) && length(input$constituency_filter) > 0) {
      data <- data[data$constituency %in% input$constituency_filter, ]
    }
    
    # Fund filter
    if (!is.null(input$fund_filter) && length(input$fund_filter) > 0) {
      data <- data[data$fund %in% input$fund_filter, ]
    }
    
    return(data)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_range", 
                        start = Sys.Date() - 365, end = Sys.Date())
    updateSelectInput(session, "constituency_filter", selected = NULL)
    updateSelectInput(session, "fund_filter", selected = NULL)
  })
  
  # Summary metrics (simplified value boxes using renderText)
  output$total_gifts <- renderText({
    app_config$currency_format(sum(filtered_data()$amount))
  })
  
  output$total_donors <- renderText({
    length(unique(filtered_data()$donor_name))
  })
  
  output$avg_gift <- renderText({
    app_config$currency_format(mean(filtered_data()$amount))
  })
  
  output$largest_gift <- renderText({
    app_config$currency_format(max(filtered_data()$amount))
  })
  
  # Summary table
  output$summary_table <- DT::renderDataTable({
    filtered_data() %>%
      group_by(constituency, fund) %>%
      summarise(
        Gifts = n(),
        Total = sum(amount),
        Average = mean(amount),
        .groups = 'drop'
      ) %>%
      mutate(
        Total = app_config$currency_format(Total),
        Average = app_config$currency_format(Average)
      )
  }, options = list(pageLength = 10))
  
  # Detailed table
  output$detailed_table <- DT::renderDataTable({
    filtered_data() %>%
      mutate(amount = app_config$currency_format(amount)) %>%
      arrange(desc(date))
  }, options = list(pageLength = 15))
  
  # Simple plots
  output$constituency_plot <- renderPlot({
    data <- filtered_data() %>%
      group_by(constituency) %>%
      summarise(total = sum(amount), .groups = 'drop')
    
    barplot(data$total, names.arg = data$constituency, 
            main = "Gifts by Constituency", ylab = "Total Amount",
            col = rainbow(nrow(data)))
  })
  
  output$fund_plot <- renderPlot({
    data <- filtered_data() %>%
      group_by(fund) %>%
      summarise(total = sum(amount), .groups = 'drop')
    
    barplot(data$total, names.arg = data$fund, 
            main = "Gifts by Fund", ylab = "Total Amount",
            col = heat.colors(nrow(data)))
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("gift_report_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(filtered_data(), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server) 