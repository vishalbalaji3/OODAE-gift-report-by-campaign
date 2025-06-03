# R/modules/viz_template.R

# Visualization Module Template

# UI Function
vizTemplateUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(12,
             div(class = "panel panel-default",
                 div(class = "panel-heading",
                     h4("Visualization Title")),
                 div(class = "panel-body",
                     # Summary metrics above the visualization
                     uiOutput(ns("vizSummary")),
                     # The main visualization output
                     plotlyOutput(ns("mainPlot"), height = "500px")
                 )
             )
      )
    ),

    # Download options
    fluidRow(
      column(12,
             div(class = "btn-group", style = "margin-top: 20px; margin-bottom: 30px;",
                 downloadButton(ns("download_pdf"), "Download PDF", class = "btn-primary"),
                 downloadButton(ns("download_png"), "Download PNG", class = "btn-primary"),
                 downloadButton(ns("download_csv"), "Download Data", class = "btn-primary")
             )
      )
    )
  )
}

# Server Function
vizTemplateServer <- function(id, filtered_data, fiscal_years, summary_stats) {
  moduleServer(id, function(input, output, session) {

    # Reactive expression for processed data specific to this visualization
    processed_data <- reactive({
      # Process the filtered data for this specific visualization
      # Return the processed data
    })

    # Render summary metrics
    output$vizSummary <- renderUI({
      # Create HTML summary
      HTML("Summary content goes here")
    })

    # Render the main visualization
    output$mainPlot <- renderPlotly({
      # Create the plotly visualization
      plot_ly(...)
    })

    # Define download handlers
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("visualization_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # PDF export logic
      }
    )

    output$download_png <- downloadHandler(
      filename = function() {
        paste0("visualization_", Sys.Date(), ".png")
      },
      content = function(file) {
        # PNG export logic
      }
    )

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("visualization_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # CSV export logic using the processed data
        write.csv(processed_data(), file, row.names = FALSE)
      }
    )
  })
}
