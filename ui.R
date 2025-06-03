# ui.R update

ui <- fluidPage(
  # Keep your existing theme
  theme = shinythemes::shinytheme(config$ui$theme),
  
  # Include shared components CSS
  includeCSS("www/shared_components.css"),

  # Title panel at the top
  fluidRow(
    column(12,
      div(class = "page-header",
        style = "margin-top: 20px; margin-bottom: 20px; position: relative;",
        h2("Campaign Data Analysis", style = "margin-bottom: 5px;"),
        div(class = "text-muted",
          style = "font-size: 12px; font-style: italic;",
          textOutput("lastUpdatedText")
        )
      )
    )
  ),

  # Main navigation - this should be first in the hierarchy
  navbarPage(
    title = NULL, id = "mainNav",

    # Data Tables Tab
    tabPanel("Data Tables",
      # Use shared filter panel component
      create_filter_panel("dataTabFilter"),

      # Data tables tabs
      tabsetPanel(
        id = "dataTabset",
        tabPanel("Summary Statistics", summaryStatisticsUI("summary")),
        tabPanel("Fund Split by Constituency", fundSplitUI("fundSplit")),
        tabPanel("Fund Analysis", fundAnalysisUI("fundAnalysis")),
        tabPanel("Unique Constituents", constituentsUI("constituents")),
        tabPanel("Average Gift Size", avgGiftUI("avgGift")),
        tabPanel("Top Donors", topDonorsUI("topDonors")),
        tabPanel("Gift Size Distribution", giftDistUI("giftDist")),
        tabPanel("Donor Levels", donorLevelsUI("donorLevels")),
        tabPanel("Full Data", fullDataUI("fullData"))
      )
    ),

    # Visualizations Tab
    tabPanel("Visualizations",
      # Use shared filter panel component
      create_filter_panel("vizFilter"),

      # Visualization tabs
      tabsetPanel(
        id = "vizTabset",
        tabPanel("Overview",
          h3("Visualization Overview"),
          p("Select a visualization category from the tabs above.")
        ),
        tabPanel("Donor Pyramid", donorPyramidUI("donor_pyramid")),
        tabPanel("Donor Retention", donorRetentionUI("donor_retention"))
        # Additional visualization tabs will go here
      )
    )
  )
)
