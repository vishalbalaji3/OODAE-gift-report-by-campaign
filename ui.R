# ui.R update

ui <- fluidPage(
  # Keep your existing theme
  theme = shinythemes::shinytheme(config$ui$theme),
  
  # Title panel at the top
  fluidRow(
    column(12,
           div(class = "page-header", style = "margin-top: 20px; margin-bottom: 20px; position: relative;",
               h2("Campaign Data Analysis", style = "margin-bottom: 5px;"),
               div(class = "text-muted", style = "font-size: 12px; font-style: italic;",
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
             # Put filters inside each main tab
             div(
               class = "well",
               h4("Filter Data", class = "text-primary"),
               fluidRow(
                 column(3, selectInput("dataTabFilter_campaign", "Select Campaign ID:",
                                       choices = unique(FullData$`Campaign ID`),
                                       selected = unique(FullData$`Campaign ID`)[1])),
                 column(3, selectInput("dataTabFilter_giftType", "Select Gift Type:",
                                       choices = levels(FullData$`Gift Type`),
                                       multiple = TRUE)),
                 column(3, selectInput("dataTabFilter_year", "Select Fiscal Years:",
                                       choices = sort(unique(FullData$`Fiscal Year`)),
                                       multiple = TRUE))
               )
             ),
             
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
             # Similar filters for visualizations
             div(
               class = "well",
               h4("Filter Data", class = "text-primary"),
               fluidRow(
                 column(3, selectInput("vizFilter_campaign", "Select Campaign ID:",
                                       choices = unique(FullData$`Campaign ID`),
                                       selected = unique(FullData$`Campaign ID`)[1])),
                 column(3, selectInput("vizFilter_giftType", "Select Gift Type:",
                                       choices = levels(FullData$`Gift Type`),
                                       multiple = TRUE)),
                 column(3, selectInput("vizFilter_year", "Select Fiscal Years:",
                                       choices = sort(unique(FullData$`Fiscal Year`)),
                                       multiple = TRUE))
               )
             ),
             
             # Visualization tabs
             tabsetPanel(
               id = "vizTabset",
               tabPanel("Overview", 
                        h3("Visualization Overview"),
                        p("Select a visualization category from the tabs above.")
               )
               # Additional visualization tabs will go here
             )
    )
  )
)