# UI Definition
ui <- fluidPage(
  # Use a built-in Shiny theme instead of custom CSS
  theme = shinythemes::shinytheme(config$ui$theme),  # You can choose any theme like "cerulean", "united", "flatly", etc.
  
  # Custom title panel with integrated update info
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
  
  # Filters section with Bootstrap styling
  div(
    class = "well",
    h4("Filter Data", class = "text-primary"),
    fluidRow(
      column(3, selectInput("campaignFilter", "Select Campaign ID:",
                            choices = unique(FullData$`Campaign ID`),
                            selected = unique(FullData$`Campaign ID`)[1])),
      column(3, selectInput("giftTypeFilter", "Select Gift Type:",
                            choices = levels(FullData$`Gift Type`),
                            multiple = TRUE)),
      column(3, selectInput("yearFilter", "Select Fiscal Years:",
                            choices = sort(unique(FullData$`Fiscal Year`)),
                            multiple = TRUE))
    )
  ),
  
  # Tab panel container using Bootstrap styling
  fluidRow(
    column(12,
           tabsetPanel(
             id = "tabset",
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
    )
  )
)