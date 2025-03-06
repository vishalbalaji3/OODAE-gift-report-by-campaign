# UI Definition
ui <- fluidPage(
  # Don't use built-in theme to avoid conflicts
  theme = NULL,
  
  # CSS fro config file
  tags$style(HTML(config$completeCSS())),
  
  # Simple title using titlePanel instead of styled div
  titlePanel("Campaign Data Analysis"),
  
  # Container for the rest of the UI
  div(
    class = "container-fluid",
    
    # Add data last updated information
    fluidRow(
      column(12, 
             div(class = "pull-right", 
                 textOutput("lastUpdatedText"),
                 style = paste0(
                   "font-style: italic; ",
                   "color: ", config$ui$colors$text, "; ",
                   "opacity: 0.7; ",
                   "margin-bottom: 15px;"
                 ))
      )
    ),
    
    # Filters section with styling
    div(
      class = "filters-section",
      h4("Filter Data", style = paste0("margin-top: 0; color: ", config$ui$colors$header, ";")),
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
    
    # Tab panel container
    fluidRow(
      column(12,
             tabsetPanel(
               id = "tabset",
               # Summary Statistics tab
               tabPanel("Summary Statistics", summaryStatisticsUI("summary")),
               
               # Fund Split by Constituency tab
               tabPanel("Fund Split by Constituency", fundSplitUI("fundSplit")),
               
               # Fund Analysis tab
               tabPanel("Fund Analysis", fundAnalysisUI("fundAnalysis")),
               
               # Unique Constituents tab
               tabPanel("Unique Constituents", constituentsUI("constituents")),
               
               # Average Gift Size tab
               tabPanel("Average Gift Size", avgGiftUI("avgGift")),
               
               # Top Donors tab
               tabPanel("Top Donors", topDonorsUI("topDonors")),
               
               # Gift Size Distribution tab
               tabPanel("Gift Size Distribution", giftDistUI("giftDist")),
               
               # Donor Levels tab
               tabPanel("Donor Levels", donorLevelsUI("donorLevels")),
               
               # Full Data tab
               tabPanel("Full Data", fullDataUI("fullData"))
             )
      )
    )
  )
)