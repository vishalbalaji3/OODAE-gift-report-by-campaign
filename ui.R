# UI Definition
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Campaign Data Analysis"),
  
  # Add data last updated information
  fluidRow(
    column(12, 
           div(class = "pull-right", 
               textOutput("lastUpdatedText"),
               style = "font-style: italic; color: #666; margin-bottom: 15px;")
    )
  ),
  
  # Filters
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
           ))
  )
)