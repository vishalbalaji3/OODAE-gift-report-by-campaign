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
             tabPanel("Summary Statistics",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   uiOutput("summaryHeading"),
                                   uiOutput("giftDistSummary")
                               )
                        )
                      ),
                      downloadButton("downloadSummary_csv", "Download Full CSV"),
                      downloadButton("downloadSummary_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("summaryTable")))),
             
             # Fund Split by Constituency tab
             tabPanel("Fund Split by Constituency",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Fund Split by Constituency"),
                                   uiOutput("fundSplitSummary")
                               )
                        )
                      ),
                      downloadButton("downloadFundSplit_csv", "Download Full CSV"),
                      downloadButton("downloadFundSplit_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("fundSplitTable")))),
             
             # Fund Analysis tab
             tabPanel("Fund Analysis",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Top Funds Summary"),
                                   uiOutput("fundAnalysisSummary")
                               )
                        )
                      ),
                      downloadButton("downloadFundAnalysis_csv", "Download Full CSV"),
                      downloadButton("downloadFundAnalysis_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("fundAnalysisTable")))),
             
             # Unique Constituents tab
             tabPanel("Unique Constituents",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Unique Constituents Summary"),
                                   uiOutput("constituentsSummary")
                               )
                        )
                      ),
                      downloadButton("downloadConstituents_csv", "Download Full CSV"),
                      downloadButton("downloadConstituents_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("uniqueConstituentsTable")))),
             
             # Average Gift Size tab
             tabPanel("Average Gift Size",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Average Gift Size Summary"),
                                   uiOutput("avgGiftSummary")
                               )
                        )
                      ),
                      downloadButton("downloadAvgGift_csv", "Download Full CSV"),
                      downloadButton("downloadAvgGift_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("avgGiftTable")))),
             
             # Top Donors tab
             tabPanel("Top Donors",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Top Donors Summary"),
                                   uiOutput("topDonorsSummary")
                               )
                        )
                      ),
                      downloadButton("downloadTopDonors_csv", "Download Full CSV"),
                      downloadButton("downloadTopDonors_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("topDonorsTable")))),
             
             # Gift Size Distribution tab
             tabPanel("Gift Size Distribution",
                      fluidRow(
                        column(12, 
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Gift Count by Range"),
                                   withSpinner(DTOutput("giftCountTable"))
                               )
                        )
                      ),
                      hr(),
                      fluidRow(
                        column(12, 
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Gift Amount by Range"),
                                   withSpinner(DTOutput("giftAmountTable"))
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(style = "margin-top: 20px;"),
                               downloadButton("downloadGiftDist_csv", "Download Full CSV"),
                               downloadButton("downloadGiftDist_excel", "Download Full Excel")
                        )
                      )
             ),
             
             # Donor Levels tab
             tabPanel("Donor Levels",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Donor Levels Summary"),
                                   uiOutput("donorLevelsSummary")
                               )
                        )
                      ),
                      # First table - Number of Donors
                      fluidRow(
                        column(12, 
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Number of Donors by Level"),
                                   withSpinner(DTOutput("donorCountTable"))
                               )
                        )
                      ),
                      # Second table - Donation Amounts
                      fluidRow(
                        column(12, 
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Donation Amount by Level"),
                                   withSpinner(DTOutput("donorAmountTable"))
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(style = "margin-top: 20px;"),
                               downloadButton("downloadDonorLevels_csv", "Download Full CSV"),
                               downloadButton("downloadDonorLevels_excel", "Download Full Excel")
                        )
                      )),
             
             # Full Data tab
             tabPanel("Full Data",
                      fluidRow(
                        column(12,
                               div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                                   h4("Data Overview"),
                                   uiOutput("fullDataSummary")
                               )
                        )
                      ),
                      downloadButton("downloadFullData_csv", "Download Full CSV"),
                      downloadButton("downloadFullData_excel", "Download Full Excel"),
                      div(style = 'overflow-x: scroll',
                          withSpinner(DTOutput("fullDataTable"))))
           ))
  )
)