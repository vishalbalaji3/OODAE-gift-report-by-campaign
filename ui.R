# UI Definition
ui <- fluidPage(
  # Don't use built-in theme to avoid conflicts
  theme = NULL,
  
  # Add custom CSS using config values
  tags$head(
    tags$style(HTML(paste0("
      /* Base styling */
      body {
        font-family: ", config$ui$typography$fontFamily, ";
        font-size: ", config$ui$typography$fontSize, ";
        line-height: ", config$ui$typography$lineHeight, ";
        background-color: white;
        color: ", config$ui$colors$text, ";
      }
      
      /* Header styling */
      h1, h2, h3, h4, h5, h6 {
        color: ", config$ui$colors$text, ";
        font-weight: ", config$ui$typography$headerFontWeight, ";
      }
      
      /* Link styling */
      a {
        color: ", config$ui$colors$link, ";
      }
      a:hover {
        color: ", config$ui$colors$secondary, ";
      }
      
      /* Panel styling */
      .panel {
        ", config$ui$panel$style, "
      }
      .panel-heading {
        ", config$ui$panel$headerStyle, "
      }
      
      /* Button styling - more subtle */
      .btn-default {
        background-color: white;
        color: ", config$ui$colors$text, ";
        border-color: ", config$ui$colors$border, ";
        border-radius: ", config$ui$spacing$borderRadius, ";
        font-weight: 500;
        padding: 6px 12px;
        transition: all 0.3s ease;
      }
      .btn-default:hover, .btn-default:focus, .btn-default:active {
        background-color: ", config$ui$colors$highlight, ";
        border-color: ", config$ui$colors$border, ";
        color: ", config$ui$colors$text, ";
      }
      
      /* Tab styling - more subtle */
      .nav-tabs {
        border-bottom: 1px solid ", config$ui$colors$border, ";
        margin-bottom: 20px;
      }
      .nav-tabs > li > a {
        background-color: ", config$ui$colors$panel, ";
        border: 1px solid ", config$ui$colors$border, ";
        border-bottom: none;
        border-radius: 4px 4px 0 0;
        margin-right: 4px;
        padding: 10px 15px;
        color: ", config$ui$colors$text, ";
      }
      .nav-tabs > li > a:hover, .nav-tabs > li > a:focus {
        background-color: ", config$ui$colors$highlight, " !important;
        border-color: ", config$ui$colors$border, ";
        color: ", config$ui$colors$text, " !important;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        background-color: white !important;
        border: 1px solid ", config$ui$colors$border, ";
        border-bottom: 2px solid ", config$ui$colors$secondary, ";
        font-weight: 600;
        color: ", config$ui$colors$text, " !important;
      }
      
      /* Table styling with !important to override theme */
      .dataTable thead th {
        background-color: ", config$ui$colors$panel, " !important;
        color: ", config$ui$colors$text, " !important;
        font-weight: 600 !important;
        border-bottom: 2px solid ", config$ui$colors$border, " !important;
      }
      .dataTable tbody tr {
        border-bottom: 1px solid ", config$ui$colors$border, " !important;
        background-color: white !important;
      }
      .dataTable.stripe tbody tr.odd {
        background-color: ", config$ui$colors$panel, " !important;
      }
      .dataTable.hover tbody tr:hover, .dataTable.hover tbody tr.odd:hover {
        background-color: ", config$ui$colors$highlight, " !important;
      }
      
      /* Info row styling (for summary sections) */
      tr.info, tr.info th, tr.info td {
        background-color: ", config$ui$colors$highlight, " !important;
        font-weight: bold !important;
      }
      
      /* Custom filter section */
      .filters-section {
        background-color: ", config$ui$colors$panel, ";
        border: 1px solid ", config$ui$colors$border, ";
        border-radius: ", config$ui$spacing$borderRadius, ";
        padding: ", config$ui$spacing$padding, ";
        margin-bottom: ", config$ui$spacing$margin, ";
      }
      
      /* Override any remaining bootstrap theme colors */
      .shiny-output-error { color: #d9534f; }
      .bg-primary { background-color: ", config$ui$colors$secondary, " !important; }
      .text-primary { color: ", config$ui$colors$secondary, " !important; }
    ")))
  ),
  
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