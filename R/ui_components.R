# Shared UI Components
# This file contains reusable UI components to eliminate duplication

# ========== FILTER PANEL COMPONENT ==========

create_filter_panel <- function(id_prefix, full_data = NULL) {
  
  # Use FullData if available, otherwise use provided data
  data_source <- if (is.null(full_data) && exists("FullData")) FullData else full_data
  
  if (is.null(data_source)) {
    stop("No data source available for filter panel")
  }
  
  div(
    class = "well",
    h4("Filter Data", class = "text-primary"),
    fluidRow(
      column(3, 
             selectInput(paste0(id_prefix, "_campaign"), 
                        "Select Campaign ID:",
                        choices = unique(data_source$`Campaign ID`),
                        selected = unique(data_source$`Campaign ID`)[1])
      ),
      column(3, 
             selectInput(paste0(id_prefix, "_giftType"), 
                        "Select Gift Type:",
                        choices = levels(data_source$`Gift Type`),
                        multiple = TRUE)
      ),
      column(3, 
             radioButtons(paste0(id_prefix, "_timeframe"), 
                         "Time Period:",
                         choices = c("Fiscal Year" = "fiscal", 
                                    "Calendar Year" = "calendar"),
                         selected = "fiscal",
                         inline = TRUE)
      ),
      column(3, 
             uiOutput(paste0(id_prefix, "_yearUI"))
      )
    )
  )
}

# ========== DOWNLOAD BUTTONS COMPONENT ==========

create_download_buttons <- function(ns) {
  fluidRow(
    column(12,
           div(class = "text-right", style = "margin-top: 15px;",
               downloadButton(ns("download_csv"), "Download CSV", class = "btn btn-primary btn-sm"),
               " ",
               downloadButton(ns("download_excel"), "Download Excel", class = "btn btn-success btn-sm")
           )
    )
  )
}

# ========== DATA TABLE COMPONENT ==========

create_datatable <- function(data, currency_cols = NULL, scroll_x = TRUE, page_length = 25) {
  
  # Default options
  dt_options <- list(
    pageLength = page_length,
    scrollX = scroll_x,
    autoWidth = TRUE,
    columnDefs = list(
      list(className = 'dt-right', targets = currency_cols)
    )
  )
  
  # Format currency columns if specified
  if (!is.null(currency_cols) && length(currency_cols) > 0) {
    datatable(data, 
              options = dt_options,
              rownames = FALSE) %>%
      formatCurrency(currency_cols + 1, # DT uses 0-based indexing, R uses 1-based
                     currency = "$", 
                     digits = 0)
  } else {
    datatable(data, 
              options = dt_options,
              rownames = FALSE)
  }
}

# ========== SUMMARY STATISTICS PANEL ==========

create_summary_panel <- function(stats_reactive) {
  div(
    class = "panel panel-info",
    div(class = "panel-heading", h4("Summary Statistics")),
    div(class = "panel-body",
        fluidRow(
          column(3,
                 div(class = "text-center",
                     h3(textOutput("total_gifts"), class = "text-primary"),
                     p("Total Gifts")
                 )
          ),
          column(3,
                 div(class = "text-center",
                     h3(textOutput("total_amount"), class = "text-success"),
                     p("Total Amount")
                 )
          ),
          column(3,
                 div(class = "text-center",
                     h3(textOutput("total_constituents"), class = "text-info"),
                     p("Unique Constituents")
                 )
          ),
          column(3,
                 div(class = "text-center",
                     h3(textOutput("avg_gift_size"), class = "text-warning"),
                     p("Average Gift Size")
                 )
          )
        )
    )
  )
}

# ========== HELPER FUNCTIONS ==========

# Format numbers with appropriate abbreviations (K, M)
format_number <- function(x, abbreviate = TRUE) {
  if (!abbreviate) {
    return(format(x, big.mark = ",", scientific = FALSE))
  }
  
  ifelse(x >= 1e6, 
         paste0(round(x/1e6, 1), "M"),
         ifelse(x >= 1e3,
                paste0(round(x/1e3, 1), "K"),
                format(x, big.mark = ",", scientific = FALSE)))
}

# Format currency with appropriate abbreviations
format_currency <- function(x, abbreviate = TRUE) {
  formatted <- format_number(x, abbreviate)
  paste0("$", formatted)
}

# Get time period label for display
get_time_period_label <- function(timeframe = "fiscal") {
  if (timeframe == "calendar") {
    "Calendar Year"
  } else {
    "Fiscal Year"
  }
}

# ========== DOWNLOAD HANDLERS ==========

create_download_handlers <- function(filename_base, data_reactive, session) {
  list(
    csv = downloadHandler(
      filename = function() {
        paste0(filename_base, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(data_reactive(), file, row.names = FALSE)
      }
    ),
    
    excel = downloadHandler(
      filename = function() {
        paste0(filename_base, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(data_reactive(), file)
      }
    )
  )
} 