# Gift Size Distribution Module

# UI Function
giftDistUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12, 
             div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                 h4("Gift Count by Range"),
                 withSpinner(DTOutput(ns("giftCountTable")))
             )
      )
    ),
    hr(),
    fluidRow(
      column(12, 
             div(style = "background-color: #f7f7f7; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                 h4("Gift Amount by Range"),
                 withSpinner(DTOutput(ns("giftAmountTable")))
             )
      )
    ),
    fluidRow(
      column(12,
             div(style = "margin-top: 20px;"),
             downloadButton(ns("download_csv"), "Download Full CSV"),
             downloadButton(ns("download_excel"), "Download Full Excel")
      )
    )
  )
}

# Server Function
giftDistServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for processed data
    processed_data <- reactive({
      process_gift_distribution_data(filtered_data())
    })
    
    # Gift Count Table renderer
    output$giftCountTable <- renderDT({
      data <- processed_data()
      
      # Get column names for gift counts
      gift_count_cols <- grep("^Number_of_Gifts_", names(data), value = TRUE)
      
      # Create a display data frame with just the count columns
      display_data <- data %>%
        select(Gift_Range, all_of(gift_count_cols), Total_Gifts)
      
      # Rename columns to remove prefixes and use cleaner names
      new_names <- names(display_data)
      
      # Clean up gift count column names
      for (col in gift_count_cols) {
        year <- gsub("Number_of_Gifts_", "", col)
        new_col_name <- year
        new_names[which(names(display_data) == col)] <- new_col_name
      }
      
      # Rename total column
      new_names[which(names(display_data) == "Total_Gifts")] <- "Total"
      
      # Apply new column names
      names(display_data) <- new_names
      
      # Determine which columns should be right-aligned (numbers)
      number_cols <- which(names(display_data) != "Gift_Range") - 1
      
      # Create the datatable with appropriate formatting
      datatable(
        display_data,
        caption = "Number of Gifts by Gift Range",
        options = list(
          pageLength = 15,  # Show all gift ranges on one page
          scrollX = TRUE,
          autoWidth = FALSE,
          dom = 'lfrtip',
          columnDefs = list(
            list(targets = number_cols, className = 'dt-right')
          ),
          language = list(search = "Search:")
        ),
        class = 'cell-border stripe',
        filter = 'none',
        selection = 'none'
      )
    })
    
    # Gift Amount Table renderer
    output$giftAmountTable <- renderDT({
      data <- processed_data()
      
      # Get column names for amounts
      amount_cols <- grep("^Total_Amount_", names(data), value = TRUE)
      
      # Create a display data frame with just the amount columns
      display_data <- data %>%
        select(Gift_Range, all_of(amount_cols), Total_Amount)
      
      # Format currency values
      for (col in c(amount_cols, "Total_Amount")) {
        display_data[[col]] <- format_currency(display_data[[col]])
      }
      
      # Rename columns to remove prefixes and use cleaner names
      new_names <- names(display_data)
      
      # Clean up amount column names
      for (col in amount_cols) {
        year <- gsub("Total_Amount_", "", col)
        new_col_name <- year
        new_names[which(names(display_data) == col)] <- new_col_name
      }
      
      # Rename total column
      new_names[which(names(display_data) == "Total_Amount")] <- "Total"
      
      # Apply new column names
      names(display_data) <- new_names
      
      # All columns except Gift_Range should be right-aligned
      currency_cols <- which(names(display_data) != "Gift_Range") - 1
      
      # Create the data table with appropriate formatting
      datatable(
        display_data,
        caption = "Gift Amount by Gift Range",
        options = list(
          pageLength = 15,  # Show all gift ranges on one page
          scrollX = TRUE,
          autoWidth = FALSE,
          dom = 'lfrtip',
          columnDefs = list(
            list(targets = currency_cols, className = 'dt-right')
          ),
          language = list(search = "Search:")
        ),
        class = 'cell-border stripe',
        filter = 'none',
        selection = 'none'
      )
    })
    
    # Create download handlers
    downloads <- create_download_handlers("gift_distribution_data", processed_data, session)
    
    # Assign download handlers
    output$download_csv <- downloads$csv
    output$download_excel <- downloads$excel
  })
}