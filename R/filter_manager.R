# Centralized Filter Management System
# This eliminates duplication between data tables and visualizations filtering

create_filter_manager <- function(full_data) {
  
  # Validation
  if (!exists("FullData") && missing(full_data)) {
    stop("FullData not found. Please ensure data_prep.R runs successfully.")
  }
  
  data_source <- if (missing(full_data)) FullData else full_data
  
  # Create the manager object with methods
  manager <- list(
    
    # ========== DATA SOURCE FUNCTIONS ==========
    
    get_campaigns = function() {
      unique(data_source$`Campaign ID`)
    },
    
    get_gift_types = function() {
      levels(data_source$`Gift Type`)
    },
    
    get_available_years = function(timeframe = "fiscal") {
      if (timeframe == "calendar") {
        sort(unique(format(as.Date(data_source$`Gift Date`), "%Y")))
      } else {
        sort(unique(data_source$`Fiscal Year`))
      }
    },
    
    # ========== CORE FILTERING FUNCTION ==========
    
    apply_filters = function(campaign = NULL, gift_types = NULL, years = NULL, timeframe = "fiscal") {
      data <- data_source
      
      # Apply campaign filter
      if (!is.null(campaign)) {
        data <- data %>% filter(`Campaign ID` == campaign)
      }
      
      # Apply gift type filter
      if (length(gift_types) > 0) {
        data <- data %>% filter(`Gift Type` %in% gift_types)
      }
      
      # Apply year filter based on timeframe
      if (length(years) > 0) {
        if (timeframe == "calendar") {
          data <- data %>% 
            filter(format(as.Date(`Gift Date`), "%Y") %in% years)
        } else {
          data <- data %>% 
            filter(`Fiscal Year` %in% years)
        }
      }
      
      data
    },
    
    # ========== DERIVED DATA FUNCTIONS ==========
    
    get_fiscal_years_from_data = function(filtered_data, timeframe = "fiscal") {
      if (nrow(filtered_data) == 0) {
        return(character(0))
      }
      
      if (timeframe == "calendar") {
        sort(unique(format(as.Date(filtered_data$`Gift Date`), "%Y")))
      } else {
        sort(unique(filtered_data$`Fiscal Year`))
      }
    },
    
    calculate_summary_stats = function(filtered_data) {
      if (nrow(filtered_data) == 0) {
        return(list(
          total_gifts = 0,
          total_amount = 0,
          total_constituents = 0,
          avg_gift_size = 0
        ))
      }
      
      list(
        total_gifts = nrow(filtered_data),
        total_amount = sum(filtered_data$`Fund Split Amount`, na.rm = TRUE),
        total_constituents = n_distinct(filtered_data$`Constituent ID`),
        avg_gift_size = sum(filtered_data$`Fund Split Amount`, na.rm = TRUE) / nrow(filtered_data)
      )
    },
    
    # ========== UI HELPER FUNCTIONS ==========
    
    create_year_label = function(timeframe = "fiscal") {
      if (timeframe == "calendar") {
        "Calendar Year:"
      } else {
        "Fiscal Year:"
      }
    }
  )
  
  # Add reactive wrapper functions that properly capture the manager methods
  manager$create_reactive_filtered_data = function(input, context = "tables") {
    reactive({
      # Determine which input set to use based on context
      if (context == "viz" || context == "visualizations") {
        campaign <- input$vizFilter_campaign
        gift_types <- input$vizFilter_giftType
        years <- input$vizFilter_year
        timeframe <- input$vizFilter_timeframe
      } else {
        campaign <- input$dataTabFilter_campaign
        gift_types <- input$dataTabFilter_giftType
        years <- input$dataTabFilter_year
        timeframe <- input$dataTabFilter_timeframe
      }
      
      manager$apply_filters(campaign, gift_types, years, timeframe)
    })
  }
  
  manager$create_reactive_available_years = function(input, context = "tables") {
    reactive({
      timeframe <- if (context == "viz" || context == "visualizations") {
        input$vizFilter_timeframe
      } else {
        input$dataTabFilter_timeframe
      }
      
      manager$get_available_years(timeframe)
    })
  }
  
  manager$create_reactive_fiscal_years = function(filtered_data_reactive, input, context = "tables") {
    reactive({
      timeframe <- if (context == "viz" || context == "visualizations") {
        input$vizFilter_timeframe
      } else {
        input$dataTabFilter_timeframe
      }
      
      manager$get_fiscal_years_from_data(filtered_data_reactive(), timeframe)
    })
  }
  
  manager$create_reactive_summary_stats = function(filtered_data_reactive) {
    reactive({
      manager$calculate_summary_stats(filtered_data_reactive())
    })
  }
  
  return(manager)
}

# ========== CONVENIENCE FUNCTIONS ==========

# Create a complete set of reactive expressions for a given context
create_filter_context = function(filter_manager, input, context = "tables") {
  list(
    filtered_data = filter_manager$create_reactive_filtered_data(input, context),
    available_years = filter_manager$create_reactive_available_years(input, context),
    fiscal_years = filter_manager$create_reactive_fiscal_years(
      filter_manager$create_reactive_filtered_data(input, context), 
      input, 
      context
    ),
    summary_stats = filter_manager$create_reactive_summary_stats(
      filter_manager$create_reactive_filtered_data(input, context)
    ),
    timeframe = reactive({
      if (context == "viz" || context == "visualizations") {
        input$vizFilter_timeframe
      } else {
        input$dataTabFilter_timeframe
      }
    })
  )
} 