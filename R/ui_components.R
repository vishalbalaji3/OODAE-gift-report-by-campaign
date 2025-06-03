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

# ========== CARD PANEL COMPONENT ==========

create_card_panel <- function(title, content, status = "default", collapsible = FALSE, collapsed = FALSE) {
  # Status classes for different card types
  status_class <- switch(status,
    "primary" = "panel-primary",
    "success" = "panel-success", 
    "info" = "panel-info",
    "warning" = "panel-warning",
    "danger" = "panel-danger",
    "panel-default"
  )
  
  # Create collapsible attributes if needed
  collapse_attrs <- if (collapsible) {
    list(
      `data-toggle` = "collapse",
      `data-target` = paste0("#collapse-", gsub("[^a-zA-Z0-9]", "", title)),
      class = "panel-title"
    )
  } else {
    list(class = "panel-title")
  }
  
  panel_body_class <- if (collapsible) {
    if (collapsed) "panel-collapse collapse" else "panel-collapse collapse in"
  } else {
    "panel-body"
  }
  
  div(class = paste("panel", status_class),
    div(class = "panel-heading",
      if (collapsible) {
        tags$a(href = "#", do.call(h4, c(list(title), collapse_attrs)))
      } else {
        h4(title, class = "panel-title")
      }
    ),
    div(class = panel_body_class,
        id = if (collapsible) paste0("collapse-", gsub("[^a-zA-Z0-9]", "", title)) else NULL,
        content
    )
  )
}

# ========== INFO BOX COMPONENT ==========

create_info_box <- function(title, value, icon = NULL, color = "primary", width = 3) {
  color_class <- switch(color,
    "primary" = "text-primary",
    "success" = "text-success",
    "info" = "text-info", 
    "warning" = "text-warning",
    "danger" = "text-danger",
    "text-primary"
  )
  
  column(width,
    div(class = "info-box text-center",
        style = "padding: 20px; margin-bottom: 20px; border: 1px solid #ddd; border-radius: 4px;",
        if (!is.null(icon)) {
          div(icon(icon, class = paste(color_class, "fa-2x")), 
              style = "margin-bottom: 10px;")
        },
        h3(value, class = color_class, style = "margin: 10px 0;"),
        p(title, style = "margin: 0; color: #666;")
    )
  )
}

# ========== STATS ROW COMPONENT ==========

create_stats_row <- function(stats_list) {
  # stats_list should be a list with elements: title, value, color, icon (optional)
  fluidRow(
    lapply(stats_list, function(stat) {
      create_info_box(
        title = stat$title,
        value = stat$value,
        icon = stat$icon,
        color = stat$color %||% "primary",
        width = 12 / length(stats_list)
      )
    })
  )
}

# ========== DOWNLOAD BUTTONS COMPONENT ==========

create_download_buttons <- function(ns, additional_buttons = NULL) {
  buttons <- list(
    downloadButton(ns("download_csv"), "Download CSV", class = "btn btn-primary btn-sm"),
    " ",
    downloadButton(ns("download_excel"), "Download Excel", class = "btn btn-success btn-sm")
  )
  
  # Add any additional buttons
  if (!is.null(additional_buttons)) {
    buttons <- c(buttons, " ", additional_buttons)
  }
  
  fluidRow(
    column(12,
           div(class = "text-right download-buttons", 
               style = "margin-top: 15px; margin-bottom: 10px;",
               do.call(tagList, buttons)
           )
    )
  )
}

# ========== DATA TABLE COMPONENT ==========

create_datatable <- function(data, currency_cols = NULL, scroll_x = TRUE, page_length = 25, 
                           dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')) {

  # Default options
  dt_options <- list(
    pageLength = page_length,
    scrollX = scroll_x,
    autoWidth = TRUE,
    dom = dom,
    buttons = buttons,
    columnDefs = list(
      list(className = 'dt-right', targets = currency_cols)
    ),
    language = list(
      search = "Search:",
      lengthMenu = "Show _MENU_ entries",
      info = "Showing _START_ to _END_ of _TOTAL_ entries",
      paginate = list(
        first = "First",
        last = "Last", 
        "next" = "Next",
        previous = "Previous"
      )
    )
  )

  # Format currency columns if specified
  if (!is.null(currency_cols) && length(currency_cols) > 0) {
    datatable(data,
              options = dt_options,
              rownames = FALSE,
              class = 'table-striped table-bordered') %>%
      formatCurrency(currency_cols + 1, # DT uses 0-based indexing, R uses 1-based
                     currency = "$",
                     digits = 0)
  } else {
    datatable(data,
              options = dt_options,
              rownames = FALSE,
              class = 'table-striped table-bordered')
  }
}

# ========== LOADING SPINNER COMPONENT ==========

create_loading_wrapper <- function(output_element, text = "Loading...", type = "dots") {
  withSpinner(
    output_element,
    type = switch(type,
      "dots" = 1,
      "circle" = 2, 
      "bounce" = 3,
      "pulse" = 4,
      1
    ),
    color = "#0073b7",
    size = 0.8
  )
}

# ========== TABLE WRAPPER COMPONENT ==========

create_table_wrapper <- function(table_output, title = NULL, subtitle = NULL) {
  content <- list()
  
  if (!is.null(title) || !is.null(subtitle)) {
    header <- div(class = "table-header", style = "margin-bottom: 15px;")
    if (!is.null(title)) {
      header <- tagAppendChild(header, h4(title, class = "table-title"))
    }
    if (!is.null(subtitle)) {
      header <- tagAppendChild(header, p(subtitle, class = "table-subtitle text-muted"))
    }
    content <- append(content, list(header))
  }
  
  content <- append(content, list(
    div(class = "table-responsive",
        create_loading_wrapper(table_output)
    )
  ))
  
  do.call(tagList, content)
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

# ========== ALERT COMPONENT ==========

create_alert <- function(message, type = "info", dismissible = TRUE) {
  type_class <- switch(type,
    "success" = "alert-success",
    "info" = "alert-info",
    "warning" = "alert-warning", 
    "danger" = "alert-danger",
    "alert-info"
  )
  
  alert_class <- if (dismissible) {
    paste("alert", type_class, "alert-dismissible")
  } else {
    paste("alert", type_class)
  }
  
  alert_content <- if (dismissible) {
    tagList(
      tags$button(type = "button", class = "close", `data-dismiss` = "alert",
                  span(HTML("&times;"))),
      message
    )
  } else {
    message
  }
  
  div(class = alert_class, role = "alert", alert_content)
}

# ========== PROGRESS BAR COMPONENT ==========

create_progress_bar <- function(value, max_value = 100, label = NULL, color = "primary") {
  percentage <- round((value / max_value) * 100, 1)
  
  color_class <- switch(color,
    "success" = "progress-bar-success",
    "info" = "progress-bar-info",
    "warning" = "progress-bar-warning",
    "danger" = "progress-bar-danger",
    "progress-bar-primary"
  )
  
  div(class = "progress",
      div(class = paste("progress-bar", color_class),
          role = "progressbar",
          style = paste0("width: ", percentage, "%"),
          `aria-valuenow` = value,
          `aria-valuemin` = 0,
          `aria-valuemax` = max_value,
          if (!is.null(label)) label else paste0(percentage, "%")
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

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ========== RESPONSIVE UTILITIES ==========

# Create responsive column that adjusts based on screen size
create_responsive_column <- function(content, xs = 12, sm = NULL, md = NULL, lg = NULL) {
  classes <- paste0("col-xs-", xs)
  if (!is.null(sm)) classes <- paste(classes, paste0("col-sm-", sm))
  if (!is.null(md)) classes <- paste(classes, paste0("col-md-", md))
  if (!is.null(lg)) classes <- paste(classes, paste0("col-lg-", lg))
  
  div(class = classes, content)
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

# ========== MODULE TEMPLATE HELPERS ==========

# Standard module UI wrapper
create_module_ui <- function(title, summary_output = NULL, table_output, 
                           download_ns, additional_content = NULL,
                           show_summary = TRUE) {
  content <- list()
  
  # Add title panel if summary is shown
  if (show_summary && !is.null(summary_output)) {
    content <- append(content, list(
      create_card_panel(
        title = title,
        content = summary_output,  
        status = "info"
      )
    ))
  }
  
  # Add any additional content
  if (!is.null(additional_content)) {
    content <- append(content, additional_content)
  }
  
  # Add table wrapper
  content <- append(content, list(
    create_table_wrapper(table_output)
  ))
  
  # Add download buttons
  content <- append(content, list(
    create_download_buttons(download_ns)
  ))
  
  do.call(tagList, content)
}
