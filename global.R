# Core packages
library(shiny)
library(shinythemes)
library(shinycssloaders)

# Data manipulation
library(dplyr)
library(tidyr)
library(readr)

# Output and visualization
library(DT)
library(writexl)
library(plotly)     # Consider loading locally in visualization modules

# Source configuration file first
source("config.R")

# Source data preparation
source("data_prep.R")

# Source centralized filter manager
source("R/filter_manager.R")

# Source shared UI components
source("R/ui_components.R")

# Source helper functions
source("R/helpers.R")
source("R/data_processing.R")
source("R/shared_processing.R")

# Source server modules
server_module_files <- list.files(
  path = "R/server_modules",
  pattern = "\\.R$",
  full.names = TRUE
)
sapply(server_module_files, source)

# Source data table modules
data_table_files <- list.files(
  path = "R/modules/data_tables",
  pattern = "\\.R$",
  full.names = TRUE
)
sapply(data_table_files, source)

# Source visualization utility functions
viz_utils_path <- "R/modules/visualizations/viz_utils.R"
if (file.exists(viz_utils_path)) {
  source(viz_utils_path)
}

# Source visualization modules
viz_files <- list.files(
  path = "R/modules/visualizations",
  pattern = "\\.R$",
  full.names = TRUE
)
if (length(viz_files) > 0) {
  # Make sure viz_utils.R is sourced first if present
  viz_files <- viz_files[!grepl("viz_utils.R", viz_files)]
  sapply(viz_files, source)
}
