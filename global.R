# Load required libraries
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(shinythemes)
library(shinycssloaders)
library(writexl)
library(scales)

library(plotly)       # For interactive plots
library(ggplot2)      # For static plots
library(rmarkdown)    # For report generation
library(knitr)        # For report formatting
library(gridExtra)    # For arranging multiple plots
library(htmlwidgets)  # For saving widgets
library(webshot2)     # For saving widgets as images

# Source configuration file first
source("config.R")

# Source data preparation
source("data_prep.R")

# Source helper functions
source("R/helpers.R")
source("R/data_processing.R")
source("R/shared_processing.R")

# Source data table modules
data_table_files <- list.files(path = "R/modules/data_tables", pattern = "\\.R$", full.names = TRUE)
sapply(data_table_files, source)

# Source visualization modules (this will be empty initially)
viz_files <- list.files(path = "R/modules/visualizations", pattern = "\\.R$", full.names = TRUE)
if(length(viz_files) > 0) {
  sapply(viz_files, source)
}