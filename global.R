# Load required libraries
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(shinythemes)
library(shinycssloaders)
library(writexl)
library(scales)

# Source data preparation
source("data_prep.R")

# Source helper functions and modules
source("R/helpers.R")
source("R/data_processing.R")
source("R/shared_processing.R")

# Source module files
modules_path <- "R/modules"
module_files <- list.files(path = modules_path, pattern = "\\.R$", full.names = TRUE)
sapply(module_files, source)