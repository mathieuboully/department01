# -----------------------------------------------------------------------------
# File name : global.R
# Description : main file
# Author : Boully Mathieu
# Date : 2025-11-05
# -----------------------------------------------------------------------------

# Load all R packages
library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(lubridate)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(sf)
library(tidyr)
library(grDevices)
library(DT)
library(crosstalk)
library(jsonlite)
library(scales)
library(RColorBrewer)
library(FactoMineR)
library(missMDA)

# Workspace cleaning
rm(list = ls())

# Change the default size of files that can be imported, and other parameters
options(shiny.maxRequestSize = 100 * 1024^2,
        dplyr.summarise.inform = FALSE,
        stringsAsFactors = FALSE)

# Set the system locale to French for date, number and text formatting
Sys.setlocale(category = "LC_ALL", locale = "French")

# Load functions

# Modules

# Capturing R dependencies with renv
rsconnect::writeManifest()