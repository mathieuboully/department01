# -----------------------------------------------------------------------------
# File name : global.R
# Description : main file
# Author : Boully Mathieu
# Date : 2025-11-05
# -----------------------------------------------------------------------------

# Workspace cleaning
rm(list = ls())

# Loading the workspace path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shiny_path <- getwd()

# Load constant variables
source("./config/constants.R")

# Change the default size of files that can be imported
options(shiny.maxRequestSize=100*1024^2)

# Load functions

# Modules

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

# Capturing R dependencies with renv
rsconnect::writeManifest()

# The information in this document is CFM Proprietary Information and is disclosed in confidence. 
# It is the property of CFM International and its parent companies, and shall not be used, disclosed 
# to others or reproduced without the express written consent of CFM. If consent is given for reproduction 
# in whole or in part, this notice shall appear in any such reproduction in whole or in part. 
# The information contained in this document may also be controlled by the U.S. and French export 
# control laws. Unauthorized export or re-export is prohibited.
