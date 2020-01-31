
# Purpose: Setup for PHIA anlayses
# Author: Tim Essam, Ph.D | USAID OHA SIEI
# Date: 2020_01_29
# Audience: OHA

# Load libraries and data -------------------------------------------------
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl", "measurements", "pdftools", "purrr", "styler", "scales", "llamar", "haven", "sjlabelled", "vtable", "sjmisc", "survey", "data.table", "lemon", "widyr", "RColorBrewer", "readxl", "tidylog")

# Create folders for project (if they do not exist)
folder_list <- list("Data", "Images", "Scripts", "Dataout", "GIS", "Documents", "Graphics")
map(folder_list, ~dir.create(.))

datapath <- "Data"
dataout <- "Dataout"
gispath <- "GIS"
graphpath <- "Graphics"
imagepath <- "Images"
rpath <- "Scripts"
