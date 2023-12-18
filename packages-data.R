### This R script centrally loads packages and data needed in the different chapters ###


# Packages #

require(tidyverse)
require(sf)


# Historic LAU Population Data #

dir.create("data")
hist_pop <- readxl::read_xlsx(path = 'data/LAU2_REFERENCE_DATES_POPL.xlsx')


# Geographic shapefiles for historic LAU population data #

dir.create("data/shapefiles")

# uncomment depending on whether you want to load shapefiles directly from the
# Eurostat database (or your local drive if cached there),
# or the cloud storage for this project

# 1. Load shapefiles from Eurostat via its API
shape_files <- giscoR::gisco_get_lau(year = "2012", cache_dir = 'data/shapefiles', verbose = TRUE)

# 2. Load shapefiles from this project's cloud storage
# !! still missing !!