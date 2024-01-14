### This R script centrally loads packages, population data, its georeferences and urbanism characteristsics for the different chapters. ###
### It also cleans and combines the different population data attributes such that it can mapped onto the climate data and used for further causal econometric analysis. ###


# Packages ----------------------------------------------------------------

require(tidyverse)
require(sf)


# Historic LAU population data --------------------------------------------

dir.create("data")
hist_pop <- readxl::read_xlsx(path = 'data/LAU2_REFERENCE_DATES_POPL.xlsx')


# Geographic shapefiles for historic LAU population data ------------------

dir.create("data/shapefiles")

# uncomment depending on whether you want to load shapefiles directly from the
# Eurostat database (or your local drive if cached there),
# or the cloud storage for this project

# 1. Load shapefiles from Eurostat via its API
shapes_files <- giscoR::gisco_get_lau(year = "2012", cache_dir = 'data/shapefiles', verbose = TRUE)
# shapefiles are loaded by default for the WGS84 (EPSG 4326) map projection
# this projection corresponds to the projection of the weather grid data

# 2. Load shapefiles from this project's cloud storage
# !! still missing !!

# Special case: Greece
gr <- read_sf('data/shapefiles/gr')

# Special case: Ireland
ie <- read_sf('data/shapefiles/ie')

# Special case: Turkey
tr <- read_sf('data/shapefiles/tr')
# !! also adjust this for dowloads from Google Drive !!


# Joining historic LAU population with geolocation data -------------------

# Special case: Greece
gr <- gr |> select(NSI_CODE, geometry)
