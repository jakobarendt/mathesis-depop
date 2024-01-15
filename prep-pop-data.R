### This R script centrally loads packages, population data, its georeferences and urbanism characteristsics for the different chapters. ###
### It also cleans and combines the different population data attributes such that it can mapped onto the climate data and used for further causal econometric analysis. ###


# Packages ----------------------------------------------------------------

require(tidyverse)
require(s2)
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



# Special cases: Joining historic LAU population with geolocation  --------

# Greece
gr <- gr |> select(NSI_CODE, SHAPE_Area, geometry) |>
  mutate(NSI_CODE = paste0("EL", NSI_CODE)) |>
  rename("GR__SHAPE_Area" = SHAPE_Area)
gr <- gr |> st_transform("OGC:CRS84")
hist_pop <- left_join(hist_pop, gr, by = join_by(CNTR_LAU_CODE == NSI_CODE))

# Special case: Ireland
ie <- ie |> select(MERG_COD, Shape_Area, geometry) |>
  mutate(MERG_COD = paste0("IE", MERG_COD)) |>
  rename("IE__SHAPE_Area" = Shape_Area)
ie <- ie |> st_transform("OGC:CRS84")
hist_pop <- left_join(hist_pop, ie, by = join_by(CNTR_LAU_CODE == MERG_COD))

# Special case: Turkey
tr <- tr |> select(ICC_LAU_CO, SHAPE_AREA, geometry) |>
  rename("TR__SHAPE_Area" = SHAPE_AREA)
tr <- tr |> st_transform("OGC:CRS84")
hist_pop <- left_join(hist_pop, tr, by = join_by(CNTR_LAU_CODE == ICC_LAU_CO))

# Consolidate all three geometry columns to one
hist_pop <- hist_pop |>
  mutate(geometry = if_else(st_is_empty(geometry.x), geometry, geometry.x)) |>
  mutate(geometry = if_else(st_is_empty(geometry.y), geometry, geometry.y)) |>
  select(-geometry.x, -geometry.y)



# Clean and transform shapefile collection of remaining countries ---------
shapes_files <- shapes_files |>
  mutate(CNTR_LAU_ID = paste0(CNTR_CODE, LAU_ID)) |>
  filter(!CNTR_CODE %in% c("LT", "PT", "SI", "EL", "IE", "TR")) |>
  select(CNTR_LAU_ID, POP_2012, POP_DENS_2012, AREA_KM2, geometry)
# reduces shapa file collection data set to only the needed columns, and excludes
# the countries that are special cases (see above) or for which the LAU1 level
# of Eurogeographis7.0 is not retrievable via the GISCO API
shapes_files <- shapes_files |> st_transform("OGC:CRS84")
hist_pop <- left_join(hist_pop, shapes_files, by = join_by(CNTR_LAU_CODE == CNTR_LAU_ID)) |>
  mutate(geometry = if_else(st_is_empty(geometry.x), geometry.y, geometry.x)) |>
  select(-geometry.x, geometry.y) |> View()
# how do I maintain that it is a sf collection even with a left_join? Will the CRS characteristics stay?

# Join shapefiles remaining majority of countries
# with left_join, but also reduce columns in shapes_files beforehand
# then also consolidate all geometry columns into one column
# and work through NA's