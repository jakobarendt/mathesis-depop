### This R script centrally loads packages, georeferences/shapefiles and corresponding population data for usage in the following steps of analysis and different chapters. ###
### It also cleans and combines the different population data attributes to prepare the georeferenced population data for mapping onto the climate data and its usage for further causal econometric analysis. ###



# Packages and directory --------------------------------------------------

require(tidyverse)
require(sf)
dir.create("data")



# Geographic shapefiles for historic LAU population data ------------------

dir.create("data/shapefiles")

# Comment/uncomment depending on whether you want to load shapefiles directly from:
# 1. The Eurostat database or your local drive if cached there (default), or
# 2. The cloud storage for this project (in case the data is not retrievable via option 1.)

# 1. Load shapefiles from Eurostat via its API
shapes_2011 <- giscoR::gisco_get_lau(year = "2011", cache_dir = 'data/shapefiles', verbose = TRUE)
shapes_2012 <- giscoR::gisco_get_lau(year = "2012", cache_dir = 'data/shapefiles', verbose = TRUE)
# shapefiles are loaded by default for the WGS84 (EPSG 4326) map projection

# 2. Load shapefiles from this project's cloud storage
#### !! still missing !!

# Transform coordinates of shapefiles to correspond to the projection of the weather grid data
shapes_2011 <- shapes_2011 |> st_transform("OGC:CRS84")
shapes_2012 <- shapes_2012 |> st_transform("OGC:CRS84")

# Load and transform separately delivered shapefiles: Greece, Ireland, Turkey
shapes_gr <- read_sf('data/shapefiles/gr') |> st_transform("OGC:CRS84")
shapes_ie <- read_sf('data/shapefiles/ie') |> st_transform("OGC:CRS84")
shapes_tr <- read_sf('data/shapefiles/tr') |> st_transform("OGC:CRS84")
#### !! also adjust these special cases for downloads from Google Drive !!



# Historic LAU population data --------------------------------------------

pop_orig <- readxl::read_xlsx(path = 'data/LAU2_REFERENCE_DATES_POPL.xlsx')



# Population figures and shapes per country -------------------------------

## LAUs with population figures per country
obs_laus_pop_orig <- pop_orig |>
  group_by(CNTR_CODE) |> summarise(HIST_POP_OBS_LAUS = n())
## Shapes of 2011 (Eurogeographics v5.0) per country
obs_shapes_2011 <- shapes_2011 |>
  as_tibble() |> select(-geometry) |> group_by(CNTR_CODE) |> summarise(SHAPES_2011_OBS = n())
## Shapes of 2012 (Eurogeographics v7.0) per country
obs_shapes_2012 <- shapes_2012 |>
  as_tibble() |> select(-geometry) |> group_by(CNTR_CODE) |> summarise(SHAPES_2012_OBS = n())
## Join numbers of shapes to population figures table by CNTR_CODE and save as Excel file
table_obs <- obs_laus_pop_orig |>
  left_join(obs_shapes_2011) |> left_join(obs_shapes_2012)
# table_obs |> writexl::write_xlsx("obs-pop-figures-shapes.xlsx")
### Save as Excel file is commented out so as to not overwrite the annotated file