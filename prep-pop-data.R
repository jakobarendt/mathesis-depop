### This R script centrally loads packages, georeferences/shapefiles and corresponding population data for usage in the following steps of analysis and different chapters. ###
### It also cleans and combines the different population data attributes to prepare the georeferenced population data for mapping onto the climate data and its usage for further causal econometric analysis. ###



# Packages and directory --------------------------------------------------

require(tidyverse)
require(sf)
dir.create("data")



# Load geographic shapefiles for historic LAU population data -------------

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



# Load historic LAU population data ---------------------------------------

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

# !!add column that denotes which shapes version is actually used for each country!!



# Shapefiles: Streamline identifiers and reduce to columns needed ---------

# EuroGeographics LAU Shapefiles

## Streamline identifiers: Create new column that combines CNTR_CODE and LAU_ID
shapes_2011 <- shapes_2011 |>
  mutate(CNTR_LAU_ID = paste0(CNTR_CODE, LAU_ID))
shapes_2012 <- shapes_2012 |>
  mutate(CNTR_LAU_ID = paste0(CNTR_CODE, LAU_ID))
## Check: Do values in columns GISCO_ID and FID correspond? Yes, they do fully.
sum(shapes_2011$GISCO_ID != shapes_2011$FID)
sum(shapes_2012$GISCO_ID != shapes_2012$FID)
## Check: Does the newly created column CNTR_LAU_ID correspond to both of the
## columns investigated in the previous paragraph?
sum(shapes_2011$CNTR_LAU_ID != gsub("_", "", shapes_2011$GISCO_ID))
sum(shapes_2012$CNTR_LAU_ID != gsub("_", "", shapes_2012$GISCO_ID))
shapes_2012 |> filter(shapes_2012$CNTR_LAU_ID != gsub("_", "", shapes_2012$GISCO_ID))
### With the exception of Budapest (HU) in the shapes of 2012 (Eurogeographics v7.0),
### the newly created column corresponds to the other two.
### !!ADD ADDITIONAL COMMENT HOW THIS IS TAKEN CARE OF!!
## Reduce shapefiles to the columns needed
shapes_2011 <- shapes_2011 |>
  select(CNTR_LAU_ID, POP_2011, POP_DENS_2011, AREA_KM2, geometry) |>
  rename("AREA_KM2_2011" = AREA_KM2, "geometry_2011" = geometry)
shapes_2012 <- shapes_2012 |>
  select(CNTR_LAU_ID, POP_2012, POP_DENS_2012, AREA_KM2, geometry) |>
  rename("AREA_KM2_2012" = AREA_KM2, "geometry_2012" = geometry)

# Shapefiles delivered with data set: GR, IE, TR

shapes_gr <- shapes_gr |>
  mutate(CNTR_LAU_ID = paste0("EL", NSI_CODE)) |>
  select(CNTR_LAU_ID, SHAPE_Area, geometry) |>
  rename("SHAPE_AREA_EL" = SHAPE_Area, "geometry_EL" = geometry)
shapes_ie <- shapes_ie |>
  mutate(CNTR_LAU_ID = paste0("IE", MERG_COD)) |>
  select(CNTR_LAU_ID, Shape_Area, geometry) |>
  rename("SHAPE_AREA_EL" = Shape_Area, "geometry_IE" = geometry)
shapes_tr <- shapes_tr |>
  select(ICC_LAU_CO, SHAPE_AREA, geometry) |>
  rename("CNTR_LAU_ID" = ICC_LAU_CO, "SHAPE_AREA_TR" = SHAPE_AREA,
         "geometry_TR" = geometry)

# Check coordinate reference systems (CRS) of all shapefiles

st_crs(shapes_2011)
st_crs(shapes_2012)
st_crs(shapes_gr)
st_crs(shapes_ie)
st_crs(shapes_tr)
