### This R script centrally loads packages, LAU georeferences/shapefiles and corresponding population data for usage in the following steps of analysis and different chapters. ###
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

# Load and transform separately delivered shapefiles: Greece, Ireland, Turkey
shapes_gr <- read_sf('data/shapefiles/gr')
shapes_ie <- read_sf('data/shapefiles/ie')
shapes_tr <- read_sf('data/shapefiles/tr')
#### !! also adjust these special cases for downloads from Google Drive !!

# Transform coordinates of shapefiles to correspond to the projection of the weather grid data
shapes_2011 <- shapes_2011 |> st_transform("OGC:CRS84")
shapes_2012 <- shapes_2012 |> st_transform("OGC:CRS84")
shapes_gr <- shapes_gr |> st_transform("OGC:CRS84")
shapes_ie <- shapes_ie |> st_transform("OGC:CRS84")
shapes_tr <- shapes_tr |> st_transform("OGC:CRS84")



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
## Remove data to free up work space; it is only needed for producing the table
rm(obs_laus_pop_orig, obs_shapes_2011, obs_shapes_2012)

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
### With the exception of Budapest (HU) in the shapefiles of 2012 (EuroGeographics v7.0),
### the newly created column corresponds to the other two.
## Correct CNTR_LAU_ID for Budapest in EuroGeographics v7.0 shapefile, such that
## it corresponds to the EuroGeographics v5.0 shapefile (and to the population data
## that is transformed later)
shapes_2012 <- shapes_2012 |>
  mutate(CNTR_LAU_ID = if_else(CNTR_LAU_ID == "HU13578", "HU1357", CNTR_LAU_ID))
## Reduce shapefiles to the columns needed
shapes_2011 <- shapes_2011 |>
  select(CNTR_LAU_ID, LAU_NAME, POP_2011, POP_DENS_2011, AREA_KM2, geometry) |>
  rename("AREA_KM2_2011" = AREA_KM2, "geometry_2011" = geometry)
shapes_2012 <- shapes_2012 |>
  select(CNTR_LAU_ID, LAU_NAME, POP_2012, POP_DENS_2012, AREA_KM2, geometry) |>
  rename("AREA_KM2_2012" = AREA_KM2, "geometry_2012" = geometry)

# Shapefiles delivered with data set: GR, IE, TR

shapes_gr <- shapes_gr |>
  mutate(CNTR_LAU_ID = paste0("EL", NSI_CODE)) |>
  select(CNTR_LAU_ID, SHAPE_Area, geometry) |>
  rename("SHAPE_AREA_EL" = SHAPE_Area, "geometry_EL" = geometry)
shapes_ie <- shapes_ie |>
  mutate(CNTR_LAU_ID = paste0("IE", MERG_COD)) |>
  select(CNTR_LAU_ID, Shape_Area, geometry) |>
  rename("SHAPE_AREA_IE" = Shape_Area, "geometry_IE" = geometry)
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



# Historic LAU population data: Correct minor peculiarities ---------------

# Check: Do values in CNTR_CODE correspond with the country-determiner in CNTR_LAU_CODE?
sum(pop_orig$CNTR_CODE == str_extract(pop_orig$CNTR_LAU_CODE, "^.{2}"), na.rm = TRUE)
nrow(pop_orig)
pop_orig |> filter(pop_orig$CNTR_CODE != str_extract(pop_orig$CNTR_LAU_CODE, "^.{2}")
                   | is.na(pop_orig$CNTR_CODE == str_extract(pop_orig$CNTR_LAU_CODE, "^.{2}")))
### Yes, they do correspond, with the exception of two French LAUs that do not
### have a CNTR_LAU_CODE at all.
## The two French LAUs without the CNTR_LAU_CODE identifier are removed from the data set
pop_orig <- pop_orig |> filter(!is.na(CNTR_LAU_CODE))

# Hungary: Aggregate (sum up) population figures of all Budapest districts to
# correspond to the city's single shape file
budapest <- pop_orig |>
  filter(str_starts(LAU_LABEL, "Budapest_"))
budap_aggreg <- budapest |>
  summarize(CNTR_CODE = "HU", CNTR_LAU_CODE = "HU1357", LAU_LABEL = "Budapest (aggreg.)",
            across(starts_with("POP_"), ~ sum(.x, na.rm = TRUE)))
pop_orig <- pop_orig |>
  bind_rows(budap_aggreg) |>
  filter(!CNTR_LAU_CODE %in% budapest$CNTR_LAU_CODE)
rm(budapest, budap_aggreg)
## All Budapest population figures are summed up to a single LAU and added to
## the population data set. The remaining disaggregated Budapest LAUs with no
## further use are removed.



# Join all shapefiles with population data --------------------------------

pop_all_shapes <- pop_orig |>
  left_join(select(shapes_2011, -LAU_NAME), by = join_by(CNTR_LAU_CODE == CNTR_LAU_ID)) |>
  left_join(select(shapes_2012, -LAU_NAME), by = join_by(CNTR_LAU_CODE == CNTR_LAU_ID)) |>
  left_join(shapes_gr, by = join_by(CNTR_LAU_CODE == CNTR_LAU_ID)) |>
  left_join(shapes_ie, by = join_by(CNTR_LAU_CODE == CNTR_LAU_ID)) |>
  left_join(shapes_tr, by = join_by(CNTR_LAU_CODE == CNTR_LAU_ID))

# Function for assigning EuroGeographics version or other shapefile version for
# each country of the LAUs
version_shape <- function(country) {
  case_when(
    country %in% c("PT", "SI") ~ "cannot join",
    country %in% c("EL", "IE", "TR") ~ "proprietary",
    country %in% c("CY", "HR", "LI", "LT", "LU", "LV", "UK") ~ "v7.0",
    .default = "v5.0" # AT, BE, BG, CH, CZ, DE, DK, EE, ES, FI, FR, HU, IS, IT, MK, MT, NL, NO, PL, RO, SE, SK
  )
}
# alternative function that determines the max between two columns and returns
# "v5.0" or "v7.0", depending on which column has the high number of non-NA values
version_shape_alt <- function(country, column1, column2) {
  case_when(
    country %in% c("PT", "SI") ~ "cannot join",
    country %in% c("EL", "IE", "TR") ~ "proprietary",
    country == "DE" ~ "v5.0",
    column1 < column2 ~ "v7.0",
    column1 >= column2 ~ "v5.0",
    .default = "cannot join"
  )
}
# Shapefile version with higher match rates are taken
# If match rates are equal, v5.0 is taken
# (DE is exception)

# Check: How do the two EuroGeographics shapefile versions compare to each other
# regarding the successful join to the population data?
table_match_rates <- pop_all_shapes |>
  group_by(CNTR_CODE) |>
  summarise(HIST_POP_OBS_LAUS = n(),
            SHAPES_2011_JOINED = sum(!st_is_empty(geometry_2011)),
            SHAPES_2012_JOINED = sum(!st_is_empty(geometry_2012))) |>
  mutate(VERS_SHAPEFILE = version_shape(CNTR_CODE)) |>
  mutate(VERS_SHAPEFILE_ALT = version_shape_alt(CNTR_CODE, SHAPES_2011_JOINED, SHAPES_2012_JOINED))