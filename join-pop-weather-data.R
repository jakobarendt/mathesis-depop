### This R script joins population and weather data ###




# Packages ----------------------------------------------------------------

require(exactextractr)
require(terra)
require(tidyverse)
require(parallel)
require(sf)
require(plm)
require(fixest)



# Load all different data types and ask to run scripts if not there -------

# Population data
if (!file.exists('data/temp/population.RData')) {
  stop('Please run prep-population-data.R before running this script')
} else {
  load('data/temp/population.RData')
}
# Weather data
if (!file.exists('data/temp/weather.nc')) {
  stop('Please run prep-weather-data.R before running this script.\n
       I recommend running it as a background job as it takes a long time.\n
       For download, a registration with the CDS service is necessary:\n
       https://cds.climate.copernicus.eu/user/register \n
       The script then needs username and API key saved on the machine, which\n
       can be done by running the following command before running the
       prep-weather-data.R script.\n
       user <- wf_set_key(service = "cds")')
} else {
  weather <- rast('data/temp/weather.nc')
}



# Verify that coordinate reference systems of all data correspond ---------
# st_read() for shape files
# crs() |> cat() for raster data


# Spatial join of population and weather data -----------------------------

joined_data <- exact_extract(weather,
                            population,
                            fun = 'mean',
                            append_cols = names(population)[-(15:17)],
                            progress = TRUE)
joined_data <- panel_data |> as_tibble()

# !! Add na.rm = true to fun = 'mean' ??
# Check whether the numbering in the weather variables is actually chronological



# Reformat table for panel estimations ------------------------------------

## Year mapping for renaming weather data columns with year
# year_mapping <- setNames(c("1961", "1971", "1981", "1991", "2001", "2011"), 1:6)
map_year <- function(string) {
  years <- seq(1961, 2011, 10)
  key <- str_extract(string, "\\d") |> as.numeric()
  variable_name <- str_extract(string, "^[^0-9]*")
  return(paste0(variable_name, years[key]))
}

panel_data <- joined_data |>
  select(CNTR_LAU_CODE, CNTR_CODE, ends_with("1_01_01"), starts_with("mean.")) |>
  rename_with(~ map_year(.), .cols = starts_with("mean.")) |>
  pivot_longer(
    cols = starts_with("POP_") | starts_with("mean."),
    names_to = c(".value", "YEAR"),
    names_sep = "_",
    # names_pattern = "(.*)_(.*)"
  )


panel_data_frame <- panel_data |>
  # mutate(CNTR_LAU_CODE = as.factor(CNTR_LAU_CODE), YEAR = as.factor(YEAR)) |>
  pdata.frame(index = c("CNTR_LAU_CODE", "YEAR"))
plm(POP ~ mean.mean.daily.mean.temperature + mean.sum.daily.precipitation.amount,
    data = panel_data_frame,
    effect = "individual") |>
  summary()

feols(POP ~ `mean.mean-daily-mean-temperature` + `mean.sum-daily-precipitation-amount` |
        CNTR_LAU_CODE + CNTR_CODE,
      data = panel_data) |>
  summary()

# !! Investigate warning message that is thrown when pivot_longer
# !! Investigate warning message that is thrown pdata.frame
# !! Check what is up with Poian, must be in data set twice

pop |>
  filter(is.na(POP_1961)) |>
  leaflet() |>
  addTiles(options = tileOptions(opacity = 0.2)) |>
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.7,
    fillColor = ~pal(POP_2011),
    label = ~ paste0(LAU_NAME, ": ", round(POP_2011, 4))
  ) |>
  addLegend(pal = pal, values = ~POP_2011, opacity = 1.0)


population |>
  select(CNTR_LAU_CODE, geometry) |>
  right_join(panel_data, by = c("CNTR_LAU_CODE"))

# panel_data <-