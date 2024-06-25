### This R script joins population and weather data ###




# Packages ----------------------------------------------------------------

require(exactextractr)
require(terra)
require(tidyverse)
require(parallel)
require(sf)




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



# Reformat table for panel estimations ------------------------------------

panel_data <- joined_data |>
  select(CNTR_LAU_CODE, ends_with("1_01_01"), starts_with("mean.")) |>
  pivot_longer(
    cols = starts_with("POP_") | starts_with("mean."),
    names_to = c(".value", "date"),
    names_sep = "_",
    # names_pattern = "(.*)_(.*)"
  )

  pivot_longer(starts_with("POP_"), names_to = "YEAR", names_prefix = "POP_",
               )



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