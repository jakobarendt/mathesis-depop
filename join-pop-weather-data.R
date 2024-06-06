### This R script joins population and weather data ###
### It also further aggregates the weather data to temporally correspond to the ###
### weather data ###




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
       The script then needs username and API key saved, which can with the\n
       following command that should be ran before the prep-weather-data.R script.\n
       user <- wf_set_key(service = "cds")')
} else {
  weather <- rast('data/temp/weather.nc')
}



# Verify that coordinate reference systems of all data correspond ---------
# st_read() for shape files
# crs() |> cat() for raster data


# Spatial join of population and weather data -----------------------------

# Define function for spatial join/overlay of the grid data onto the shape files
spatial_join <- function(raster, sf) {
  joined_data <- exact_extract(raster, sf, fun = mean)
  return(joined_data)
}

panel_data <- exact_extract(weather,
                            population,
                            fun = 'mean',
                            append_cols = names(population)[-(14:17)],
                            progress = TRUE)
panel_data <- panel_data |> as_tibble()


# panel_data <-