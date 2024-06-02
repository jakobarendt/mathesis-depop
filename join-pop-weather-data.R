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
if (!file.exists('data/temp/weather.tif')) {
  stop('Please run prep-weather-data.R before running this script.\n
       I recommend running it as a background job as it takes a long time.')
} else {
  weather <- rast('data/temp/weather.tif')
}



# Spatial join of population and weather data -----------------------------

# Define function for spatial join/overlay of the grid data onto the shape files
spatial_join <- function(raster, grid) {
  joined_data <- exact_extract(raster, grid, fun = mean)
  return(joined_data)
}



# panel_data <-