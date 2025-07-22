### This R script prepares the weather grid data ###



# Packages ----------------------------------------------------------------

require(ecmwfr)
require(terra)
require(tidyverse)



# Resample weather data online and download -------------------------------

# set a key to the keychain interactively
user <- wf_set_key(service = "cds")

# Down-sample daily mean temperatures to yearly average on CDS servers
# and download them
# see https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_workflow_vignette.html
# for detailed explanations
code_mean_temp_resample <- readLines('resample-mean-temp.py') |>
  paste0(collapse = "\n")
request_mean_temp <- list(
  code = code_mean_temp_resample,
  kwargs = list(
    placeholder_var = 1
  ),
  workflow_name = "download_resample_mean_temp_grid",
  target = "mean-temp-resampled.nc"
)
file_mean_temp_resampled <- wf_request(request_mean_temp, user, path = 'data/weather', time_out = 7200)

# Down-sample daily precipitation amounts to yearly sums on CDS servers
# and download them
# see https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_workflow_vignette.html
# for detailed explanations
code_precipitation_resample <- readLines('resample-precipitation.py') |>
  paste0(collapse = "\n")
request_precipitation <- list(
  code = code_precipitation_resample,
  kwargs = list(
    placeholder_var = 1
  ),
  workflow_name = "download_resample_precipitation_grid",
  target = "precipitation-resampled.nc"
)
file_precipitation_resampled <- wf_request(request_precipitation, user, path = 'data/weather', time_out = 7200)



# Join all raster data ----------------------------------------------------

weather <- c(
  rast(file_mean_temp_resampled),
  rast(file_precipitation_resampled)
)
dir.create("data/temp")
writeRaster(weather, 'data/temp/weather.tif')