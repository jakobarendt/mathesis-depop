### This R script prepares the weather grid data ###



# Packages and settings ---------------------------------------------------

require(ecmwfr)
require(terra)
require(tidyverse)



# Download weather data ---------------------------------------------------

# set a key to the keychain interactively
# (commented out, otherwise script won't run as background job)
# user <- wf_set_key(service = "cds")

# Set specifications for grid data set and download
request <- list(
  dataset_short_name = "insitu-gridded-observations-europe",
  product_type = "ensemble_mean",
  variable = c("mean_temperature", "precipitation_amount"),
  grid_resolution = "0.1deg",
  period = "full_period",
  version = "29.0e",
  format = "zip",
  target = "mean-temp-precipitation.zip"
)
zipfile_mean_temp_precipitation <- wf_request(request, user, path = 'data/weather', time_out = 7200)

# Unzip downloaded ZIP file
zipfile_mean_temp_precipitation |>
  unzip(overwrite = FALSE, junkpaths = TRUE, exdir = 'data/weather')



# Verify coordinate reference systems of downloaded raster data -----------

# Daily mean temperature
rast("data/weather/tg_ens_mean_0.1deg_reg_v29.0e.nc") |>
  crs() |>
  cat()

# Daily precipitation amount
rast("data/weather/rr_ens_mean_0.1deg_reg_v29.0e.nc") |>
  crs() |>
  cat()



# Load, resample to yearly values, and combine raster data ----------------

# Resampling for all variables is split into multiple steps to reduce
# computational load

# Daily mean temperature - first half of full time period (01/01/1951-31/12/1980):
# Load daily mean temperature raster data into workspace
mean_temperature <- rast("data/weather/tg_ens_mean_0.1deg_reg_v29.0e.nc")
# Subset to first half of full time period
mean_temperature <- mean_temperature |>
  subset(time(mean_temperature) >= as.Date("1951-01-01") &
           time(mean_temperature) <= as.Date("1980-12-31"))
# Resample the daily mean temperature: Calculate its yearly mean
mean_temperature <- mean_temperature |>
  tapp(index = "years", fun = mean, na.rm = TRUE)
# Save resampled grid to hard drive and delete from R workspace to free up space
mean_temperature |>
  writeCDF(filename = "data/temp/mean-temperature-19510101-19801231.nc",
           varname = "t", longname = "year average of daily mean temperatures",
           unit = "Celsius",
           overwrite = TRUE)
rm(mean_temperature)

# Daily mean temperature - second half of full time period (01/01/1981-31/12/2010):
# Load daily mean temperature raster data into workspace
mean_temperature <- rast("data/weather/tg_ens_mean_0.1deg_reg_v29.0e.nc")
# Subset to second half of full time period
mean_temperature <- mean_temperature |>
  subset(time(mean_temperature) >= as.Date("1981-01-01") &
           time(mean_temperature) <= as.Date("2010-12-31"))
# Resample the daily mean temperature: Calculate its yearly mean
mean_temperature <- mean_temperature |>
  tapp(index = "years", fun = mean, na.rm = TRUE)
# Save resampled grid to hard drive and delete from R workspace to free up space
mean_temperature |>
  writeCDF(filename = "data/temp/mean-temperature-19810101-20101231.nc",
           varname = "t", longname = "year average of daily mean temperatures",
           unit = "Celsius",
           overwrite = TRUE)
rm(mean_temperature)


# Daily precipitation amount - first half of full time period (01/01/1951-31/12/1980):
# Load daily precipitation amount raster data into workspace
precipitation_amount <- rast("data/weather/rr_ens_mean_0.1deg_reg_v29.0e.nc")
# Subset to first half of full time period
precipitation_amount <- precipitation_amount |>
  subset(time(precipitation_amount) >= as.Date("1951-01-01") &
           time(precipitation_amount) <= as.Date("1980-12-31"))
# Resample the daily precipitation amount: Calculate its yearly sum
precipitation_amount <- precipitation_amount |>
  tapp(index = "years", fun = sum, na.rm = TRUE)
# Save resampled grid to hard drive and delete from R workspace to free up space
precipitation_amount |>
  writeCDF(filename = "data/temp/precipitation-amount-19510101-19801231.nc",
           varname = "r", longname = "year sum of daily precipitation sums",
           unit = "mm",
           overwrite = TRUE)
rm(precipitation_amount)

# Daily precipitation amount - second half of full time period (01/01/1981-31/12/2010):
# Load daily precipitation amount raster data into workspace
precipitation_amount <- rast("data/weather/rr_ens_mean_0.1deg_reg_v29.0e.nc")
# Subset to second half of full time period
precipitation_amount <- precipitation_amount |>
  subset(time(precipitation_amount) >= as.Date("1981-01-01") &
           time(precipitation_amount) <= as.Date("2010-12-31"))
# Resample the daily precipitation amount: Calculate its yearly sum
precipitation_amount <- precipitation_amount |>
  tapp(index = "years", fun = sum, na.rm = TRUE)
# Save resampled grid to hard drive and delete from R workspace to free up space
precipitation_amount |>
  writeCDF(filename = "data/temp/precipitation-amount-19810101-20101231.nc",
           varname = "r", longname = "year sum of daily precipitation sums",
           unit = "mm",
           overwrite = TRUE)
rm(precipitation_amount)


# Load resampled raster data bank into R workspace and combine into SpatRasterDataset
weather <- sds(
  c(rast('data/temp/mean-temperature-19510101-19801231.nc'),
    rast('data/temp/mean-temperature-19810101-20101231.nc')),
  c(rast('data/temp/precipitation-amount-19510101-19801231.nc'),
    rast('data/temp/precipitation-amount-19810101-20101231.nc'))
)



# Resample all variables to decade-wise averages --------------------------

# Set up index for computing decade-wise averages
yrs <- seq(from = 1951, to = 2010, by = 10) |> rep(each = 10)
yrs <- paste0("Y", yrs, "_", yrs + 10)

# Resample the yearly temperature and precipitation statistics: take decennial
  # averages
weather_dec_avgs <- sds(
  tapp(weather$t, index = yrs, fun = mean, na.rm = TRUE),
  tapp(weather$r, index = yrs, fun = mean, na.rm = TRUE)
)

# Add variable names, longnames and unit specifications to resampled raster data
names(weather_dec_avgs) <- c("t_dec_avg", "r_dec_avg")
longnames(weather_dec_avgs) <- c(
  "decennial average of year averages of daily mean temperatures",
  "decennial average of year sums of daily precipitation sums"
)
units(weather_dec_avgs) <- c("Celsius", "mm")

# Save decennial raster data
dir.create("data/temp")
weather_dec_avgs |>
  writeCDF(filename = "data/temp/weather-dec-avgs.nc", overwrite = TRUE)
