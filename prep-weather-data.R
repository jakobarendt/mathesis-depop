### This R script prepares the weather grid data ###



# Packages and settings ---------------------------------------------------

# Packages
require(ecmwfr)
require(terra)
require(tidyverse)
require(ncdf4)

# Settings
path_raw_data <- "data/weather"
path_processed_data <- "data/temp"
dir.create(path_raw_data, recursive = TRUE, showWarnings = FALSE)
dir.create(path_processed_data, recursive = TRUE, showWarnings = FALSE)



# Download weather data ---------------------------------------------------

# set a key to the device's keychain interactively
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
zipfile_mean_temp_precipitation <- wf_request(request, user, path = path_raw_data, time_out = 7200)

# Unzip downloaded ZIP file
zipfile_mean_temp_precipitation |>
  unzip(overwrite = FALSE, junkpaths = TRUE, exdir = path_raw_data)



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

# Resampling for all variables is split into multiple steps to limit
# computational load and in particular RAM load

# 1. Processing configuration
# Processing configuration defined as in this table. To add a new time chunk, add a new row.
config_yearly <- tribble(
  ~var_type, ~source_file, ~date_start, ~date_end, ~agg_fun, ~nc_var, ~unit, ~longname,
  "temp", "tg_ens_mean_0.1deg_reg_v29.0e.nc", "1951-01-01", "1980-12-31", "mean", "t", "Celsius", "year average of daily mean temperatures",
  "temp", "tg_ens_mean_0.1deg_reg_v29.0e.nc", "1981-01-01", "2010-12-31", "mean", "t", "Celsius", "year average of daily mean temperature",
  "prec", "rr_ens_mean_0.1deg_reg_v29.0e.nc", "1951-01-01", "1980-12-31", "sum", "r", "mm", "year sum of daily precipitation sums",
  "prec", "rr_ens_mean_0.1deg_reg_v29.0e.nc", "1981-01-01", "2010-12-31", "sum", "r", "mm", "year sum of daily precipitation sums"
)

# 2. Iterate through configuration
# Map over the config rows, process, and return the filenames of created files
yearly_aggregate_files <- pmap(config_yearly, function(var_type, source_file, date_start, date_end, agg_fun, nc_var, unit, longname) {

  # Name and file path of output file
  out_name <- file.path(path_processed_data, paste0(paste(var_type, agg_fun, year(as.Date(date_start)), year(as.Date(date_end)), sep = "-"), ".nc"))

  # Status message for the respective loop run
  message(paste("Processing:", var_type, "from", date_start, "to", date_end))

  # Load source (raster with daily data)
  r <- rast(file.path(path_raw_data, source_file))

  # Subset source respect to time
  r_sub <- r |>
    subset(time(r) >= as.Date(date_start) &
             time(r) <= as.Date(date_end))

  # Aggregate/Resample daily weather data to yearly statistics
  r_yearly <- r_sub |>
    tapp(index = "years", fun = agg_fun, na.rm = TRUE)
           #match.fun(agg_fun), na.rm = TRUE)

  # Write to disk
  r_yearly |>
    writeCDF(filename = out_name, varname = nc_var, longname = longname,
             unit = unit, overwrite = TRUE)

  # Free up space
  rm(r, r_sub, r_yearly)
  gc()

  # Return the file names of the created files
  # return(out_name)
  out_name
})



# Combine resampled data into SpatRasterDataset and save to file ----------

weather_year_avgs <- sds(
  c(rast('data/temp/temp-mean-1951-1980.nc'),
    rast('data/temp/temp-mean-1981-2010.nc')),
  c(rast('data/temp/prec-sum-1951-1980.nc'),
    rast('data/temp/prec-sum-1981-2010.nc'))
)

weather_year_avgs |>
  writeCDF(filename = file.path(path_processed_data, 'weather-year-avgs.nc'), overwrite = TRUE)



# Resample all variables to decade-wise averages --------------------------

# Set up index for computing decade-wise averages
decade_endings <- seq(from = 1960, to = 2010, by = 10)
decades <- paste0("Y", decade_endings - 9, "_", decade_endings)
decades_index <- decades |> rep(each = 10)

# Resample the yearly temperature and precipitation statistics: take decennial
  # averages
temp_dec_avgs <- weather_year_avgs$t |>
  tapp(index = decades_index, fun = mean, na.rm = TRUE)
prec_dec_avgs <- weather_year_avgs$r |>
  tapp(index = decades_index, fun = mean, na.rm = TRUE)

# Assign the last year of each decade as layer name before combining all to
  # SpatRasterDataset
depth(temp_dec_avgs) <- decade_endings
depthName(temp_dec_avgs) <- "dec_end"
depth(prec_dec_avgs) <- decade_endings
depthName(prec_dec_avgs) <- "dec_end"

# Combine to SpatRasterDataset
weather_dec_avgs <- sds(temp_dec_avgs, prec_dec_avgs)

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
