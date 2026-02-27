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
# Processing configuration defined as in the CSV file's table. To add a new time chunk, add a new row.
config_yearly <- read_csv(file = "weather-config-yearly.csv") |>
  mutate(across(starts_with("date_"), ~ as.Date(.x, format = "%d/%m/%Y")))

# 2. Iterate through configuration
# Map over the config rows, process, and return the filenames of created files
yearly_aggregate_files <- pmap(config_yearly, function(var_type, source_file, date_start, date_end, months, agg_fun, threshold_temp, nc_var, unit, longname) {

  # Name and file path of output file
  out_name <- file.path(path_processed_data, paste0(paste(nc_var, agg_fun, year(as.Date(date_start)), year(as.Date(date_end)), sep = "-"), ".nc"))

  # Status message for the respective loop run
  message(paste0("Processing: ", var_type, " (", nc_var, ": ", longname, ") ",
                "from ", date_start, " to ", date_end))

  # Load source (raster with daily data)
  r <- rast(file.path(path_raw_data, source_file))

  # Subset source with regards to time period length (for better memory allocation)
  r_sub <- r |>
    subset(time(r) >= as.Date(date_start) &
             time(r) <= as.Date(date_end))

  # Subset within year if needed (by months)
  if (!is.na(months)) {
    months_vector <- scan(text = months, sep = ",")
    r_sub <- r_sub |>
      subset(month(time(r_sub)) %in% months_vector)
    rm(months_vector)
  }

  # if-statement for statistics that only consider the number of hot days or
    # other thresholds
  if (!is.na(threshold_temp)) {
    r_sub <- r_sub >= threshold_temp
  }

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

# Unlist filepaths resulting from pmap-loop into vector
yearly_aggregate_files_vec <- unlist(yearly_aggregate_files)



# Combine resampled data into SpatRasterDataset and save to file ----------

weather_year_avgs <- sds(
  rast(yearly_aggregate_files_vec[1:2]),
  rast(yearly_aggregate_files_vec[3:4]),
  rast(yearly_aggregate_files_vec[5:6]),
  rast(yearly_aggregate_files_vec[7:8]),
  rast(yearly_aggregate_files_vec[9:10])
)

weather_year_avgs |>
  writeCDF(filename = file.path(path_processed_data, 'weather-year-avgs.nc'), overwrite = TRUE)



# Resample all variables to decade-wise averages --------------------------

# Set up index for computing decade-wise averages
decade_endings <- seq(from = 1960, to = 2020, by = 10)
decades <- paste0("Y", decade_endings - 9, "_", decade_endings)
decades_index <- decades |> rep(each = 10)

# Helper function for aggregation to decadal averages
  # also adds layer names
calc_decade_avg <- function(raster_layer) {
  # Resampling
  dec_agg <- raster_layer |>
    tapp(index = decades_index, fun = mean, na.rm = TRUE)

  # Assign the last year of each decade as layer name
  depth(dec_agg) <- decade_endings
  depthName(dec_agg) <- "dec_end"

  return(dec_agg)
}

# Calculate/Resample: yearly statistics to decennial averages
t_dec_avgs <- calc_decade_avg(weather_year_avgs$t)
r_dec_avgs <- calc_decade_avg(weather_year_avgs$r)
t_jja_dec_avgs <- calc_decade_avg(weather_year_avgs$t_jja)
r_jja_dec_avgs <- calc_decade_avg(weather_year_avgs$r_jja)
t_n_days_dec_avgs <- calc_decade_avg(weather_year_avgs$t_n_days)

# Combine to SpatRasterDataset
weather_dec_avgs <- sds(t_dec_avgs, r_dec_avgs,
                        t_jja_dec_avgs, r_jja_dec_avgs,
                        t_n_days_dec_avgs)

# Add variable names, longnames and unit specifications to resampled raster data
names(weather_dec_avgs) <- c("t_dec_avg", "r_dec_avg",
                             "t_jja_dec_avg", "r_jja_dec_avg",
                             "t_n_days_dec_avg")
longnames(weather_dec_avgs) <- c(
  "decennial average of year averages of daily mean temperatures",
  "decennial average of year sums of daily precipitation sums",
  "decennial average of summer (JJA) averages of daily mean temperatures",
  "decennial average of summer (JJA) sums of daily precipitation sums",
  "decennial average of year number of days with mean temp >=30C"
)
units(weather_dec_avgs) <- c("Celsius", "mm", "Celsius", "mm", "no.")

# Save decennial raster data
weather_dec_avgs |>
  writeCDF(filename = file.path(path_processed_data, "weather-dec-avgs.nc"),
           overwrite = TRUE)
