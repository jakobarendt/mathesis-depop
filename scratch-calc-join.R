require(terra)
require(ncdf4)
require(sf)

rast()
app()


# Assuming the time dimension is the third dimension
time <- time(temperature_raster)
decades <- cut(time, breaks = seq(from = 1950, to = 2020, by = 10), right = FALSE)

decade_means <- lapply(levels(decades), function(dec) {
  dec_indices <- which(decades == dec)
  dec_stack <- temperature_raster[[dec_indices]]
  app(dec_stack, mean, na.rm = TRUE)
})

# Combine the results into a single SpatRaster
decade_means_raster <- rast(decade_means)


# Assuming the time dimension is named "time"
time <- st_get_dimension_values(temperature_stars, "time")
decades <- cut(time, breaks = seq(from = 1950, to = 2020, by = 10), right = FALSE)

decade_means <- lapply(levels(decades), function(dec) {
  dec_indices <- which(decades == dec)
  dec_stars <- temperature_stars[,,,dec_indices]
  st_apply(dec_stars, c("x", "y"), mean, na.rm = TRUE)
})

# Combine the results into a single stars object
decade_means_stars <- do.call(c, decade_means)



hist_pop <- hist_pop |>
  mutate(TEST_TEMP = exact_extract(weather[[1]], hist_pop, 'mean'))
