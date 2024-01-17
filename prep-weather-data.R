### This R script prepares the weather grid data ###



# Packages ----------------------------------------------------------------

require(terra)
require(exactextractr)



# Weather data ------------------------------------------------------------

weather <- rast("data/weather/tg_ens_mean_0.1deg_reg_v28.0e.nc")

hist_pop <- hist_pop |>
  mutate(TEST_TEMP = exact_extract(weather[[1]], hist_pop, 'mean'))
