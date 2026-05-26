### This R script joins population and weather data and computes estimations ###




# Packages ----------------------------------------------------------------

require(exactextractr)
require(terra)
require(tidyverse)
require(sf)
require(fixest)
require(modelsummary)
require(ncdf4)
require(data.table)
require(marginaleffects)



# Load all different data sources or ask to run scripts if not there ------

# Population data
if (!file.exists('data/temp/population.RData')) {
  stop('Please run prep-population-data.R before running this script')
} else {
  load('data/temp/population.RData')
}

# Weather data
if (!file.exists('data/temp/weather-dec-avgs.nc')) {
  stop('Please run prep-weather-data.R before running this script.\n
       I recommend running it as a background job as it takes a long time.\n
       For download, a registration with the CDS service is necessary:\n
       https://cds.climate.copernicus.eu/user/register \n
       The script then needs username and API key saved on the machine, which\n
       can be done by running the following command before running the
       prep-weather-data.R script.\n
       user <- ecmwfr::wf_set_key(service = "cds")')
} else {
  weather <- rast('data/temp/weather-dec-avgs.nc')
}



# Verify correspondence of the CRS of all geographic data -----------------

stopifnot(
  st_crs(population)[[2]] == st_crs(shapes_full_map)[[2]] |
    st_crs(population)[[2]] == crs(weather)
)



# Spatial join of population and weather data -----------------------------

joined_data <- exact_extract(weather,
                            population,
                            fun = 'mean',
                            append_cols = names(population),
                            progress = TRUE) |>
  as_tibble()
# the mean function is optimised for exact_extract() and ignores NAs by default
  # if a grid cell is not fully intersecting a shape, it is weighted proportional
  # to the area the shape intersects with the cell relative to the total area of
  # the cell



# Introduce rural-urban differentiation based on first period -------------

# LAU is rural if its 1961 population density is lower than 300 inhabitants per
  # km2 and its 1961 total population is lower than 5,000 inhabitants
  # Differentiation simplied from the definition in European Commission and
  # Eurostat (2019)
joined_data <- joined_data |>
  mutate(POP_DENS_1961 = POP_1961_01_01 / AREA_KM2) |>
  mutate(RURAL_1961 = if_else(POP_DENS_1961 < 300 & POP_1961_01_01 < 5000, TRUE,
                              FALSE)) |>
  select(-POP_DENS_1961)



# Make table long for panel estimations -----------------------------------

# Mapping function for renaming spatial means columns of weather aggregates from
  # spatial join (resulting from exact_extract()).
  # Matches the last year of the weather decadal average (e.g. 1960) to the
  # respective decadal time point in the decadal historic population data (e.g. 1961)
map_year <- function(string) {
  end_year <- str_sub(string, -4) |> as.numeric()
  variable_name <- str_remove(string, "dec_avg_dec_end=\\d+")
  return(paste0(variable_name, end_year + 1))
}

panel_data <- joined_data |>
  rename_with(~ map_year(.), .cols = starts_with("mean.")) |>
  rename_with(~ str_remove(.x, "_01_01"), .cols = starts_with("POP_")) |> # TODO: optimise column selection here
  pivot_longer(
    cols = starts_with("POP_") | starts_with("mean."),
    names_to = c(".value", "YEAR"),
    names_pattern = "(.*)_(\\d{4})"
  ) |>
  mutate(YEAR = as.numeric(YEAR))



# Subset data sets for estimation and prediction & Add panel IDs ----------

# Sub-panel data set for estimation & Set panel identifiers
panel_data_est <- panel_data |>
  filter(YEAR <= 2011) |>
  as.data.table() |>
  fixest::panel(panel.id = ~CNTR_LAU_CODE+YEAR)

# Add log first-differences of the dependent variable manually before running
  # FE regressions
panel_data_est[, d_log_POP := d(log(POP))]



# Estimation: fixed-effects models ----------------------------------------

temp <- ".t"  # alternatively: ".t_jja", ".t_n_days"
prec <- ".r"  # alternatively: ".r_jja"

dec_panel_models <- setNames(lapply(temp, function(temp) {

  list(
    # TODO: check results with country-time (squared) trend
    # TODO: check whether to also run the Conley HAC standard errors
    # TODO: check model performance without rural-urban dummy

    # Pooled models (without rural dummy):
    # feols(d_log_POP ~ mean.[temp] + mean.[prec],
    #       data = panel_data_est, vcov = cluster ~ CNTR_LAU_CODE),  # VCOV (variance-covariance) matrix is not positive semi-definite, likely due to numerical instability of the scaling of mean.[prec]. The computer has difficulties inverting the cluster covariance matrix due to floating-point precision limits.

    feols(d_log_POP ~ mean.[temp] + mean.[temp]^2
          + mean.[prec] + mean.[prec]^2
          | CNTR_LAU_CODE + CNTR_CODE^YEAR,
          data = panel_data_est, vcov = cluster ~ CNTR_LAU_CODE),
    feols(d_log_POP ~ mean.[temp] / RURAL_1961 + mean.[temp]^2 / RURAL_1961
          + mean.[prec] + mean.[prec]^2
          | CNTR_LAU_CODE + CNTR_CODE^YEAR,
          data = panel_data_est, vcov = cluster ~ CNTR_LAU_CODE),
    feols(d_log_POP ~ mean.[temp] + mean.[temp]^2
          + mean.[prec] / RURAL_1961 + mean.[prec]^2 / RURAL_1961
          | CNTR_LAU_CODE + CNTR_CODE^YEAR,
          data = panel_data_est, vcov = cluster ~ CNTR_LAU_CODE),
    feols(d_log_POP ~ mean.[temp] / RURAL_1961 + mean.[temp]^2 / RURAL_1961
          + mean.[prec] / RURAL_1961 + mean.[prec]^2 / RURAL_1961
          | CNTR_LAU_CODE + CNTR_CODE^YEAR,
          data = panel_data_est, vcov = cluster ~ CNTR_LAU_CODE)

  )
}), temp)

save(dec_panel_models, file = 'data/temp/model-results.RData')



# Exclusion of observations during estimations: no. of NAs in data --------
# TODO

# Overview of NAs in baseline panel data set
panel_data |>
  group_by(YEAR) |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(!YEAR, names_to = "column", values_to = "na_count") |>
  pivot_wider(names_from = YEAR, names_prefix = "YEAR_", values_from = na_count)

# Check if panel data attributes and identifiers are also saved with tibble,
  # alternatively use package data.table
panel_date_as_data_table <- panel_data |>
  as.data.table() |>
  panel(panel.id = ~CNTR_LAU_CODE+YEAR)

# Check on values removed during regression
panel_date_as_data_table[, d_log_pop := d(log(POP))]
lhs_unused <- panel_date_as_data_table |>
  filter(is.na(d_log_pop) | is.infinite(d_log_pop)) |>
  as_tibble()
rhs_unused <- panel_date_as_data_table |>
  filter(if_any(c(mean.t, mean.r), ~ is.na(.x) | is.infinite(.x))) |>
  as_tibble()



# Curve plots: regressors -------------------------------------------------
# TODO

# model |> fixef() für Ergebnisse zu den fixed effects
# model |> fixef() |> summary()
# model |> fixef() |> plot()

# coefplot() für coefficient plot

# TODO: solve issues with implementation via marginaleffects package
# plot_predictions(models_decennial[[16]], condition = c("mean.t", "RURAL_1961"))



# Scratch notes - TODO: maps of spatial overlay ---------------------------
# TODO

# scratch code to plot shapes of Graz and its surroundings along with the
# grid cells that overlay them

graz <- shapes_full_map |>
  filter(CNTR_LAU_CODE == "AT60101")
graz_surroundings <- shapes_full_map |>
  st_filter(graz)
mask(weather$`t_dec_avg_dec_end=1960`, graz_surroundings) |>
  trim() |>
  plot()
polys(graz_surroundings)
plot(st_centroid(graz_surroundings), add = TRUE)

belgium <- shapes_full_map |>
  filter(CNTR_CODE == "BE")
mask(weather$`t_dec_avg_dec_end=2010`, belgium) |>
  trim() |>
  plot()
polys(belgium)

norway <- shapes_full_map |>
  filter(CNTR_CODE == "NO")
mask(weather$`t_dec_avg_dec_end=2010`, norway) |>
  trim() |>
  plot()
polys(norway)

austria <- shapes_full_map |>
  filter(CNTR_CODE == "AT")
mask(weather$`t_dec_avg_dec_end=2010`, austria) |>
  trim() |>
  plot()
polys(austria)

# Plotting multiple attributes in sub-graphs
population |>
  filter(CNTR_CODE == "AT") |>
  plot()
