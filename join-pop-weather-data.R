### This R script joins population and weather data ###




# Packages ----------------------------------------------------------------

require(exactextractr)
require(terra)
require(tidyverse)
require(parallel)
require(sf)
require(plm)
require(fixest)
require(modelsummary)



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
joined_data <- joined_data |> as_tibble() # initially it was panel_data |> as_tibble() here
# clarify why !?!?!

# !! Add na.rm = true to fun = 'mean' ??
# Check whether the numbering in the weather variables is actually chronological



# Reformat table for panel estimations ------------------------------------

## Year mapping for renaming weather data columns with year
# year_mapping <- setNames(c("1961", "1971", "1981", "1991", "2001", "2011"), 1:6)
map_year <- function(string) {
  years <- seq(1961, 2011, 10)
  key <- str_extract(string, "\\d") |> as.numeric()
  variable_name <- str_extract(string, "^[^0-9]*")
  return(paste0(variable_name, years[key]))
}

panel_data <- joined_data |>
  select(CNTR_LAU_CODE, CNTR_CODE, ends_with("1_01_01"), starts_with("mean.")) |>
  rename_with(~ map_year(.), .cols = starts_with("mean.")) |>
  pivot_longer(
    cols = starts_with("POP_") | starts_with("mean."),
    names_to = c(".value", "YEAR"),
    names_sep = "_",
    # names_pattern = "(.*)_(.*)"
  )



# Introduce rural-urban differentiation from base year --------------------

panel_data <- panel_data |>
  left_join(
    select(joined_data, CNTR_LAU_CODE, EUROGEOGRAPHICS_AREA_KM2_2011_2012),
    by = c("CNTR_LAU_CODE")
  ) |>
  mutate(POP_DENS = POP / EUROGEOGRAPHICS_AREA_KM2_2011_2012) |>
  mutate(RURAL = if_else(POP_DENS < 1500, 1, 0)) |>
  group_by(CNTR_LAU_CODE) |>
  arrange(YEAR, .by_group = TRUE) |>
  mutate(RURAL_1961 = first(RURAL)) |>
  select(-c(EUROGEOGRAPHICS_AREA_KM2_2011_2012, POP_DENS, RURAL)) |>
  ungroup()



# Add time indicator ------------------------------------------------------

panel_data <- panel_data |>
  group_by(CNTR_LAU_CODE) |>
  arrange(YEAR, .by_group = TRUE) |>
  mutate(TTREND = row_number()) |>
  ungroup() |>
  mutate(log_POP = log(POP))

# Filter out LAUs with non-positive integers of log_POP variable
laus_filter_out <- panel_data |>
  filter(is.na(log_POP) | is.nan(log_POP) | is.infinite(log_POP)) |>
  pull(CNTR_LAU_CODE) |>
  unique()
panel_data <- panel_data |>
  filter(!(CNTR_LAU_CODE %in% laus_filter_out))

# panel_data_frame <- panel_data |>
#   # mutate(CNTR_LAU_CODE = as.factor(CNTR_LAU_CODE), YEAR = as.factor(YEAR)) |>
#   pdata.frame(index = c("CNTR_LAU_CODE", "YEAR"))
# plm(POP ~ mean.mean.daily.mean.temperature + mean.sum.daily.precipitation.amount,
#     data = panel_data_frame,
#     effect = "individual") |>
#   summary()

# Finish code for aggregation to form interaction term of country-year-FEs
# (or, alternatively, interaction of country-year-trend)
# panel_data <- panel_data |>
#   mutate(CNTR_CODE_YEAR = CNTR_CODE)

# Also transform population figures to log(POP), such that I have log-level eco-
# nometric models

# Overview of foreseen models:
# 1. Pooled model without&with rural(urban) dummy --> spatial autocorrelation-SEs
# also possible to estimate?
# 2. Panel without rural(urban) dummy with clustered & with Conley HAC SEs
# 3. Panel with rural(urban) dummy with clustered & with Conley HAC SEs
# --> Estimate model with rural-urban effect for temperature only, for precipi-
# tation only, and lastly also temperature-specific and precipitation-specific
# rural-urban effects together in the same model
# --> Do I also consider non-linear functional forms of climate variables in the
# model equations (and estimate them)?

feols(POP ~ `mean.mean-daily-mean-temperature` + `mean.sum-daily-precipitation-amount` |
        CNTR_LAU_CODE + CNTR_CODE*YEAR,
      data = panel_data) |> # find reason why panel_data_frame cannot be used
  summary()

feols(POP ~ `mean.mean-daily-mean-temperature` + `mean.sum-daily-precipitation-amount` |
        CNTR_LAU_CODE + CNTR_CODE,
      data = panel_data) |> # find reason why panel_data_frame cannot be used
  summary()

feols(POP ~ `mean.mean-daily-mean-temperature` + `mean.sum-daily-precipitation-amount` |
        CNTR_LAU_CODE + CNTR_CODE + YEAR,
      data = panel_data) |> # find reason why panel_data_frame cannot be used
  summary()

# !! Investigate warning message that is thrown when pivot_longer
# !! Investigate warning message that is thrown pdata.frame
# !! Check what is up with Poian, must be in data set twice

models_decennial <- list(
  "Pooled (OLS)" = lm(log_POP ~ `mean.mean-daily-mean-temperature`
                      + `mean.mean-daily-mean-temperature` : RURAL_1961
                      + `mean.sum-daily-precipitation-amount`, data = panel_data),
  "Pooled (GLS)" = glm(log_POP ~ `mean.mean-daily-mean-temperature`
                       + `mean.mean-daily-mean-temperature` : RURAL_1961
                       + `mean.sum-daily-precipitation-amount`
                       + I(`mean.sum-daily-precipitation-amount`^2), data = panel_data),
  # "REs" = plm(log_POP ~ `mean.mean-daily-mean-temperature`
  #             + `mean.mean-daily-mean-temperature` : RURAL_1961
  #             + `mean.sum-daily-precipitation-amount`, data = panel_data,
  #             model = "random", index = c("CNTR_LAU_CODE", "YEAR")),
  "FEs (one-way)" = feols(log_POP ~ `mean.mean-daily-mean-temperature`
                          + `mean.mean-daily-mean-temperature` : RURAL_1961
                          + `mean.sum-daily-precipitation-amount` | CNTR_LAU_CODE,
                          data = panel_data),
  "FEs (two-way)" = feols(log_POP ~ `mean.mean-daily-mean-temperature`
                          + `mean.mean-daily-mean-temperature` : RURAL_1961
                          + `mean.sum-daily-precipitation-amount`
                          | CNTR_LAU_CODE + YEAR,
                          data = panel_data),
  "FEs (one-way & time-trend)" = feols(log_POP ~ `mean.mean-daily-mean-temperature`
                                       + `mean.mean-daily-mean-temperature` : RURAL_1961
                                       + `mean.sum-daily-precipitation-amount`
                                       + TTREND
                                       | CNTR_LAU_CODE,
                                       data = panel_data)
  )
save(panel_data, models_decennial, file = 'data/temp/model-results.RData')

# Estimation of standard errors -------------------------------------------

# 1st Option: Standard error cluster

# 2st Option: Conley HAC errors
# install.packages("conleyreg")
# Borsky: Spatial autocorrelation wahrscheinlich estimated über long & lat
# --> über sf mittelpunkt zu jedem shape berechnen und in tibble mitgeben

# pop |>
#   filter(is.na(POP_1961)) |>
#   leaflet() |>
#   addTiles(options = tileOptions(opacity = 0.2)) |>
#   addPolygons(
#     stroke = FALSE,
#     fillOpacity = 0.7,
#     fillColor = ~pal(POP_2011),
#     label = ~ paste0(LAU_NAME, ": ", round(POP_2011, 4))
#   ) |>
#   addLegend(pal = pal, values = ~POP_2011, opacity = 1.0)
#
#
# population |>
#   select(CNTR_LAU_CODE, geometry) |>
#   right_join(panel_data, by = c("CNTR_LAU_CODE"))

# panel_data <-