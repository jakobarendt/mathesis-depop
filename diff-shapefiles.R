# Investigate whether there are significant differences in the shapes of 2011
# and 2012 (for BE, CY, HU, IS, LU, LV, MT, NO, PL, RO, SE, SK)


# 1. Check how LAU identifiers match between the two shapefiles

shapes_test <- shapes_2011 |>
  as_tibble() |>
  full_join(shapes_2012, by = join_by(CNTR_LAU_ID == CNTR_LAU_ID))
# filter shapes_test for rows where CNTR_LAU_ID starts with any of these
# country codes: BE, CY, HU, IS, LU, LV, MT, NO, PL, RO, SE, SK
# sort table by first two letters of CNTR_LAU_ID
# count empty geometries in both shapefile columns
shapes_test |>
  mutate(CNTR_CODE = str_sub(CNTR_LAU_ID, 1, 2)) |>
  arrange(CNTR_CODE, CNTR_LAU_ID) |>
  filter(CNTR_CODE %in% c("BE", "CY", "HU", "IS", "LU", "LV", "MT", "NO", "PL", "RO", "SE", "SK")) |>
  group_by(CNTR_CODE) |>
  summarise(n_empty_2011 = sum(st_is_empty(geometry_2011)), n_empty_2012 = sum(st_is_empty(geometry_2012))) |> View()

shapes_test |>
  mutate(CNTR_CODE = str_sub(CNTR_LAU_ID, 1, 2)) |>
  arrange(CNTR_CODE, CNTR_LAU_ID) |>
  filter(CNTR_CODE %in% c("BE", "CY", "HU", "IS", "LU", "LV", "MT", "NO", "PL", "RO", "SE", "SK")) |>
  group_by(CNTR_CODE) |>
filter(st_is_empty(geometry_2011) | st_is_empty(geometry_2012)) |> View()



# 2. Check how boundaries in the two shapefiles overlap spatially

# For the subset of countries that have equal amounts of LAU shapes in both
# versions of the EuroGeographics LAU shapefiles, are there any significant
# differences in the geographic dimensions of their shapes?

shapes_test <- st_join(shapes_2011, shapes_2012, join = st_equals)
shapes_test |> filter(!is.na(LAU_NAME.y)) |> group_by(CNTR_CODE.x, CNTR_CODE.y) |>
  summarise(n = n())

## BE
shapes_test_be <- st_join(filter(shapes_2011, CNTR_CODE == "BE"), filter(shapes_2012, CNTR_CODE == "BE"), join = st_equals_exact, par = 0.1)
shapes_test_be |> filter(!is.na(LAU_NAME.y))

## CY
shapes_test_cy <- st_join(filter(shapes_2011, CNTR_CODE == "CY"), filter(shapes_2012, CNTR_CODE == "CY"), join = st_equals_exact, par = 0.1)
shapes_test_cy |> filter(!is.na(LAU_NAME.y))



shapes_test |> filter(CNTR_CODE.x == "BE" | CNTR_CODE.y == "BE")
shapes_test |> filter(CNTR_CODE.x == "CY" | CNTR_CODE.y == "CY")
shapes_test |> filter(CNTR_CODE.x == "HU" | CNTR_CODE.y == "HU")