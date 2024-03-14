## Match soil grid points to 8 km grid

rm(list = ls())

#### ILLINOIS ####

# Load point-level soil data
load('data/processed/soils/processed_soil_il.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/illinois_matched.RData')

# Check if the soil and PLS are in the same order
identical(IL_soil$pls_x, illinois_ecosystem_matched$x)
identical(IL_soil$pls_y, illinois_ecosystem_matched$y)

# Join the grid ID to the soil dataset
IL_soil_matched <- illinois_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = IL_soil, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(IL_soil_matched$pls_x, illinois_ecosystem_matched$x)
identical(IL_soil_matched$pls_y, illinois_ecosystem_matched$y)

# Summarize over grid cells
IL_soil_grid <- IL_soil_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(clay = mean(clay),
                   sand = mean(sand),
                   silt = mean(silt),
                   caco3 = mean(caco3),
                   awc = mean(awc))

# Check the number of individual grid cells in the state matches
length(unique(IL_soil_grid$grid_id)) == length(unique(illinois_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
IL_soil_grid <- IL_soil_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
IL_soil_grid <- sf::st_as_sf(IL_soil_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
IL_soil_grid <- sf::st_transform(IL_soil_grid, crs = 'EPSG:4326')
IL_soil_grid <- sfheaders::sf_to_df(IL_soil_grid, fill = TRUE)
IL_soil_grid <- dplyr::select(IL_soil_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'illinois',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- IL_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- IL_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = sand), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(IL_soil_grid, file = 'data/processed/soils/gridded_soil_il.RData')

#### INDIANA ####
rm(list = ls())

# Load point-level soil data
load('data/processed/soils/processed_soil_in.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/indiana_matched.RData')

# Check if the soil and PLS are in the same order
identical(IN_soil$pls_x, indiana_ecosystem_matched$x)
identical(IN_soil$pls_y, indiana_ecosystem_matched$y)

# Join the grid ID to the soil dataset
IN_soil_matched <- indiana_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = IN_soil, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(IN_soil_matched$pls_x, indiana_ecosystem_matched$x)
identical(IN_soil_matched$pls_y, indiana_ecosystem_matched$y)

# Summarize over grid cells
IN_soil_grid <- IN_soil_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(clay = mean(clay),
                   sand = mean(sand),
                   silt = mean(silt),
                   caco3 = mean(caco3),
                   awc = mean(awc))

# Check the number of individual grid cells in the state matches
length(unique(IN_soil_grid$grid_id)) == length(unique(indiana_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
IN_soil_grid <- IN_soil_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
IN_soil_grid <- sf::st_as_sf(IN_soil_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
IN_soil_grid <- sf::st_transform(IN_soil_grid, crs = 'EPSG:4326')
IN_soil_grid <- sfheaders::sf_to_df(IN_soil_grid, fill = TRUE)
IN_soil_grid <- dplyr::select(IN_soil_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'indiana',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- IN_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- IN_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = sand), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(IN_soil_grid, file = 'data/processed/soils/gridded_soil_in.RData')

#### MICHIGAN ####
rm(list = ls())

# Load point-level soil data
load('data/processed/soils/processed_soil_mi.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/lowmichigan_matched.RData')
load('data/processed/PLS/upmichigan_matched.RData')

michigan_ecosystem_matched <- rbind(lowmichigan_ecosystem_matched, upmichigan_ecosystem_matched)

# Check if the soil and PLS are in the same order
identical(MI_soil$pls_x, michigan_ecosystem_matched$x)
identical(MI_soil$pls_y, michigan_ecosystem_matched$y)

# Join the grid ID to the soil dataset
MI_soil_matched <- michigan_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = MI_soil, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(MI_soil_matched$pls_x, michigan_ecosystem_matched$x)
identical(MI_soil_matched$pls_y, michigan_ecosystem_matched$y)

# Summarize over grid cells
MI_soil_grid <- MI_soil_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(clay = mean(clay),
                   sand = mean(sand),
                   silt = mean(silt),
                   caco3 = mean(caco3),
                   awc = mean(awc))

# Check the number of individual grid cells in the state matches
length(unique(MI_soil_grid$grid_id)) == length(unique(michigan_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
MI_soil_grid <- MI_soil_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
MI_soil_grid <- sf::st_as_sf(MI_soil_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
MI_soil_grid <- sf::st_transform(MI_soil_grid, crs = 'EPSG:4326')
MI_soil_grid <- sfheaders::sf_to_df(MI_soil_grid, fill = TRUE)
MI_soil_grid <- dplyr::select(MI_soil_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'michigan',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- MI_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- MI_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = sand), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(MI_soil_grid, file = 'data/processed/soils/gridded_soil_mi.RData')

#### MINNESOTA ####
rm(list = ls())

# Load point-level soil data
load('data/processed/soils/processed_soil_mn.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/minnesota_matched.RData')

# Check if the soil and PLS are in the same order
identical(MN_soil$pls_x, minnesota_ecosystem_matched$x)
identical(MN_soil$pls_y, minnesota_ecosystem_matched$y)

# Join the grid ID to the soil dataset
MN_soil_matched <- minnesota_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = MN_soil, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(MN_soil_matched$pls_x, minnesota_ecosystem_matched$x)
identical(MN_soil_matched$pls_y, minnesota_ecosystem_matched$y)

# Summarize over grid cells
MN_soil_grid <- MN_soil_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(clay = mean(clay),
                   sand = mean(sand),
                   silt = mean(silt),
                   caco3 = mean(caco3),
                   awc = mean(awc))

# Check the number of individual grid cells in the state matches
length(unique(MN_soil_grid$grid_id)) == length(unique(minnesota_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
MN_soil_grid <- MN_soil_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
MN_soil_grid <- sf::st_as_sf(MN_soil_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
MN_soil_grid <- sf::st_transform(MN_soil_grid, crs = 'EPSG:4326')
MN_soil_grid <- sfheaders::sf_to_df(MN_soil_grid, fill = TRUE)
MN_soil_grid <- dplyr::select(MN_soil_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- MN_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- MN_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = sand), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(MN_soil_grid, file = 'data/processed/soils/gridded_soil_mn.RData')

#### WISCONSIN ####
rm(list = ls())

# Load point-level soil data
load('data/processed/soils/processed_soil_wi.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/wisconsin_matched.RData')

# Check if the soil and PLS are in the same order
identical(WI_soil$pls_x, wisconsin_ecosystem_matched$x)
identical(WI_soil$pls_y, wisconsin_ecosystem_matched$y)

# Join the grid ID to the soil dataset
WI_soil_matched <- wisconsin_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = WI_soil, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(WI_soil_matched$pls_x, wisconsin_ecosystem_matched$x)
identical(WI_soil_matched$pls_y, wisconsin_ecosystem_matched$y)

# Summarize over grid cells
WI_soil_grid <- WI_soil_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(clay = mean(clay),
                   sand = mean(sand),
                   silt = mean(silt),
                   caco3 = mean(caco3),
                   awc = mean(awc))

# Check the number of individual grid cells in the state matches
length(unique(WI_soil_grid$grid_id)) == length(unique(wisconsin_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
WI_soil_grid <- WI_soil_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
WI_soil_grid <- sf::st_as_sf(WI_soil_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
WI_soil_grid <- sf::st_transform(WI_soil_grid, crs = 'EPSG:4326')
WI_soil_grid <- sfheaders::sf_to_df(WI_soil_grid, fill = TRUE)
WI_soil_grid <- dplyr::select(WI_soil_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- WI_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- WI_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = sand), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(WI_soil_grid, file = 'data/processed/soils/gridded_soil_wi.RData')
