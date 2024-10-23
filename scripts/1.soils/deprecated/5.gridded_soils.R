## Match soil grid points to 8 km grid

rm(list = ls())

# Load point-level soil data
load('data/processed/soils/processed_soil_il.RData')
load('data/processed/soils/processed_soil_in.RData')
load('data/processed/soils/processed_soil_mi.RData')
load('data/processed/soils/processed_soil_mn.RData')
load('data/processed/soils/processed_soil_wi.RData')

# Check if columns are all the same
all(colnames(IL_soil) == colnames(IN_soil))
all(colnames(IN_soil) == colnames(MI_soil))
all(colnames(MI_soil) == colnames(MN_soil))
all(colnames(MN_soil) == colnames(WI_soil))

# Combine
soil <- rbind(IL_soil, IN_soil, MI_soil, MN_soil, WI_soil)

# Load matched point and gridded PLS data
load('data/processed/PLS/total_matched.RData')

# Join the grid ID to the soil dataset
soil_matched <- ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = soil, by = c('pls_x', 'pls_y'))

# Check if the soil and PLS are in the same order
identical(soil_matched$pls_x, ecosystem_matched$x)
identical(soil_matched$pls_y, ecosystem_matched$y)

# Summarize over grid cells
soil_grid <- soil_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(clay = mean(clay),
                   sand = mean(sand),
                   silt = mean(silt),
                   caco3 = mean(caco3),
                   awc = mean(awc))

# Check the number of individual grid cells matches
length(unique(soil_grid$grid_id)) == length(unique(ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
soil_grid <- soil_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
soil_grid <- sf::st_as_sf(soil_grid, coords = c('grid_x', 'grid_y'),
                          crs = 'EPSG:3175')
soil_grid <- sf::st_transform(soil_grid, crs = 'EPSG:4326')
soil_grid <- sfheaders::sf_to_df(soil_grid, fill = TRUE)
soil_grid <- dplyr::select(soil_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- soil |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = sand), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(soil_grid, file = 'data/processed/soils/gridded_soil.RData')