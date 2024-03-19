## Match floodplain grid points to 8 km grid

rm(list = ls())

# Load point-level floodplain data
load('data/processed/soils/processed_flood_il.RData')
load('data/processed/soils/processed_flood_in.RData')
load('data/processed/soils/processed_flood_mi.RData')
load('data/processed/soils/processed_flood_mn.RData')
load('data/processed/soils/processed_flood_wi.RData')

# Check if columns are all the same
all(colnames(IL_flood) == colnames(IN_flood))
all(colnames(IN_flood) == colnames(MI_flood))
all(colnames(MI_flood) == colnames(MN_flood))
all(colnames(MN_flood) == colnames(WI_flood))

# Combine
flood <- rbind(IL_flood, IN_flood, MI_flood, MN_flood, WI_flood)

# Load matched point and gridded PLS data
load('data/processed/PLS/total_matched.RData')

# Join the grid ID to the floodplain dataset
flood_matched <- ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = flood, by = c('pls_x', 'pls_y'))

# Check if the floodplain and PLS are in the same order
identical(flood_matched$pls_x, ecosystem_matched$x)
identical(flood_matched$pls_y, ecosystem_matched$y)

# Summarize over grid cells
flood_grid <- flood_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(Floodplain = sum(Floodplain == 'Yes') / dplyr::n())

# Check the number of individual grid cells matches
length(unique(flood_grid$grid_id)) == length(unique(ecosystem_matched$grid_id))

# Load intermedaite grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numberes and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to floodplain dataframe
flood_grid <- flood_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
flood_grid <- sf::st_as_sf(flood_grid, coords = c('grid_x', 'grid_y'),
                           crs = 'EPSG:3175')
flood_grid <- sf::st_transform(flood_grid, crs = 'EPSG:4326')
flood_grid <- sfheaders::sf_to_df(flood_grid, fill = TRUE)
flood_grid <- dplyr::select(flood_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- flood_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = Floodplain)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- flood |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = Floodplain)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(flood_grid, file = 'data/processed/soils/gridded_flood.RData')
