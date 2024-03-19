## Match topography grid points to 8 km grid

rm(list = ls())

# Load point-level topography data
load('data/processed/topography/processed_topo_il.RData')
load('data/processed/topography/processed_topo_in.RData')
load('data/processed/topography/processed_topo_mi.RData')
load('data/processed/topography/processed_topo_mn.RData')
load('data/processed/topography/processed_topo_wi.RData')

# Check if columns are all the same
all(colnames(IL_topo) == colnames(IN_topo))
all(colnames(IN_topo) == colnames(MI_topo))
all(colnames(MI_topo) == colnames(MN_topo))
all(colnames(MN_topo) == colnames(WI_topo))

# Combine
topo <- rbind(IL_topo, IN_topo, MI_topo, MN_topo, WI_topo)

# Load matched point and gridded PLS data
load('data/processed/PLS/total_matched.RData')

# Join the grid ID to the topography dataset
topo_matched <- ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = topo, by = c('pls_x', 'pls_y'))

# Check if the topography and PLS are in the same order
identical(topo_matched$pls_x, ecosystem_matched$x)
identical(topo_matched$pls_y, ecosystem_matched$y)

# Summarize over grid cells
topo_grid <- topo_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(elevation = mean(elevation),
                   slope = mean(slope),
                   aspect = mean(aspect))

# Check the number of individual grid cells matches
length(unique(topo_grid$grid_id)) == length(unique(ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
topo_grid <- topo_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
topo_grid <- sf::st_as_sf(topo_grid, coords = c('grid_x', 'grid_y'),
                          crs = 'EPSG:3175')
topo_grid <- sf::st_transform(topo_grid, crs = 'EPSG:4326')
topo_grid <- sfheaders::sf_to_df(topo_grid, fill = TRUE)
topo_grid <- dplyr::select(topo_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(topo_grid, file = 'data/processed/topography/gridded_topography.RData')
