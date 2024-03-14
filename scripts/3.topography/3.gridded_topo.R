## Match topography grid points to 8 km grid

rm(list = ls())

#### ILLINOIS ####

# Load point-level soil data
load('data/processed/topography/processed_topo_il.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/illinois_matched.RData')

# Check if the soil and PLS are in the same order
identical(IL_topo$pls_x, illinois_ecosystem_matched$x)
identical(IL_topo$pls_y, illinois_ecosystem_matched$y)

# Join the grid ID to the soil dataset
IL_topo_matched <- illinois_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = IL_topo, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(IL_topo_matched$pls_x, illinois_ecosystem_matched$x)
identical(IL_topo_matched$pls_y, illinois_ecosystem_matched$y)

# Summarize over grid cells
IL_topo_grid <- IL_topo_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(elevation = mean(elevation),
                   slope = mean(slope),
                   aspect = mean(aspect))

# Check the number of individual grid cells in the state matches
length(unique(IL_topo_grid$grid_id)) == length(unique(illinois_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
IL_topo_grid <- IL_topo_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
IL_topo_grid <- sf::st_as_sf(IL_topo_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
IL_topo_grid <- sf::st_transform(IL_topo_grid, crs = 'EPSG:4326')
IL_topo_grid <- sfheaders::sf_to_df(IL_topo_grid, fill = TRUE)
IL_topo_grid <- dplyr::select(IL_topo_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'illinois',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- IL_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- IL_topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = elevation), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(IL_topo_grid, file = 'data/processed/topography/gridded_topo_il.RData')

#### INDIANA ####
rm(list = ls())

# Load point-level soil data
load('data/processed/topography/processed_topo_in.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/indiana_matched.RData')

# Check if the soil and PLS are in the same order
identical(IN_topo$pls_x, indiana_ecosystem_matched$x)
identical(IN_topo$pls_y, indiana_ecosystem_matched$y)

# Join the grid ID to the soil dataset
IN_topo_matched <- indiana_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = IN_topo, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(IN_topo_matched$pls_x, indiana_ecosystem_matched$x)
identical(IN_topo_matched$pls_y, indiana_ecosystem_matched$y)

# Summarize over grid cells
IN_topo_grid <- IN_topo_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(elevation = mean(elevation),
                   slope = mean(slope),
                   aspect = mean(aspect))

# Check the number of individual grid cells in the state matches
length(unique(IN_topo_grid$grid_id)) == length(unique(indiana_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
IN_topo_grid <- IN_topo_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
IN_topo_grid <- sf::st_as_sf(IN_topo_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
IN_topo_grid <- sf::st_transform(IN_topo_grid, crs = 'EPSG:4326')
IN_topo_grid <- sfheaders::sf_to_df(IN_topo_grid, fill = TRUE)
IN_topo_grid <- dplyr::select(IN_topo_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'indiana',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- IN_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- IN_topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = elevation), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(IN_topo_grid, file = 'data/processed/topography/gridded_topo_in.RData')

#### MICHIGAN ####
rm(list = ls())

# Load point-level soil data
load('data/processed/topography/processed_topo_mi.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/lowmichigan_matched.RData')
load('data/processed/PLS/upmichigan_matched.RData')

michigan_ecosystem_matched <- rbind(lowmichigan_ecosystem_matched, upmichigan_ecosystem_matched)

# Check if the soil and PLS are in the same order
identical(MI_topo$pls_x, michigan_ecosystem_matched$x)
identical(MI_topo$pls_y, michigan_ecosystem_matched$y)

# Join the grid ID to the soil dataset
MI_topo_matched <- michigan_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = MI_topo, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(MI_topo_matched$pls_x, michigan_ecosystem_matched$x)
identical(MI_topo_matched$pls_y, michigan_ecosystem_matched$y)

# Summarize over grid cells
MI_topo_grid <- MI_topo_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(elevation = mean(elevation),
                   slope = mean(slope),
                   aspect = mean(aspect))

# Check the number of individual grid cells in the state matches
length(unique(MI_topo_grid$grid_id)) == length(unique(michigan_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
MI_topo_grid <- MI_topo_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
MI_topo_grid <- sf::st_as_sf(MI_topo_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
MI_topo_grid <- sf::st_transform(MI_topo_grid, crs = 'EPSG:4326')
MI_topo_grid <- sfheaders::sf_to_df(MI_topo_grid, fill = TRUE)
MI_topo_grid <- dplyr::select(MI_topo_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'michigan',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- MI_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- MI_topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = elevation), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(MI_topo_grid, file = 'data/processed/topography/gridded_topo_mi.RData')

#### MINNESOTA ####
rm(list = ls())

# Load point-level soil data
load('data/processed/topography/processed_topo_mn.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/minnesota_matched.RData')

# Check if the soil and PLS are in the same order
identical(MN_topo$pls_x, minnesota_ecosystem_matched$x)
identical(MN_topo$pls_y, minnesota_ecosystem_matched$y)

# Join the grid ID to the soil dataset
MN_topo_matched <- minnesota_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = MN_topo, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(MN_topo_matched$pls_x, minnesota_ecosystem_matched$x)
identical(MN_topo_matched$pls_y, minnesota_ecosystem_matched$y)

# Summarize over grid cells
MN_topo_grid <- MN_topo_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(elevation = mean(elevation),
                   slope = mean(slope),
                   aspect = mean(aspect))

# Check the number of individual grid cells in the state matches
length(unique(MN_topo_grid$grid_id)) == length(unique(minnesota_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
MN_topo_grid <- MN_topo_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
MN_topo_grid <- sf::st_as_sf(MN_topo_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
MN_topo_grid <- sf::st_transform(MN_topo_grid, crs = 'EPSG:4326')
MN_topo_grid <- sfheaders::sf_to_df(MN_topo_grid, fill = TRUE)
MN_topo_grid <- dplyr::select(MN_topo_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- MN_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- MN_topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = elevation), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(MN_topo_grid, file = 'data/processed/topography/gridded_topo_mn.RData')

#### WISCONSIN ####
rm(list = ls())

# Load point-level soil data
load('data/processed/topography/processed_topo_wi.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/wisconsin_matched.RData')

# Check if the soil and PLS are in the same order
identical(WI_topo$pls_x, wisconsin_ecosystem_matched$x)
identical(WI_topo$pls_y, wisconsin_ecosystem_matched$y)

# Join the grid ID to the soil dataset
WI_topo_matched <- wisconsin_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = WI_topo, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(WI_topo_matched$pls_x, wisconsin_ecosystem_matched$x)
identical(WI_topo_matched$pls_y, wisconsin_ecosystem_matched$y)

# Summarize over grid cells
WI_topo_grid <- WI_topo_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(elevation = mean(elevation),
                   slope = mean(slope),
                   aspect = mean(aspect))

# Check the number of individual grid cells in the state matches
length(unique(WI_topo_grid$grid_id)) == length(unique(wisconsin_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
WI_topo_grid <- WI_topo_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
WI_topo_grid <- sf::st_as_sf(WI_topo_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
WI_topo_grid <- sf::st_transform(WI_topo_grid, crs = 'EPSG:4326')
WI_topo_grid <- sfheaders::sf_to_df(WI_topo_grid, fill = TRUE)
WI_topo_grid <- dplyr::select(WI_topo_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- WI_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- WI_topo |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = elevation), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(WI_topo_grid, file = 'data/processed/topography/gridded_topo_wi.RData')
