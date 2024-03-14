## Match climate grid points to 8 km grid

rm(list = ls())

#### ILLINOIS ####

# Load point-level soil data
load('data/processed/climate/processed_climate_il.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/illinois_matched.RData')

# Check if the soil and PLS are in the same order
identical(IL_clim$pls_x, illinois_ecosystem_matched$x)
identical(IL_clim$pls_y, illinois_ecosystem_matched$y)

# Join the grid ID to the soil dataset
IL_clim_matched <- illinois_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = IL_clim, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(IL_clim_matched$pls_x, illinois_ecosystem_matched$x)
identical(IL_clim_matched$pls_y, illinois_ecosystem_matched$y)

# Summarize over grid cells
IL_clim_grid <- IL_clim_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(ppt_mean = mean(ppt_mean),
                   ppt_sd = mean(ppt_sd),
                   tmean_mean = mean(tmean_mean),
                   tmean_sd = mean(tmean_sd),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   vpdmin = mean(vpdmin),
                   vpdmax = mean(vpdmax))

# Check the number of individual grid cells in the state matches
length(unique(IL_clim_grid$grid_id)) == length(unique(illinois_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
IL_clim_grid <- IL_clim_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
IL_clim_grid <- sf::st_as_sf(IL_clim_grid, coords = c('grid_x', 'grid_y'),
                              crs = 'EPSG:3175')
IL_clim_grid <- sf::st_transform(IL_clim_grid, crs = 'EPSG:4326')
IL_clim_grid <- sfheaders::sf_to_df(IL_clim_grid, fill = TRUE)
IL_clim_grid <- dplyr::select(IL_clim_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'illinois',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- IL_clim_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- IL_clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = ppt_mean), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(IL_clim_grid, file = 'data/processed/climate/gridded_clim_il.RData')

#### INDIANA ####
rm(list = ls())

# Load point-level soil data
load('data/processed/climate/processed_climate_in.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/indiana_matched.RData')

# Check if the soil and PLS are in the same order
identical(IN_clim$pls_x, indiana_ecosystem_matched$x)
identical(IN_clim$pls_y, indiana_ecosystem_matched$y)

# Join the grid ID to the soil dataset
IN_clim_matched <- indiana_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = IN_clim, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(IN_clim_matched$pls_x, indiana_ecosystem_matched$x)
identical(IN_clim_matched$pls_y, indiana_ecosystem_matched$y)

# Summarize over grid cells
IN_clim_grid <- IN_clim_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(ppt_mean = mean(ppt_mean),
                   ppt_sd = mean(ppt_sd),
                   tmean_mean = mean(tmean_mean),
                   tmean_sd = mean(tmean_sd),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   vpdmin = mean(vpdmin),
                   vpdmax = mean(vpdmax))

# Check the number of individual grid cells in the state matches
length(unique(IN_clim_grid$grid_id)) == length(unique(indiana_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
IN_clim_grid <- IN_clim_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
IN_clim_grid <- sf::st_as_sf(IN_clim_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
IN_clim_grid <- sf::st_transform(IN_clim_grid, crs = 'EPSG:4326')
IN_clim_grid <- sfheaders::sf_to_df(IN_clim_grid, fill = TRUE)
IN_clim_grid <- dplyr::select(IN_clim_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'indiana',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- IN_clim_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- IN_clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = ppt_mean), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(IN_clim_grid, file = 'data/processed/climate/gridded_clim_in.RData')

#### MICHIGAN ####
rm(list = ls())

# Load point-level soil data
load('data/processed/climate/processed_climate_mi.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/lowmichigan_matched.RData')
load('data/processed/PLS/upmichigan_matched.RData')

michigan_ecosystem_matched <- rbind(lowmichigan_ecosystem_matched, upmichigan_ecosystem_matched)

# Check if the soil and PLS are in the same order
identical(MI_clim$pls_x, michigan_ecosystem_matched$x)
identical(MI_clim$pls_y, michigan_ecosystem_matched$y)

# Join the grid ID to the soil dataset
MI_clim_matched <- michigan_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = MI_clim, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(MI_clim_matched$pls_x, michigan_ecosystem_matched$x)
identical(MI_clim_matched$pls_y, michigan_ecosystem_matched$y)

# Summarize over grid cells
MI_clim_grid <- MI_clim_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(ppt_mean = mean(ppt_mean),
                   ppt_sd = mean(ppt_sd),
                   tmean_mean = mean(tmean_mean),
                   tmean_sd = mean(tmean_sd),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   vpdmin = mean(vpdmin),
                   vpdmax = mean(vpdmax))

# Check the number of individual grid cells in the state matches
length(unique(MI_clim_grid$grid_id)) == length(unique(michigan_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
MI_clim_grid <- MI_clim_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
MI_clim_grid <- sf::st_as_sf(MI_clim_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
MI_clim_grid <- sf::st_transform(MI_clim_grid, crs = 'EPSG:4326')
MI_clim_grid <- sfheaders::sf_to_df(MI_clim_grid, fill = TRUE)
MI_clim_grid <- dplyr::select(MI_clim_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'michigan',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- MI_clim_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- MI_clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = ppt_mean), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(MI_clim_grid, file = 'data/processed/climate/gridded_clim_mi.RData')

#### MINNESOTA ####
rm(list = ls())

# Load point-level soil data
load('data/processed/climate/processed_climate_mn.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/minnesota_matched.RData')

# Check if the soil and PLS are in the same order
identical(MN_clim$pls_x, minnesota_ecosystem_matched$x)
identical(MN_clim$pls_y, minnesota_ecosystem_matched$y)

# Join the grid ID to the soil dataset
MN_clim_matched <- minnesota_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = MN_clim, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(MN_clim_matched$pls_x, minnesota_ecosystem_matched$x)
identical(MN_clim_matched$pls_y, minnesota_ecosystem_matched$y)

# Summarize over grid cells
MN_clim_grid <- MN_clim_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(ppt_mean = mean(ppt_mean),
                   ppt_sd = mean(ppt_sd),
                   tmean_mean = mean(tmean_mean),
                   tmean_sd = mean(tmean_sd),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   vpdmin = mean(vpdmin),
                   vpdmax = mean(vpdmax))

# Check the number of individual grid cells in the state matches
length(unique(MN_clim_grid$grid_id)) == length(unique(minnesota_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
MN_clim_grid <- MN_clim_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
MN_clim_grid <- sf::st_as_sf(MN_clim_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
MN_clim_grid <- sf::st_transform(MN_clim_grid, crs = 'EPSG:4326')
MN_clim_grid <- sfheaders::sf_to_df(MN_clim_grid, fill = TRUE)
MN_clim_grid <- dplyr::select(MN_clim_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- MN_clim_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- MN_clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = ppt_mean), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(MN_clim_grid, file = 'data/processed/climate/gridded_clim_mn.RData')

#### WISCONSIN ####
rm(list = ls())

# Load point-level soil data
load('data/processed/climate/processed_climate_wi.RData')

# Load matched point and gridded PLS data
load('data/processed/PLS/wisconsin_matched.RData')

# Check if the soil and PLS are in the same order
identical(WI_clim$pls_x, wisconsin_ecosystem_matched$x)
identical(WI_clim$pls_y, wisconsin_ecosystem_matched$y)

# Join the grid ID to the soil dataset
WI_clim_matched <- wisconsin_ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = WI_clim, by = c('pls_x', 'pls_y'))

# Check again if the soil and PLS are in the same order
identical(WI_clim_matched$pls_x, wisconsin_ecosystem_matched$x)
identical(WI_clim_matched$pls_y, wisconsin_ecosystem_matched$y)

# Summarize over grid cells
WI_clim_grid <- WI_clim_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(ppt_mean = mean(ppt_mean),
                   ppt_sd = mean(ppt_sd),
                   tmean_mean = mean(tmean_mean),
                   tmean_sd = mean(tmean_sd),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   vpdmin = mean(vpdmin),
                   vpdmax = mean(vpdmax))

# Check the number of individual grid cells in the state matches
length(unique(WI_clim_grid$grid_id)) == length(unique(wisconsin_ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to soil dataframe
WI_clim_grid <- WI_clim_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
WI_clim_grid <- sf::st_as_sf(WI_clim_grid, coords = c('grid_x', 'grid_y'),
                             crs = 'EPSG:3175')
WI_clim_grid <- sf::st_transform(WI_clim_grid, crs = 'EPSG:4326')
WI_clim_grid <- sfheaders::sf_to_df(WI_clim_grid, fill = TRUE)
WI_clim_grid <- dplyr::select(WI_clim_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- WI_clim_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- WI_clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = ppt_mean), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(WI_clim_grid, file = 'data/processed/climate/gridded_clim_wi.RData')
