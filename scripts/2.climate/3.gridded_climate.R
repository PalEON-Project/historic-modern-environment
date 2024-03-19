## Match climate grid points to 8 km grid

rm(list = ls())

# Load point-level climate data
load('data/processed/climate/processed_climate_il.RData')
load('data/processed/climate/processed_climate_in.RData')
load('data/processed/climate/processed_climate_mi.RData')
load('data/processed/climate/processed_climate_mn.RData')
load('data/processed/climate/processed_climate_wi.RData')

# Check if columns are all the same
all(colnames(IL_clim) == colnames(IN_clim))
all(colnames(IN_clim) == colnames(MI_clim))
all(colnames(MI_clim) == colnames(MN_clim))
all(colnames(MN_clim) == colnames(WI_clim))

# Combine
clim <- rbind(IL_clim, IN_clim, MI_clim, MN_clim, WI_clim)

# Load matched point and gridded PLS data
load('data/processed/PLS/total_matched.RData')

# Join the grid ID to the climate dataset
clim_matched <- ecosystem_matched |>
  dplyr::select(x, y, grid_id) |>
  dplyr::rename(pls_x = x,
                pls_y = y) |>
  dplyr::right_join(y = clim, by = c('pls_x', 'pls_y'))

# Check if the climate and PLS are in the same order
identical(clim_matched$pls_x, ecosystem_matched$x)
identical(clim_matched$pls_y, ecosystem_matched$y)

# Summarize over grid cells
clim_grid <- clim_matched |>
  dplyr::group_by(grid_id) |>
  dplyr::summarize(tmean_mean = mean(tmean_mean),
                   tmean_sd = mean(tmean_sd),
                   ppt_mean = mean(ppt_mean),
                   ppt_sd = mean(ppt_sd),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   vpdmin = mean(vpdmin),
                   vpdmax = mean(vpdmax))

# Check the number of individual grid cells matches
length(unique(clim_grid$grid_id)) == length(unique(ecosystem_matched$grid_id))

# Load intermediate grid matching
load('data/processed/PLS/matching_intermediate.RData')

# Keep only the grid ID numbers and their coordinates
grid_coords <- point_grid_match |>
  dplyr::select(grid_x, grid_y, grid_id) |>
  dplyr::distinct()

# Add grid lat/long to climate dataframe
clim_grid <- clim_grid |>
  dplyr::left_join(y = grid_coords, by = 'grid_id')

# Re-project to EPSG:4326
clim_grid <- sf::st_as_sf(clim_grid, coords = c('grid_x', 'grid_y'),
                          crs = 'EPSG:3175')
clim_grid <- sf::st_transform(clim_grid, crs = 'EPSG:4326')
clim_grid <- sfheaders::sf_to_df(clim_grid, fill = TRUE)
clim_grid <- dplyr::select(clim_grid, -sfg_id, -point_id)

# Plot to make sure the aggregation is correct
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

grid <- clim_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()
point <- clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = ppt_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

cowplot::plot_grid(grid, point, nrow = 1)

save(clim_grid, file = 'data/processed/climate/gridded_climate.RData')
