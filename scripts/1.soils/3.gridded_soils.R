## Matching ggSSURGO and PLS spatially

rm(list = ls())

# Load soil data
load('data/raw/soils/gssurgo_average_030_700m.RData')

# Combine soil estimates for all states
df_soil <- rbind(df_IL, df_IN, df_MI, df_MN, df_WI)

# Load floodplain data
load('data/raw/soils/gssurgo_floodplain_030_700m.RData')

# Combine floodplain estimates for all states
df_flood <- rbind(df_IL, df_IN, df_MI, df_MN, df_WI)

# Outline of states
states <- sf::st_as_sf(maps::map(database = 'state',
                                 regions = c('illinois', 'indiana',
                                             'michigan', 'minnesota',
                                             'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Plot to make sure we have the correct CRS for the soil estimates
df_soil |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = claytotal_r), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

df_flood |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = Floodplain), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Transform
df_soil <- sf::st_as_sf(df_soil,
                        coords = c('x', 'y'),
                        crs = 'EPSG:4326')
df_soil <- sf::st_transform(df_soil, crs = 'EPSG:3175')
df_soil <- sfheaders::sf_to_df(df_soil, fill = TRUE)
df_soil <- dplyr::select(df_soil, -sfg_id, -point_id)

df_flood <- sf::st_as_sf(df_flood,
                         coords = c('x', 'y'),
                         crs = 'EPSG:4326')
df_flood <- sf::st_transform(df_flood, crs = 'EPSG:3175')
df_flood <- sfheaders::sf_to_df(df_flood, fill = TRUE)
df_flood <- dplyr::select(df_flood, -sfg_id, -point_id)

# Load gridded FIA data from other repository
load('~/Google Drive 2/environ-veg-prediction/data/processed/FIA/gridded_all_plots.RData')

# Load gridded PLS data from other repository
load('~/Google Drive 2/environ-veg-prediction/data/processed/PLS/gridded_fcomp_density.RData')

# Take unique grid cells from both
# (There are some grid cells in FIA that are not in PLS
# because they are on political boundaries)
fia_unique_grid <- density_fcomp_df |>
  dplyr::select(x, y) |>
  dplyr::mutate(loc = paste0(x, '_', y)) |>
  dplyr::distinct()

pls_unique_grid <- stem_density_agg2 |>
  dplyr::ungroup() |>
  dplyr::select(x, y) |>
  dplyr::mutate(loc = paste0(x, '_', y)) |>
  dplyr::distinct()

# Combine
veg_unique_grid <- rbind(fia_unique_grid, pls_unique_grid)

# Remove duplicate grid cells
veg_unique_grid <- dplyr::distinct(veg_unique_grid)

# Soil coords
soil_coords <- df_soil |>
  dplyr::select(x, y) |>
  dplyr::distinct()

# Floodplain coords
flood_coords <- df_flood |>
  dplyr::select(x, y) |>
  dplyr::distinct()

# Vegetation coords
veg_coords <- veg_unique_grid |>
  dplyr::select(x, y)

# For each veg grid cell, find all climate points occurring within
for(i in 1:nrow(veg_unique_grid)){
  # Take one grid cell's ID and coordinates
  sub <- veg_unique_grid[i,]
  
  # Find extent of grid cell, given coordinates are centroids
  # and in meters
  xmin <- sub$x - 4000
  xmax <- sub$x + 4000
  ymin <- sub$y - 4000
  ymax <- sub$y + 4000
  
  # Find soil estimates occurring within the grid cell
  sub2 <- dplyr::filter(df_soil,
                        x >= xmin & x <= xmax &
                          y >= ymin & y <= ymax)
  
  # Find floodplain estimates occurring within the grid cell
  sub3 <- dplyr::filter(df_flood,
                        x >= xmin & x <= xmax &
                          y >= ymin & y <= ymax)
  
  # Summary climate variables for all reconstructions within the grid cell
  veg_unique_grid$clay[i] <- mean(sub2$claytotal_r, na.rm = TRUE)
  veg_unique_grid$sand[i] <- mean(sub2$sandtotal_r, na.rm = TRUE)
  veg_unique_grid$silt[i] <- mean(sub2$silttotal_r, na.rm = TRUE)
  veg_unique_grid$caco3[i] <- mean(sub2$caco3_r, na.rm = TRUE)
  veg_unique_grid$awc[i] <- mean(sub2$awc_r, na.rm = TRUE)
  veg_unique_grid$flood[i] <- length(which(sub3$Floodplain == 'Yes')) / nrow(sub3)
  
  print(i)
}

## Plot soil variables to ensure geographical patterns make sense
## given our understanding of soil properties in this region

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = clay)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = '% clay') +
  ggplot2::ggtitle('Soil % clay') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = '% sand') +
  ggplot2::ggtitle('Soil % sand') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = silt)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = '% silt') +
  ggplot2::ggtitle('Soil % silt') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = caco3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = expression(paste('% CaC', O[3]))) +
  ggplot2::ggtitle('Soil calcium carbonate concentration') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = awc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = 'cm/cm') +
  ggplot2::ggtitle('Soil available water content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = flood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                name = 'Fration grid cell') +
  ggplot2::ggtitle('Proportion of grid cell in a floodplain') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Grid cells with no data
nodata <- dplyr::filter(veg_unique_grid, is.na(clay))

# For each grid cell with no data
for(i in 1:nrow(nodata)){
  # Row number in full dataset
  rowid <- which(veg_unique_grid$loc == nodata$loc[i])
  
  # All data except that cell
  sub <- veg_unique_grid |>
    dplyr::filter(loc != rowid) |>
    tidyr::drop_na()
  
  # Grid cell x and y coordinates
  p_xy <- dplyr::select(nodata[i,], x, y)
  
  sub_xy <- dplyr::select(sub, x, y)
  
  # Find closest grid cell that does have data
  closest_cell <- fields::rdist(p_xy, sub_xy)
  closest_cell <- apply(closest_cell, 1, which.min)

  # Soil data
  veg_unique_grid[rowid,4] <- sub$clay[closest_cell]
  veg_unique_grid[rowid,5] <- sub$sand[closest_cell]
  veg_unique_grid[rowid,6] <- sub$silt[closest_cell]
  veg_unique_grid[rowid,7] <- sub$caco3[closest_cell]
  veg_unique_grid[rowid,8] <- sub$awc[closest_cell]
  veg_unique_grid[rowid,9] <- sub$flood[closest_cell]
  
  print(i)
}

# Same plots again to see if gaps were filled
veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = clay)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = '% clay') +
  ggplot2::ggtitle('Soil % clay') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = sand)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = '% sand') +
  ggplot2::ggtitle('Soil % sand') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = silt)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = '% silt') +
  ggplot2::ggtitle('Soil % silt') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = caco3)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = expression(paste('% CaC', O[3]))) +
  ggplot2::ggtitle('Soil calcium carbonate concentration') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = awc)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Oranges',
                                name = 'cm/cm') +
  ggplot2::ggtitle('Soil available water content') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = flood)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_distiller(palette = 'Blues',
                                name = 'Fraction grid cell') +
  ggplot2::ggtitle('Proportion of grid cell in a floodplain') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

# Save
save(veg_unique_grid,
     file = 'data/processed/soils/gridded_soil.RData')
