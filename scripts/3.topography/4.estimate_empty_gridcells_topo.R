## Estimate topographic covariates for PLS grid cells with no corresponding PLS points

rm(list = ls())

# Load total PLS dataset
load('data/processed/PLS/total_matched.RData')

# All grid cells across domain
grid <- unique(ecosystem_matched$grid_id)

# Gridded PLS data products
load('data/processed/PLS/8km.RData')

# Which grid cells have no points in them?
nopoints <- comp_dens[!comp_dens$id %in% grid,]

# Find extent of each grid cell with no points
nopoints <- nopoints |>
  dplyr::mutate(x_min = x - 4000,
                x_max = x + 4000,
                y_min = y - 4000,
                y_max = y + 4000)

# Load raw topography data
load('data/raw/topography/il_topo.RData')
load('data/raw/topography/in_topo.RData')
load('data/raw/topography/mi_topo.RData')
load('data/raw/topography/mn_topo.RData')
load('data/raw/topography/wi_topo.RData')

# Combine
df_topo <- rbind(il_topo, in_topo, mi_topo, mn_topo, wi_topo)

# Remove negative points
df_topo <- dplyr::filter(df_topo, elevation >= 0)

# Reproject
df_topo <- sf::st_as_sf(df_topo, coords = c('x', 'y'),
                        crs = 'EPSG:4326')
df_topo <- sf::st_transform(df_topo, crs = 'EPSG:3175')
df_topo <- sfheaders::sf_to_df(df_topo, fill = TRUE)
df_topo <- dplyr::select(df_topo, -sfg_id, -point_id)

# Load gridded climate data
# We will use this to interpolate when there are no soil
# data points available for a grid cell
# (This is very rare and basically limited to some large lakes)
load('data/processed/topography/gridded_topo_il.RData')
load('data/processed/topography/gridded_topo_in.RData')
load('data/processed/topography/gridded_topo_mi.RData')
load('data/processed/topography/gridded_topo_mn.RData')
load('data/processed/topography/gridded_topo_wi.RData')

# Combine gridded soil data
topo_grid <- rbind(IL_topo_grid, IN_topo_grid, MI_topo_grid, MN_topo_grid, WI_topo_grid)

# Loop over grid cells that don't have points
for(i in 1:nrow(nopoints)){
  # Grid cell
  p <- nopoints[i,]
  # Raw soil data points that are within that grid cell
  sub <- df_topo |>
    dplyr::filter(x >= p$x_min & x <= p$x_max &
                    y >= p$y_min & y <= p$y_max)
  
  # If there are no soil data points within that grid cell,
  # make the grid cell soil values an average of the closest points
  if(nrow(sub) == 0){
    # Grid cell x and y coordinates
    p_xy <- c(p$x, p$y)
    # Gridded soil products x and y coordinates
    grid_xy <- dplyr::select(comp_dens, x, y)
    # Find distance between the grid cell of interest and all grid cells with soil data already
    closest_gridcells <- fields::rdist(p_xy, grid_xy)
    # Find the closest grid cells to the one lacking data
    closest_gridcells <- apply(closest_gridcells, 1, which.min)
    # Make sure our IDs are consistent
    closest_gridcells <- comp_dens$id[closest_gridcells]
    # Get soil data for  closest grid cells
    closest_topo <- dplyr::filter(topo_grid, grid_id %in% closest_gridcells)
    # Do some transformations to keep naming conventions consistent
    sub <- closest_topo |>
      dplyr::select(-grid_id)
  }
  # Average over all points within grid cell
  # or soil values from adjacent grid cells
  summ <- sub |>
    dplyr::summarize(elevation = mean(elevation),
                     slope = mean(slope, na.rm = TRUE),
                     aspect = mean(aspect, na.rm = TRUE))
  # Save
  if(i == 1){
    summ_xy <- cbind(p$x, p$y, p$id, summ)
  }else{
    temp <- cbind(p$x, p$y, p$id, summ)
    summ_xy <- rbind(summ_xy, temp)
  }
  # Progress
  print(i)
}

# Make output a spatial object
new_grid <- sf::st_as_sf(summ_xy, coords = c('p$x', 'p$y'),
                         crs = 'EPSG:3175')

# Map of states
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Add state to data
new_grid <- new_grid |>
  sf::st_join(states)

# Mannually add state for some points on the border
new_grid$ID[1] <- 'illinois'
new_grid$ID[7] <- 'illinois'
new_grid$ID[8] <- 'illinois'
new_grid$ID[10] <- 'illinois'
new_grid$ID[26] <- 'indiana'
new_grid$ID[177] <- 'indiana'
new_grid$ID[436] <- 'illinois'
new_grid$ID[473] <- 'indiana'
new_grid$ID[494] <- 'indiana'
new_grid$ID[616] <- 'illinois'
new_grid$ID[669] <- 'michigan'
new_grid$ID[670] <- 'michigan'
new_grid$ID[671] <- 'michigan'
new_grid$ID[672] <- 'michigan'
new_grid$ID[673] <- 'michigan'
new_grid$ID[674] <- 'michigan'
new_grid$ID[723] <- 'michigan'
new_grid$ID[766] <- 'minnesota'
new_grid$ID[770] <- 'minnesota'
new_grid$ID[771] <- 'minnesota'
new_grid$ID[772] <- 'minnesota'
new_grid$ID[773] <- 'minnesota'
new_grid$ID[774] <- 'minnesota'
new_grid$ID[775] <- 'minnesota'
new_grid$ID[776] <- 'minnesota'
new_grid$ID[777] <- 'minnesota'

# Reproject
new_grid <- sf::st_transform(new_grid, crs = 'EPSG:4326')

# Convert to regular dataframe
new_grid <- sfheaders::sf_to_df(new_grid, fill = TRUE)
# Reformat
new_grid <- new_grid |>
  dplyr::rename(state = ID,
                grid_id = `p$id`) |>
  dplyr::select(state, grid_id, elevation, slope, aspect, x, y)

# Divide by state because that's how the soil data are stored
IL_new_grid <- new_grid |>
  dplyr::filter(state == 'illinois') |>
  dplyr::select(-state)
IN_new_grid <- new_grid |>
  dplyr::filter(state == 'indiana') |>
  dplyr::select(-state)
MI_new_grid <- new_grid |>
  dplyr::filter(state == 'michigan') |>
  dplyr::select(-state)
MN_new_grid <- new_grid |>
  dplyr::filter(state == 'minnesota') |>
  dplyr::select(-state)
WI_new_grid <- new_grid |>
  dplyr::filter(state == 'wisconsin') |>
  dplyr::select(-state)

# Add the grid cells without points to our already-formatted
# gridded soil data
IL_topo_grid <- rbind(IL_topo_grid, IL_new_grid)
IN_topo_grid <- rbind(IN_topo_grid, IN_new_grid)
MI_topo_grid <- rbind(MI_topo_grid, MI_new_grid)
MN_topo_grid <- rbind(MN_topo_grid, MN_new_grid)
WI_topo_grid <- rbind(WI_topo_grid, WI_new_grid)

# Plot to make sure it looks good for each state
il <- sf::st_as_sf(maps::map('state', region = 'illinois',
                             fill = TRUE, plot = FALSE))
il <- sf::st_transform(il, crs = 'EPSG:4326')

IL_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = il, color = 'black', fill = NA)

IN <- sf::st_as_sf(maps::map('state', region = 'indiana',
                             fill = TRUE, plot = FALSE))
IN <- sf::st_transform(IN, crs = 'EPSG:4326')

IN_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = IN, color = 'black', fill = NA)

mi <- sf::st_as_sf(maps::map('state', region = 'michigan',
                             fill = TRUE, plot = FALSE))
mi <- sf::st_transform(mi, crs = 'EPSG:4326')

MI_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = mi, color = 'black', fill = NA)

mn <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                             fill = TRUE, plot = FALSE))
mn <- sf::st_transform(mn, crs = 'EPSG:4326')

MN_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = mn, color = 'black', fill = NA)

wi <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                             fill = TRUE, plot = FALSE))
wi <- sf::st_transform(wi, crs = 'EPSG:4326')

WI_topo_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = wi, color = 'black', fill = NA)

save(IL_topo_grid, file = 'data/processed/topography/gridded_topo_il.RData')
save(IN_topo_grid, file = 'data/processed/topography/gridded_topo_in.RData')
save(MI_topo_grid, file = 'data/processed/topography/gridded_topo_mi.RData')
save(MN_topo_grid, file = 'data/processed/topography/gridded_topo_mn.RData')
save(WI_topo_grid, file = 'data/processed/topography/gridded_topo_wi.RData')
