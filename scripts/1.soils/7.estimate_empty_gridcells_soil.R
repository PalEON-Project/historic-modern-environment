## Estimate soil covariates for PLS grid cells with no corresponding PLS points

rm(list = ls())

# Load total PLS dataset
load('data/processed/PLS/total_matched.RData')

# All grid cells across domain
grid <- unique(ecosystem_matched$grid_id)

# Gridded PLS data products
load('data/processed/PLS/8km.RData')

# Which grid cells have no points in them?
nopoints <- comp_dens[!comp_dens$id %in% grid,]

# Plot of states
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

# Plot locations of grid cells with no points
nopoints |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA)

# All these locations make sense. They are where we don't have point-level records
# digitized in Indiana and Illinois, where we don't have data near Detroit,
# and locations of things like large lakes and Native American reservations in other areas
# We simply need to fill in these grid cells with environmental data

# Find extent of each grid cell with no points
nopoints <- nopoints |>
  dplyr::mutate(x_min = x - 4000,
                x_max = x + 4000,
                y_min = y - 4000,
                y_max = y + 4000)

# Load raw soil data
load('data/raw/soils/gssurgo_average_030_700m.RData')

# Combine all states
df_soils <- rbind(df_IL, df_IN, df_MI, df_MN, df_WI)

# Reproject
df_soils <- sf::st_as_sf(df_soils, coords = c('x', 'y'),
                         crs = 'EPSG:4326')
df_soils <- sf::st_transform(df_soils, crs = 'EPSG:3175')
df_soils <- sfheaders::sf_to_df(df_soils, fill = TRUE)
df_soils <- dplyr::select(df_soils, -sfg_id, -point_id)

# Remove missing values
df_soils <- tidyr::drop_na(df_soils)

# Load gridded soil data
# We will use this to interpolate when there are no soil
# data points available for a grid cell
# (This is very rare and basically limited to some large lakes)
load('data/processed/soils/gridded_soil_il.RData')
load('data/processed/soils/gridded_soil_in.RData')
load('data/processed/soils/gridded_soil_mi.RData')
load('data/processed/soils/gridded_soil_mn.RData')
load('data/processed/soils/gridded_soil_wi.RData')

# Combine gridded soil data
soil_grid <- rbind(IL_soil_grid, IN_soil_grid, MI_soil_grid, MN_soil_grid, WI_soil_grid)

# Loop over grid cells that don't have points
for(i in 1:nrow(nopoints)){
  # Grid cell
  p <- nopoints[i,]
  # Raw soil data points that are within that grid cell
  sub <- df_soils |>
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
    closest_soil <- dplyr::filter(soil_grid, grid_id %in% closest_gridcells)
    # Do some transformations to keep naming conventions consistent
    sub <- closest_soil |>
      dplyr::select(-grid_id) |>
      dplyr::rename(claytotal_r = clay,
                    sandtotal_r = sand,
                    silttotal_r = silt,
                    caco3_r = caco3,
                    awc_r = awc)
  }
  # Average over all points within grid cell
  # or soil values from adjacent grid cells
  summ <- sub |>
    dplyr::summarize(clay = mean(claytotal_r),
                     sand = mean(sandtotal_r),
                     silt = mean(silttotal_r),
                     caco3 = mean(caco3_r),
                     awc = mean(awc_r))
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
  dplyr::select(state, grid_id, clay, sand, silt, caco3, awc, x, y)

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
IL_soil_grid <- rbind(IL_soil_grid, IL_new_grid)
IN_soil_grid <- rbind(IN_soil_grid, IN_new_grid)
MI_soil_grid <- rbind(MI_soil_grid, MI_new_grid)
MN_soil_grid <- rbind(MN_soil_grid, MN_new_grid)
WI_soil_grid <- rbind(WI_soil_grid, WI_new_grid)

# Plot to make sure it looks good for each state
il <- sf::st_as_sf(maps::map('state', region = 'illinois',
                             fill = TRUE, plot = FALSE))
il <- sf::st_transform(il, crs = 'EPSG:4326')

IL_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = il, color = 'black', fill = NA)

IN <- sf::st_as_sf(maps::map('state', region = 'indiana',
                             fill = TRUE, plot = FALSE))
IN <- sf::st_transform(IN, crs = 'EPSG:4326')

IN_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = IN, color = 'black', fill = NA)

mi <- sf::st_as_sf(maps::map('state', region = 'michigan',
                             fill = TRUE, plot = FALSE))
mi <- sf::st_transform(mi, crs = 'EPSG:4326')

MI_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = mi, color = 'black', fill = NA)

mn <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                             fill = TRUE, plot = FALSE))
mn <- sf::st_transform(mn, crs = 'EPSG:4326')

MN_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = mn, color = 'black', fill = NA)

wi <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                             fill = TRUE, plot = FALSE))
wi <- sf::st_transform(wi, crs = 'EPSG:4326')

WI_soil_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = sand)) +
  ggplot2::geom_sf(data = wi, color = 'black', fill = NA)

save(IL_soil_grid, file = 'data/processed/soils/gridded_soil_il.RData')
save(IN_soil_grid, file = 'data/processed/soils/gridded_soil_in.RData')
save(MI_soil_grid, file = 'data/processed/soils/gridded_soil_mi.RData')
save(MN_soil_grid, file = 'data/processed/soils/gridded_soil_mn.RData')
save(WI_soil_grid, file = 'data/processed/soils/gridded_soil_wi.RData')
