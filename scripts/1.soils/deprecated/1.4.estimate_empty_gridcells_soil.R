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
load('data/processed/soils/gridded_soil.RData')

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

# Reproject
new_grid <- sf::st_transform(new_grid, crs = 'EPSG:4326')

# Convert to regular dataframe
new_grid <- sfheaders::sf_to_df(new_grid, fill = TRUE)

# Reformat
new_grid <- new_grid |>
  dplyr::rename(grid_id = `p$id`) |>
  dplyr::select(grid_id, clay, sand, silt, caco3, awc, x, y)

all(colnames(soil_grid) == colnames(new_grid))

# Combine
soil_grid <- rbind(soil_grid, new_grid)

save(soil_grid, file = 'data/processed/soils/gridded_soil.RData')
