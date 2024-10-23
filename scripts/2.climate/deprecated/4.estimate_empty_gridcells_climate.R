## Estimate climate covariates for PLS grid cells with no corresponding PLS points

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

# Load raw climate data
load('data/raw/PRISM/climate_summary.RData')

# Reproject
df_clim <- sf::st_as_sf(clim_sum, coords = c('x', 'y'),
                         crs = 'EPSG:4326')
df_clim <- sf::st_transform(df_clim, crs = 'EPSG:3175')
df_clim <- sfheaders::sf_to_df(df_clim, fill = TRUE)
df_clim <- dplyr::select(df_clim, -sfg_id, -point_id)

# Load gridded climate data
# We will use this to interpolate when there are no soil
# data points available for a grid cell
# (This is very rare and basically limited to some large lakes)
load('data/processed/climate/gridded_climate.RData')

# Loop over grid cells that don't have points
for(i in 1:nrow(nopoints)){
  # Grid cell
  p <- nopoints[i,]
  # Raw soil data points that are within that grid cell
  sub <- df_clim |>
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
    closest_clim <- dplyr::filter(clim_grid, grid_id %in% closest_gridcells)
    # Do some transformations to keep naming conventions consistent
    sub <- closest_clim |>
      dplyr::select(-grid_id)
  }
  # Average over all points within grid cell
  # or soil values from adjacent grid cells
  summ <- sub |>
    dplyr::summarize(ppt_mean = mean(ppt_mean),
                     ppt_sd = mean(ppt_sd),
                     tmean_mean = mean(tmean_mean),
                     tmean_sd = mean(tmean_sd),
                     tmin = mean(tmin),
                     tmax = mean(tmax),
                     vpdmin = mean(vpdmin),
                     vpdmax = mean(vpdmax))
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
  dplyr::select(grid_id, tmean_mean, tmean_sd,
                ppt_mean, ppt_sd, tmin, tmax,
                vpdmin, vpdmax, x, y)

all(colnames(clim_grid) == colnames(new_grid))

# Combine
clim_grid <- rbind(clim_grid, new_grid)

# Save
save(clim_grid, file = 'data/processed/climate/gridded_climate.RData')