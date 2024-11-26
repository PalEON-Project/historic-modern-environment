#### STEP 2-4

## Aggregating modern climate estimates to PalEON grid

## 1. Load data
## 2. Unique grid cells
## 3. Assign climate to grid cells
## 4. Plotting check
## 5. Save

## Input: /Volumes/FileBackup/SDM_bigdata/PRISM_modern/climate_summary.RData
##        OR data/intermediate/climate_summary_modern.RData
## Dataframe with modern temporal climate summaries at native PRISM resolution

## Input: ~/Google Drive 2/environ-veg-prediction/data/processed/FIA/gridded_all_plots.RData
##        OR data/processed/FIA/gridded_all_plots.RData
## Dataframe with 8 x 8 km grid cells in the modern time period
## This is from the repository https://github.com/amwillson/environ-veg-prediction/

## Input: ~/Google Drive 2/environ-veg-prediction/data/processed/PLS/gridded_fcomp_density.RData
##        OR data/processed/PLS/gridded_fcomp_density.RData
## Dataframe with 8 x 8 km grid cells in the historic time period
## This is from the repository https://github.com/amwillson/environ-veg-prediction

## Output: data/processed/climate/gridded_climate_modern.Rdata
## Dataframe of modern climate estimates aggregate to the PalEON 8 x 8 km grid
## Used in the repository https://github.com/amwillson/environ-veg-prediction/
## and for other PalEON projects

rm(list = ls())

#### 1. Load data ####

# Load climate estimates
# Either change external directory or from local save
if(!file.exists('/Volumes/FileBackup/SDM_bigdata/PRISM_modern/')){
  load('data/intermediate/climate_summary_modern.RData')
}else{
  load('/Volumes/FileBackup/SDM_bigdata/PRISM_modern/climate_summary.RData')
}

## The following variables are available at
## https://github.com/amwillson/environ-veg-prediction/data/processed/FIA
## and https://github.com/amwillson/environ-veg-prediction/data/processed/PLS
## Instead of loading from the external path, you can add them
## to the data/processed/ path and load directly from there

# Load gridded FIA data
if(file.exists('data/processed/FIA/')){
  load('data/processed/FIA/gridded_all_plots.RData')
}else{
  load('~/Google Drive 2/environ-veg-prediction/data/processed/FIA/gridded_all_plots.RData')
}
# Load gridded PLS data
if(file.exists('data/processed/PLS/')){
  load('data/processed/PLS/gridded_fcomp_density.RData')
}else{
  load('~/Google Drive 2/environ-veg-prediction/data/processed/PLS/gridded_fcomp_density.RData')
}

#### 2. Unique grid cells ####

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

# Climate coords
clim_coords <- clim_sum |>
  dplyr::select(x, y) |>
  dplyr::distinct()

# Vegetation coords
veg_coords <- veg_unique_grid |>
  dplyr::select(x, y)

#### 3. Assign climate to grid cells ####

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
  
  # Find climate reconstructions occurring within the grid cell
  sub2 <- dplyr::filter(clim_sum,
                        x >= xmin & x <= xmax &
                          y >= ymin & y <= ymax)
  
  # Summary climate variables for all reconstructinos within the grid cell
  veg_unique_grid$ppt_sum[i] <- mean(sub2$ppt_sum)
  veg_unique_grid$tmean_mean[i] <- mean(sub2$tmean_mean)
  veg_unique_grid$ppt_sd[i] <- mean(sub2$ppt_sd)
  veg_unique_grid$ppt_cv[i] <- mean(sub2$ppt_cv)
  veg_unique_grid$tmean_sd[i] <- mean(sub2$tmean_sd)
  veg_unique_grid$tmean_cv[i] <- mean(sub2$tmean_cv)
  veg_unique_grid$tmin[i] <- mean(sub2$tmin)
  veg_unique_grid$tmax[i] <- mean(sub2$tmax)
  veg_unique_grid$vpdmax[i] <- mean(sub2$vpdmax)
  
  print(i)
}

#### 4. Plotting checks ####

# State outlines
states <- sf::st_as_sf(maps::map(database = 'state',
                                 region = c('illinois', 'indiana',
                                            'michigan', 'minnesota',
                                            'wisconsin'),
                                 plot = FALSE, fill = TRUE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

## Plot climate variables to ensure geograhpical patterns are consistent
## with the plotted patterns from step 1

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ppt_sum)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'D',
                                direction = -1,
                                name = 'mm/year') +
  ggplot2::ggtitle('Total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmean_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'A',
                                name = '°C') +
  ggplot2::ggtitle('Average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ppt_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'B',
                                name = 'mm/year') + 
  ggplot2::ggtitle('Standard deviation of total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  dplyr::mutate(ppt_cv = ppt_cv * 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = ppt_cv)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'B',
                                name = '%') +
  ggplot2::ggtitle('Coefficient of variation of total annual precipitation') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmean_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'C',
                                name = '°C') +
  ggplot2::ggtitle('Standard deviation of average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  dplyr::mutate(tmean_cv = tmean_cv * 100) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmean_cv)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'C',
                                name = '%') +
  ggplot2::ggtitle('Coefficient of variation of average annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmin)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'E',
                                name = '°C') +
  ggplot2::ggtitle('Minimum annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = tmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'F',
                                name = '°C') +
  ggplot2::ggtitle('Maximum annual temperature') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

veg_unique_grid |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = states, color = NA, fill = 'grey85') +
  ggplot2::geom_tile(ggplot2::aes(x = x, y = y, fill = vpdmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::scale_fill_viridis_c(option = 'G',
                                name = 'hPa') +
  ggplot2::ggtitle('Maximum annual vapor pressure deficit') +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### 5. Save ####

# Save
save(veg_unique_grid,
     file = 'data/processed/climate/gridded_climate_modern.Rdata')
