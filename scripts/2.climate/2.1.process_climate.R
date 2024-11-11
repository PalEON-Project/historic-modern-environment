#### STEP 2-1

## Preparing climate drivers downloaded from PRISM
## Climate estimates downloaded from PRISM website in February 2024
## Formatting into dataframe and doing plotting checks

## 1. Loading climate data
## 2. Formatting climate data
## 3. Summarizing climate over time
## 4. Save
## 5. Plotting checks

## Input: .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_ppt_stable_4kmM2_189501_198012_bil/
## PRISM estimates of total monthly precipitation for the period 1895-1915

## Input: .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmean_stable_4kmM3_189501_198012_bil/
## PRISM estimates of mean monthly temperature for the period 1895-1915

## Input: .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmax_stable_4kmM3_189501_198012_bil/
## PRISM estimates of maximum monthly temperature for the period 1895-1915

## Input: .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmin_stable_4kmM3_189501_198012_bil/
## PRISM estimates of minimum monthly temperature for the period 1895-1915

## Input: .bil files in /Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_vpdmax_stable_4kmM3_189501_198012_bil/
## PRISM estimates of maximum monthly vapor pressure deficit for the period 1895-1915

## Output: /Volumes/FileBackup/SDM_bigdata/PRISM/climate_points.RData
## Monthly estimates of climate drivers in the native scale of PRISM data
## This is not used. It is saved so that I don't have to re-run the
## steps of loading in the data because it takes a long time to convert to points

## Output: /Volumes/FileBackup/SDM_bigdata/PRISM/climate_summary.RData
## Temporal summaries of climate variables over months of the year and years
## in this time period, still on the PRISM native resolution
## Used in 2.2.gridded_climate.R

rm(list = ls())

#### 1. Loading climate data ####

# List all files that we want to read in ('bil' files)
ppt_files <- list.files(path = '/Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_ppt_stable_4kmM2_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
tmean_files <- list.files(path = '/Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmean_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
tmax_files <- list.files(path = '/Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmax_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
tmin_files <- list.files(path = '/Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_tmin_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
vpdmax_files <- list.files(path = '/Volumes/FileBackup/SDM_bigdata/PRISM/PRISM_vpdmax_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
all_files <- c(ppt_files, tmean_files, tmax_files, tmin_files, vpdmax_files)

# Stack the files
climate_stack <- raster::stack(all_files)

# Extent of PLS data
ROU <- as(raster::extent(-98, -82, 36.9, 50), 'SpatialPolygons')

# Clip
climate_stack <- raster::crop(climate_stack, ROU)

# Make points from rasters
climate_points <- raster::rasterToPoints(climate_stack)

# Save all points
save(climate_points,
     file = '/Volumes/FileBackup/SDM_bigdata/PRISM/climate_points.RData')

#### 2. Formatting climate data ####

# Re-load saved data
load('/Volumes/FileBackup/SDM_bigdata/PRISM/climate_points.RData')

# Reformat
climate_points <- as.data.frame(climate_points)

# Add coordinates
climate_points <- sf::st_as_sf(climate_points, coords = c('x', 'y'), crs = 'EPSG:4269')

# Reproject to EPSG 3175
climate_points <- sf::st_transform(climate_points, crs = 'EPSG:3175')

# Convert back to regular data frame
climate_points <- sfheaders::sf_to_df(climate_points, fill = TRUE)

# Remove unnecessary columns
climate_points <- dplyr::select(climate_points, -sfg_id, -point_id)

# Format variables and time
climate_long <- climate_points |>
  tidyr::pivot_longer(PRISM_ppt_stable_4kmM2_189501_bil:PRISM_vpdmax_stable_4kmM3_191512_bil,
                      names_to = 'yearmonvar', values_to = 'val') |>
  dplyr::mutate(start = sub(pattern = '_stable.*', replacement = '', x = yearmonvar),
                var = sub(pattern = '.*PRISM_', replacement = '', x = start),
                end = sub(pattern = '.*4kmM', replacement = '', x = yearmonvar),
                year = substring(text = end, first = 3, last = 6),
                month = substring(text = end, first = 7, last = 8),
                year = as.numeric(year),
                month = as.numeric(month)) |>
  dplyr::select(var, year, month, val, x, y) |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val')

#### 3. Summarizing climate over time ####

# Summarize over monthly values for each year and location
clim_annual <- climate_long |>
  dplyr::group_by(x, y, year) |>
  dplyr::summarize(ppt_sum = sum(ppt),
                   ppt_sd = sd(ppt),
                   ppt_cv = ppt_sd / mean(ppt),
                   tmean_mean = mean(tmean),
                   tmean_sd = sd(tmean),
                   tmean_cv = tmean_sd / tmean_mean,
                   tmin = min(tmin),
                   tmax = max(tmax),
                   vpdmax = max(vpdmax))

# Check that mean temperature is never negative
# If it is, you need to redo temperature cv and
# convert temperature to kelvin before calculating cv
any(clim_annual$tmean_mean < 0) # should be FALSE

# Summarize over years for each location
clim_sum <- clim_annual |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(ppt_sum = mean(ppt_sum),
                   ppt_sd = mean(ppt_sd),
                   ppt_cv = mean(ppt_cv),
                   tmean_mean = mean(tmean_mean),
                   tmean_sd = mean(tmean_sd),
                   tmean_cv = mean(tmean_cv),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   vpdmax = mean(vpdmax)) |>
  dplyr::mutate(tmean_cv = dl)

#### 4. Save ####

# Save
save(clim_sum, file = '/Volumes/FileBackup/SDM_bigdata/PRISM/climate_summary.RData')

#### 5. Plotting checks ####

# Plot
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_sum)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_cv)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmean_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmean_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmean_cv)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmin)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = vpdmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

GGally::ggpairs(data = clim_sum, columns = 3:10)
