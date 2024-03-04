## Preparing climate drivers

rm(list = ls())

# List all files that we want ot read in ('bil' files)
ppt_files <- list.files(path = 'data/raw/PRISM/PRISM_ppt_stable_4kmM2_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
tmean_files <- list.files(path = 'data/raw/PRISM/PRISM_tmean_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
tmax_files <- list.files(path = 'data/raw/PRISM/PRISM_tmax_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
tmin_files <- list.files(path = 'data/raw/PRISM/PRISM_tmin_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
vpdmax_files <- list.files(path = 'data/raw/PRISM/PRISM_vpdmax_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
vpdmin_files <- list.files(path = 'data/raw/PRISM/PRISM_vpdmin_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)

# Stack the files
ppt <- raster::stack(ppt_files)
tmean <- raster::stack(tmean_files)
tmax <- raster::stack(tmax_files)
tmin <- raster::stack(tmin_files)
vpdmax <- raster::stack(vpdmax_files)
vpdmin <- raster::stack(vpdmin_files)

# Extent of PLS data
ROU <- as(raster::extent(-98, -82, 37, 50), 'SpatialPolygons')

# Clip
ppt <- raster::crop(ppt, ROU)
tmean <- raster::crop(tmean, ROU)
tmin <- raster::crop(tmin, ROU)
tmax <- raster::crop(tmax, ROU)
vpdmax <- raster::crop(vpdmax, ROU)
vpdmin <- raster::crop(vpdmin, ROU)

# Make points from rasters
ppt <- raster::rasterToPoints(ppt)
tmean <- raster::rasterToPoints(tmean)
tmin <- raster::rasterToPoints(tmin)
tmax <- raster::rasterToPoints(tmax)
vpdmax <- raster::rasterToPoints(vpdmax)
vpdmin <- raster::rasterToPoints(vpdmin)

# Save all points
save(ppt, tmean, tmin, tmax, vpdmax, vpdmin,
     file = 'data/raw/PRISM/climate_points.RData')

# Re-load saved data
load('data/raw/PRISM/climate_points.RData')

# Reformat
ppt <- as.data.frame(ppt)
tmean <- as.data.frame(tmean)
tmin <- as.data.frame(tmin)
tmax <- as.data.frame(tmax)
vpdmax <- as.data.frame(vpdmax)
vpdmin <- as.data.frame(vpdmin)

# Add coordinates
ppt <- sf::st_as_sf(ppt, coords = c('x', 'y'))
tmean <- sf::st_as_sf(tmean, coords = c('x', 'y'))
tmin <- sf::st_as_sf(tmin, coords = c('x', 'y'))
tmax <- sf::st_as_sf(tmax, coords = c('x', 'y'))
vpdmax <- sf::st_as_sf(vpdmax, coords = c('x', 'y'))
vpdmin <- sf::st_as_sf(vpdmin, coords = c('x', 'y'))

# Add current projection
# Currently in GCS_North_American_1983 EPSG 4269
sf::st_crs(ppt) <- sf::st_crs(tmean) <- sf::st_crs(tmin) <-
  sf::st_crs(tmax) <- sf::st_crs(vpdmax) <- sf::st_crs(vpdmin) <-
  'EPSG:4269'

# Reproject to EPSG 4326
ppt <- sf::st_transform(ppt, crs = 'EPSG:4326')
tmean <- sf::st_transform(tmean, crs = 'EPSG:4326')
tmin <- sf::st_transform(tmin, crs = 'EPSG:4326')
tmax <- sf::st_transform(tmax, crs = 'EPSG:4326')
vpdmax <- sf::st_transform(vpdmax, crs = 'EPSG:4326')
vpdmin <- sf::st_transform(vpdmin, crs = 'EPSG:4326')

# Convert back to regular data frame
ppt <- sfheaders::sf_to_df(ppt, fill = TRUE)
tmean <- sfheaders::sf_to_df(tmean, fill = TRUE)
tmin <- sfheaders::sf_to_df(tmin, fill = TRUE)
tmax <- sfheaders::sf_to_df(tmax, fill = TRUE)
vpdmax <- sfheaders::sf_to_df(vpdmax, fill = TRUE)
vpdmin <- sfheaders::sf_to_df(vpdmin, fill = TRUE)

# Remove unnecessary columns
ppt <- dplyr::select(ppt, -c(sfg_id, point_id))
tmean <- dplyr::select(tmean, -c(sfg_id, point_id))
tmin <- dplyr::select(tmin, -c(sfg_id, point_id))
tmax <- dplyr::select(tmax, -c(sfg_id, point_id))
vpdmax <- dplyr::select(vpdmax, -c(sfg_id, point_id))
vpdmin <- dplyr::select(vpdmin, -c(sfg_id, point_id))

# Pivot longer
ppt_long <- ppt |>
  tidyr::pivot_longer(PRISM_ppt_stable_4kmM2_189501_bil:PRISM_ppt_stable_4kmM2_191512_bil,
                      names_to = 'yearmon', values_to = 'ppt') |>
  dplyr::mutate(year = substr(x = yearmon, start = 24, stop = 27),
                month = substr(x = yearmon, start = 28, stop = 29)) |>
  dplyr::select(-yearmon)
tmean_long <- tmean |>
  tidyr::pivot_longer(PRISM_tmean_stable_4kmM3_189501_bil:PRISM_tmean_stable_4kmM3_191512_bil,
                      names_to = 'yearmon', values_to = 'tmean') |>
  dplyr::mutate(year = substr(x = yearmon, start = 26, stop = 29),
                month = substr(x = yearmon, start = 30, stop = 31)) |>
  dplyr::select(-yearmon)
tmin_long <- tmin |>
  tidyr::pivot_longer(PRISM_tmin_stable_4kmM3_189501_bil:PRISM_tmin_stable_4kmM3_191512_bil,
                      names_to = 'yearmon', values_to = 'tmin') |>
  dplyr::mutate(year = substr(x = yearmon, start = 25, stop = 28),
                month = substr(x = yearmon, start = 29, stop = 30)) |>
  dplyr::select(-yearmon)
tmax_long <- tmax |>
  tidyr::pivot_longer(PRISM_tmax_stable_4kmM3_189501_bil:PRISM_tmax_stable_4kmM3_191512_bil,
                      names_to = 'yearmon', values_to = 'tmax') |>
  dplyr::mutate(year = substr(x = yearmon, start = 25, stop = 28),
                month = substr(x = yearmon, start = 29, stop = 30)) |>
  dplyr::select(-yearmon)
vpdmax_long <- vpdmax |>
  tidyr::pivot_longer(PRISM_vpdmax_stable_4kmM3_189501_bil:PRISM_vpdmax_stable_4kmM3_191512_bil,
                      names_to = 'yearmon', values_to = 'vpdmax') |>
  dplyr::mutate(year = substr(x = yearmon, start = 27, stop = 30),
                month = substr(x = yearmon, start = 31, stop = 32)) |>
  dplyr::select(-yearmon)
vpdmin_long <- vpdmin |>
  tidyr::pivot_longer(PRISM_vpdmin_stable_4kmM3_189501_bil:PRISM_vpdmin_stable_4kmM3_191512_bil,
                      names_to = 'yearmon', values_to = 'vpdmin') |>
  dplyr::mutate(year = substr(x = yearmon, start = 27, stop = 30),
                month = substr(x = yearmon, start = 31, stop = 32)) |>
  dplyr::select(-yearmon)

# Join together
clim <- ppt_long |>
  dplyr::full_join(tmean_long, by = c('x', 'y', 'year', 'month')) |>
  dplyr::full_join(tmin_long, by  = c('x', 'y', 'year', 'month')) |>
  dplyr::full_join(tmax_long, by = c('x', 'y', 'year', 'month')) |>
  dplyr::full_join(vpdmax_long, by = c('x', 'y', 'year', 'month')) |>
  dplyr::full_join(vpdmin_long, by = c('x', 'y', 'year', 'month'))

# Summarize over monthly values for each year and location
clim_annual <- clim |>
  dplyr::group_by(x, y, year) |>
  dplyr::summarize(ppt_mean = mean(ppt),
                   ppt_sd = sd(ppt),
                   tmean_mean = mean(tmean),
                   tmean_sd = sd(tmean),
                   tmin = min(tmin),
                   tmax = max(tmax),
                   vpdmin = min(vpdmin),
                   vpdmax = max(vpdmax))

# Summarize over years for each location
clim_sum <- clim_annual |>
  dplyr::group_by(x, y) |>
  dplyr::summarize(ppt_mean = mean(ppt_mean),
                   ppt_sd = mean(ppt_sd),
                   tmean_mean = mean(tmean_mean),
                   tmean_sd = mean(tmean_sd),
                   tmin = mean(tmin),
                   tmax = mean(tmax),
                   vpdmin = mean(vpdmin),
                   vpdmax = mean(vpdmax))

# Save
save(clim_sum, file = 'data/raw/PRISM/climate_summary.RData')

# Plot
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = ppt_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmean_mean)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmean_sd)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmin)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = tmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = vpdmin)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim_sum |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = vpdmax)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

GGally::ggpairs(data = clim_sum, columns = 3:10)
