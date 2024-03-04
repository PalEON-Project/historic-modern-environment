## Downloading DEM and calcualting elevation and aspect
rm(list = ls())

#### ILLINOIS ####
# Load PLS data with locations
load('data/processed/PLS/illinois_format.RData')

# Extract location from PLS data
latlong <- dplyr::select(illinois_ecosystem, x, y)
latlong <- sf::st_as_sf(latlong, coords = c('x', 'y'), crs = 'EPSG:4326')

# Get raster
# location specified from PLS data
# source = amazon web services
# z = 7 to match spatial resolution of gssurgo data
# Output is in EPSG:4326
il_elev <- elevatr::get_elev_raster(locations = latlong, src = 'aws', z = 7)

# calculate slope and aspect
il_slope <- raster::terrain(il_elev, opt = 'slope', unit = 'degrees')
il_aspect <- raster::terrain(il_elev, opt = 'aspect', unit = 'degrees')

# Make points from rasters
il_elev <- raster::rasterToPoints(il_elev)
il_slope <- raster::rasterToPoints(il_slope)
il_aspect <- raster::rasterToPoints(il_aspect)

# Reformat
il_elev <- as.data.frame(il_elev)
il_slope <- as.data.frame(il_slope)
il_aspect <- as.data.frame(il_aspect)

# Join elevation, slope and aspect
il_topo <- il_elev |>
  dplyr::left_join(y = il_slope, by = c('x', 'y')) |>
  dplyr::left_join(y = il_aspect, by = c('x', 'y'))

# Rename columns
colnames(il_topo) <- c('x', 'y', 'elevation', 'slope', 'aspect')

# Map of state
states <- sf::st_as_sf(maps::map('state', region = c('illinois'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Plots to make sure things look correct
il_topo |>
  dplyr::filter(x > -91.50136 & x < -87.49638 & 
                  y > 37.00161 & y < 42.50774) |>
  dplyr::filter(elevation > 0) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

il_topo |>
  dplyr::filter(x > -91.50136 & x < -87.49638 &
                  y > 37.00161 & y < 42.50774) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = slope)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::scale_color_continuous(na.value = 'grey')

il_topo |>
  dplyr::filter(x > -91.50136 & x < -87.49638 &
                  y > 37.00161 & y < 42.50774) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aspect)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::scale_color_continuous(na.value = 'grey')

# Save
save(il_topo, file = 'data/raw/topography/il_topo.RData')

#### INDIANA ####
rm(list = ls())

# Load PLS data with locations
load('data/processed/PLS/indiana_format.RData')

# Extract locations from PLS data
latlong <- dplyr::select(indiana_ecosystem, x, y)
latlong <- sf::st_as_sf(latlong, coords = c('x', 'y'), crs = 'EPSG:4326')

# Get raster
# Location specified from PLS data
# source = amazon web services
# z = 7 to match spatial resolution of gssurgo data
# Output is in EPSG:4326
in_elev <- elevatr::get_elev_raster(locations = latlong, src = 'aws', z = 7)

# calculate slope and aspect
in_slope <- raster::terrain(in_elev, opt = 'slope', unit = 'degrees')
in_aspect <- raster::terrain(in_elev, opt = 'aspect', unit = 'degrees')

# Make points from rasters
in_elev <- raster::rasterToPoints(in_elev)
in_slope <- raster::rasterToPoints(in_slope)
in_aspect <- raster::rasterToPoints(in_aspect)

# Reformat
in_elev <- as.data.frame(in_elev)
in_slope <- as.data.frame(in_slope)
in_aspect <- as.data.frame(in_aspect)

# Join elevation, slope and aspect
in_topo <- in_elev |>
  dplyr::left_join(y = in_slope, by = c('x', 'y')) |>
  dplyr::left_join(y = in_aspect, by = c('x', 'y'))

# Rename columns
colnames(in_topo) <- c('x', 'y', 'elevation', 'slope', 'aspect')

# Map of state
states <- sf::st_as_sf(maps::map('state', region = 'indiana',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Plots to make sure things look correct
in_topo |>
  dplyr::filter(x > -88.10372 & x < -84.79775 &
                  y > 37.78657 & y < 41.78008) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

in_topo |>
  dplyr::filter(x > -88.10372 & x < -84.79775 &
                  y > 37.78657 & y < 41.78008) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = slope)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

in_topo |>
  dplyr::filter(x > -88.10372 & x < -84.79775 &
                  y > 37.78657 & y < 41.78008) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aspect)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Save
save(in_topo, file = 'data/raw/topography/in_topo.RData')

#### MICHIGAN ####
rm(list = ls())

# Load PLS data with locations
load('data/processed/PLS/lowmichigan_process.RData')
load('data/processed/PLS/upmichigan_process.RData')

michigan <- rbind(lowmichigan_ecosystem, upmichigan_ecosystem)
latlong <- dplyr::select(michigan, x, y)
latlong <- sf::st_as_sf(latlong, coords = c('x', 'y'), crs = 'EPSG:4326')

# Get raster
# location specified from PLS data
# source = amazon web services
# z = 7 to match spatial resolution of gssurgo data
# output is in EPSG:4326
mi_elev <- elevatr::get_elev_raster(locations = latlong, src = 'aws', z = 7)

# calculate slope and aspect
mi_slope <- raster::terrain(mi_elev, opt = 'slope', unit = 'degrees')
mi_aspect <- raster::terrain(mi_elev, opt = 'aspect', unit = 'degrees')

# Make points from rasters
mi_elev <- raster::rasterToPoints(mi_elev)
mi_slope <- raster::rasterToPoints(mi_slope)
mi_aspect <- raster::rasterToPoints(mi_aspect)

# Reformat
mi_elev <- as.data.frame(mi_elev)
mi_slope <- as.data.frame(mi_slope)
mi_aspect <- as.data.frame(mi_aspect)

# Join elevation, slope and aspect
mi_topo <- mi_elev |>
  dplyr::left_join(y = mi_slope, by = c('x', 'y')) |>
  dplyr::left_join(y = mi_aspect, by = c('x', 'y'))

# Rename columns
colnames(mi_topo) <- c('x', 'y', 'elevation', 'slope', 'aspect')

# map of state
states <- sf::st_as_sf(maps::map('state', region = 'michigan',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = "EPSG:4326")

# Plots to make sure things look correct
mi_topo |>
  dplyr::filter(x > -90.41273 & x < -82.44289 &
                  y > 41.41133 & y < 47.48101) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

mi_topo |>
  dplyr::filter(x > -90.41273 & x < -82.44289 &
                  y > 41.41133 & y < 47.48101) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = slope)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

mi_topo |>
  dplyr::filter(x > -90.41273 & x < -82.44289 &
                  y > 41.41133 & y < 47.48101) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aspect)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Save
save(mi_topo, file = 'data/raw/topography/mi_topo.RData')

#### MINNESOTA ####
rm(list = ls())

# Load PLS data with locations
load('data/processed/PLS/minnesota_process.RData')

# Extract location from PLS data
latlong <- dplyr::select(minnesota_ecosystem, x, y)
latlong <- sf::st_as_sf(latlong, coords = c('x', 'y'), crs = 'EPSG:4326')

# Get raster
# location specified from PLS data
# source = amazon web services
# z = 7 to match spatial resolution of gssurgo data
# Output is in EPSG:4326
mn_elev <- elevatr::get_elev_raster(locations = latlong, src = 'aws', z = 7)

# calculate slope and aspect
mn_slope <- raster::terrain(mn_elev, opt = 'slope', unit = 'degrees')
mn_aspect <- raster::terrain(mn_elev, opt = 'aspect', unit = 'degrees')

# Make points from rasters
mn_elev <- raster::rasterToPoints(mn_elev)
mn_slope <- raster::rasterToPoints(mn_slope)
mn_aspect <- raster::rasterToPoints(mn_aspect)

# Reformat
mn_elev <- as.data.frame(mn_elev)
mn_slope <- as.data.frame(mn_slope)
mn_aspect <- as.data.frame(mn_aspect)

# Join elevation, slope and aspect
mn_topo <- mn_elev |>
  dplyr::left_join(y = mn_slope, by = c('x', 'y')) |>
  dplyr::left_join(y = mn_aspect, by = c('x', 'y'))

# Rename columns
colnames(mn_topo) <- c('x', 'y', 'elevation', 'slope', 'aspect')

# Map of state
states <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

mn_topo |>
  dplyr::filter(x > -97.22521 & x < -89.47309 &
                  y > 43.49323 & y < 49.38323) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

mn_topo |>
  dplyr::filter(x > -97.22521 & x < -89.47309 &
                  y > 43.49323 & y < 49.38323) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = slope)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

mn_topo |>
  dplyr::filter(x > -97.22521 & x < -89.47309 &
                  y > 43.49323 & y < 49.38323) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aspect)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Save
save(mn_topo, file = 'data/raw/topography/mn_topo.RData')

#### WISCONSIN ####
rm(list = ls())

# Load PLS data with locations
load('data/processed/PLS/wisconsin_process.RData')

# Extract location from PLS data
latlong <- dplyr::select(wisconsin_ecosystem, x, y)
latlong <- sf::st_as_sf(latlong, coords = c('x', 'y'), crs = 'EPSG:4326')

# Get raster
# location specified from PLS data
# source = amazon web services
# z = 7 to match spatial resolution of gssurgo data
# Output is in EPSG:4326
wi_elev <- elevatr::get_elev_raster(locations = latlong, src = 'aws', z = 7)

# calculate slope and aspect
wi_slope <- raster::terrain(wi_elev, opt = 'slope', unit = 'degrees')
wi_aspect <- raster::terrain(wi_elev, opt = 'aspect', unit = 'degrees')

# Make points from rasters
wi_elev <- raster::rasterToPoints(wi_elev)
wi_slope <- raster::rasterToPoints(wi_slope)
wi_aspect <- raster::rasterToPoints(wi_aspect)

# Reformat
wi_elev <- as.data.frame(wi_elev)
wi_slope <- as.data.frame(wi_slope)
wi_aspect <- as.data.frame(wi_aspect)

# Join elevation, slope and aspect
wi_topo <- wi_elev |>
  dplyr::left_join(y = wi_slope, by = c('x', 'y')) |>
  dplyr::left_join(y = wi_aspect, by = c('x', 'y'))

# Rename columns
colnames(wi_topo) <- c('x', 'y', 'elevation', 'slope', 'aspect')

# Map of state
states <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Plot to make sure things look correct
wi_topo |>
  dplyr::filter(x > -92.92229 & x < -86.9578 &
                  y > 42.49628 & y < 46.9367) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = elevation)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

wi_topo |>
  dplyr::filter(x > -92.92229 & x < -86.9578 &
                  y > 42.49628 & y < 46.9367) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = slope)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

wi_topo |>
  dplyr::filter(x > -92.92229 & x < -86.9578 &
                  y > 42.49628 & y < 46.9367) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y, color = aspect)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

# Save
save(wi_topo, file = 'data/raw/topography/wi_topo.RData')
