#### STEP 1-2

## Downloading data on the presence of floodplain from gSSURGO
## same as gSSURGO soil routine except you need to specfic
## method = 'Dominant Component (Category)' because it's a categorical variable

## 1. Define geographic extent
## 2. Fetch map unit keys
## 3. Extract RAT
## 4. Extract geomdesc variable
## 5. Format
## 6. Extract floodplain variable
## 7. Save

## Input: none. Downloaded directly in the script

## Output: data/raw/soils/gssurgo_floodplain_030_700m.RData
## Floodplain variable for 0-30 cm depth for all five states
## in the geographic domain
## Used in 1.3.gridded_soils.R

rm(list = ls())

#### 1. Define geographic extent ####

# Helper function for dividing the state into four quadrants
source('scripts/1.soils/define_bounds.R')

## Bounding boxes by states from the following link: 
## https://observablehq.com/@rdmurphy/u-s-state-bounding-boxes
## in WGS84 = EPSG:4326

## Illinois

# Split IL into four quadrants
IL <- define_quadrants(xmin = -91.514, xmax = -87.4947,
                       ymin = 36.9704, ymax = 42.5088)

# Extract min & max lat/long coords for each quadrant
IL1 <- IL[[1]]
IL2 <- IL[[2]]
IL3 <- IL[[3]]
IL4 <- IL[[4]]

# Define bounding boxes based on above coords
a_IL1 <- sf::st_bbox(
  c(xmin = IL1[1], xmax = IL1[2],
    ymin = IL1[3], ymax = IL1[4]),
  crs = sf::st_crs(4326)
)
a_IL2 <- sf::st_bbox(
  c(xmin = IL2[1], xmax = IL2[2],
    ymin = IL2[3], ymax = IL2[4]),
  crs = sf::st_crs(4326)
)
a_IL3 <- sf::st_bbox(
  c(xmin = IL3[1], xmax = IL3[2],
    ymin = IL3[3], ymax = IL3[4]),
  crs = sf::st_crs(4326)
)
a_IL4 <- sf::st_bbox(
  c(xmin = IL4[1], xmax = IL4[2],
    ymin = IL4[3], ymax = IL4[4]),
  crs = sf::st_crs(4326)
)

## Indiana

IN <- define_quadrants(xmin = -88.0977, xmax = -84.7848,
                       ymin = 37.7719, ymax = 41.7605)

IN1 <- IN[[1]]
IN2 <- IN[[2]]
IN3 <- IN[[3]]
IN4 <- IN[[4]]

a_IN1 <- sf::st_bbox(
  c(xmin = IN1[1], xmax = IN1[2],
    ymin = IN1[3], ymax = IN1[4]),
  crs = sf::st_crs(4326)
)
a_IN2 <- sf::st_bbox(
  c(xmin = IN2[1], xmax = IN2[2],
    ymin = IN2[3], ymax = IN2[4]),
  crs = sf::st_crs(4326)
)
a_IN3 <- sf::st_bbox(
  c(xmin = IN3[1], xmax = IN3[2],
    ymin = IN3[3], ymax = IN3[4]),
  crs = sf::st_crs(4326)
)
a_IN4 <- sf::st_bbox(
  c(xmin = IN4[1], xmax = IN4[2],
    ymin = IN4[3], ymax = IN4[4]),
  crs = sf::st_crs(4326)
)

## Michigan

MI <- define_quadrants(xmin = -90.4164, xmax = -82.4159,
                       ymin = 41.6961, ymax = 48.1906)

MI1 <- MI[[1]]
MI2 <- MI[[2]]
MI3 <- MI[[3]]
MI4 <- MI[[4]]

a_MI1 <- sf::st_bbox(
  c(xmin = MI1[1], xmax = MI1[2],
    ymin = MI1[3], ymax = MI1[4]),
  crs = sf::st_crs(4326)
)
a_MI2 <- sf::st_bbox(
  c(xmin = MI2[1], xmax = MI2[2],
    ymin = MI2[3], ymax = MI2[4]),
  crs = sf::st_crs(4326)
)
a_MI3 <- sf::st_bbox(
  c(xmin = MI3[1], xmax = MI3[2],
    ymin = MI3[3], ymax = MI3[4]),
  crs = sf::st_crs(4326)
)
a_MI4 <- sf::st_bbox(
  c(xmin = MI4[1], xmax = MI4[2],
    ymin = MI4[3], ymax = MI4[4]),
  crs = sf::st_crs(4326)
)

## Minnesota
MN <- define_quadrants(xmin = -97.2387, xmax = -89.4904,
                       ymin = 43.4993, ymax = 49.3847)

MN1 <- MN[[1]]
MN2 <- MN[[2]]
MN3 <- MN[[3]]
MN4 <- MN[[4]]

a_MN1 <- sf::st_bbox(
  c(xmin = MN1[1], xmax = MN1[2],
    ymin = MN1[3], ymax = MN1[4]),
  crs = sf::st_crs(4326)
)
a_MN2 <- sf::st_bbox(
  c(xmin = MN2[1], xmax = MN2[2],
    ymin = MN2[3], ymax = MN2[4]),
  crs = sf::st_crs(4326)
)
a_MN3 <- sf::st_bbox(
  c(xmin = MN3[1], xmax = MN3[2],
    ymin = MN3[3], ymax = MN3[4]),
  crs = sf::st_crs(4326)
)
a_MN4 <- sf::st_bbox(
  c(xmin = MN4[1], xmax = MN4[2],
    ymin = MN4[3], ymax = MN4[4]),
  crs = sf::st_crs(4326)
)

## Wisconsin
WI <- define_quadrants(xmin = -92.8894, xmax = -86.8235,
                       ymin = 42.4916, ymax = 47.0772)

WI1 <- WI[[1]]
WI2 <- WI[[2]]
WI3 <- WI[[3]]
WI4 <- WI[[4]]

a_WI1 <- sf::st_bbox(
  c(xmin = WI1[1], xmax = WI1[2],
    ymin = WI1[3], ymax = WI1[4]),
  crs = sf::st_crs(4326)
)
a_WI2 <- sf::st_bbox(
  c(xmin = WI2[1], xmax = WI2[2],
    ymin = WI2[3], ymax = WI2[4]),
  crs = sf::st_crs(4326)
)
a_WI3 <- sf::st_bbox(
  c(xmin = WI3[1], xmax = WI3[2],
    ymin = WI3[3], ymax = WI3[4]),
  crs = sf::st_crs(4326)
)
a_WI4 <- sf::st_bbox(
  c(xmin = WI4[1], xmax = WI4[2],
    ymin = WI4[3], ymax = WI4[4]),
  crs = sf::st_crs(4326)
)

#### 2. Fetch map unit keys ####

# fetch gSSURGO map unit keys at 100m resolution 
# (highest possible = 30m, lowest possible = 3000m)
# highest resolution possible for all states
# (lowest resolution limited by extent of Michigan)
# will need to be rescaled for use in analysis
mu_IL1 <- soilDB::mukey.wcs(list(aoi = a_IL1, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_IL2 <- soilDB::mukey.wcs(list(aoi = a_IL2, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_IL3 <- soilDB::mukey.wcs(list(aoi = a_IL3, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_IL4 <- soilDB::mukey.wcs(list(aoi = a_IL4, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)

mu_IN1 <- soilDB::mukey.wcs(list(aoi = a_IN1, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_IN2 <- soilDB::mukey.wcs(list(aoi = a_IN2, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_IN3 <- soilDB::mukey.wcs(list(aoi = a_IN3, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_IN4 <- soilDB::mukey.wcs(list(aoi = a_IN4, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)

mu_MI1 <- soilDB::mukey.wcs(list(aoi = a_MI1, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_MI2 <- soilDB::mukey.wcs(list(aoi = a_MI2, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_MI3 <- soilDB::mukey.wcs(list(aoi = a_MI3, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_MI4 <- soilDB::mukey.wcs(list(aoi = a_MI4, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)

mu_MN1 <- soilDB::mukey.wcs(list(aoi = a_MN1, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_MN2 <- soilDB::mukey.wcs(list(aoi = a_MN2, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_MN3 <- soilDB::mukey.wcs(list(aoi = a_MN3, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_MN4 <- soilDB::mukey.wcs(list(aoi = a_MN4, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)

mu_WI1 <- soilDB::mukey.wcs(list(aoi = a_WI1, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_WI2 <- soilDB::mukey.wcs(list(aoi = a_WI2, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_WI3 <- soilDB::mukey.wcs(list(aoi = a_WI3, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)
mu_WI4 <- soilDB::mukey.wcs(list(aoi = a_WI4, crs = 'EPSG:4326'), db = 'gssurgo', res = 700)

#### 3. Extract RAT ####

# extract RAT for thematic mapping for each state
rat_IL1 <- terra::cats(mu_IL1)[[1]]
rat_IL2 <- terra::cats(mu_IL2)[[1]]
rat_IL3 <- terra::cats(mu_IL3)[[1]]
rat_IL4 <- terra::cats(mu_IL4)[[1]]

rat_IN1 <- terra::cats(mu_IN1)[[1]]
rat_IN2 <- terra::cats(mu_IN2)[[1]]
rat_IN3 <- terra::cats(mu_IN3)[[1]]
rat_IN4 <- terra::cats(mu_IN4)[[1]]

rat_MI1 <- terra::cats(mu_MI1)[[1]]
rat_MI2 <- terra::cats(mu_MI2)[[1]]
rat_MI3 <- terra::cats(mu_MI3)[[1]]
rat_MI4 <- terra::cats(mu_MI4)[[1]]

rat_MN1 <- terra::cats(mu_MN1)[[1]]
rat_MN2 <- terra::cats(mu_MN2)[[1]]
rat_MN3 <- terra::cats(mu_MN3)[[1]]
rat_MN4 <- terra::cats(mu_MN4)[[1]]

rat_WI1 <- terra::cats(mu_WI1)[[1]]
rat_WI2 <- terra::cats(mu_WI2)[[1]]
rat_WI3 <- terra::cats(mu_WI3)[[1]]
rat_WI4 <- terra::cats(mu_WI4)[[1]]

#### 4. Extract geomdesc variable ####

# property = geomdesc (I learned this would work by following the procedure
# described here: https://www.nrcs.usda.gov/sites/default/files/2022-08/gSSURGO_UserGuide_July2020.pdf
# in GIS and realizing that the component table has the variable geomdesc
# which is matched to more specific features in cogeomordesc table
# so we can just skip the cogeomordesc table and extract presence of floodplain
# from geomdesc, which is a property, while cogeomordesc is not)
tab_IL1 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_IL1$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_IL2 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_IL2$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_IL3 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_IL3$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_IL4 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_IL4$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')

tab_IN1 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_IN1$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_IN2 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_IN2$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_IN3 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_IN3$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_IN4 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_IN4$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')

tab_MI1 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_MI1$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_MI2 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_MI2$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_MI3 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_MI3$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_MI4 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_MI4$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')

tab_MN1 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_MN1$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_MN2 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_MN2$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_MN3 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_MN3$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_MN4 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_MN4$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')

tab_WI1 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_WI1$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_WI2 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_WI2$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_WI3 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_WI3$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')
tab_WI4 <- soilDB::get_SDA_property(property = 'geomdesc',
                                    mukeys = as.integer(rat_WI4$mukey),
                                    top_depth = 0,
                                    bottom_depth = 30,
                                    method = 'Dominant Component (Category)')

# Set raster categories
levels(mu_IL1) <- tab_IL1[, c('mukey', 'geomdesc')]
levels(mu_IL2) <- tab_IL2[, c('mukey', 'geomdesc')]
levels(mu_IL3) <- tab_IL3[, c('mukey', 'geomdesc')]
levels(mu_IL4) <- tab_IL4[, c('mukey', 'geomdesc')]

levels(mu_IN1) <- tab_IN1[, c('mukey', 'geomdesc')]
levels(mu_IN2) <- tab_IN2[, c('mukey', 'geomdesc')]
levels(mu_IN3) <- tab_IN3[, c('mukey', 'geomdesc')]
levels(mu_IN4) <- tab_IN4[, c('mukey', 'geomdesc')]

levels(mu_MI1) <- tab_MI1[, c('mukey', 'geomdesc')]
levels(mu_MI2) <- tab_MI2[, c('mukey', 'geomdesc')]
levels(mu_MI3) <- tab_MI3[, c('mukey', 'geomdesc')]
levels(mu_MI4) <- tab_MI4[, c('mukey', 'geomdesc')]

levels(mu_MN1) <- tab_MN1[, c('mukey', 'geomdesc')]
levels(mu_MN2) <- tab_MN2[, c('mukey', 'geomdesc')]
levels(mu_MN3) <- tab_MN3[, c('mukey', 'geomdesc')]
levels(mu_MN4) <- tab_MN4[, c('mukey', 'geomdesc')]

levels(mu_WI1) <- tab_WI1[, c('mukey', 'geomdesc')]
levels(mu_WI2) <- tab_WI2[, c('mukey', 'geomdesc')]
levels(mu_WI3) <- tab_WI3[, c('mukey', 'geomdesc')]
levels(mu_WI4) <- tab_WI4[, c('mukey', 'geomdesc')]

#### 5. Format ####

# reproject
ssc_IL1 <- terra::project(x = mu_IL1, 'EPSG:4326')
ssc_IL2 <- terra::project(x = mu_IL2, 'EPSG:4326')
ssc_IL3 <- terra::project(x = mu_IL3, 'EPSG:4326')
ssc_IL4 <- terra::project(x = mu_IL4, 'EPSG:4326')

ssc_IN1 <- terra::project(x = mu_IN1, 'EPSG:4326')
ssc_IN2 <- terra::project(x = mu_IN2, 'EPSG:4326')
ssc_IN3 <- terra::project(x = mu_IN3, 'EPSG:4326')
ssc_IN4 <- terra::project(x = mu_IN4, 'EPSG:4326')

ssc_MI1 <- terra::project(x = mu_MI1, 'EPSG:4326')
ssc_MI2 <- terra::project(x = mu_MI2, 'EPSG:4326')
ssc_MI3 <- terra::project(x = mu_MI3, 'EPSG:4326')
ssc_MI4 <- terra::project(x = mu_MI4, 'EPSG:4326')

ssc_MN1 <- terra::project(x = mu_MN1, 'EPSG:4326')
ssc_MN2 <- terra::project(x = mu_MN2, 'EPSG:4326')
ssc_MN3 <- terra::project(x = mu_MN3, 'EPSG:4326')
ssc_MN4 <- terra::project(x = mu_MN4, 'EPSG:4326')

ssc_WI1 <- terra::project(x = mu_WI1, 'EPSG:4326')
ssc_WI2 <- terra::project(x = mu_WI2, 'EPSG:4326')
ssc_WI3 <- terra::project(x = mu_WI3, 'EPSG:4326')
ssc_WI4 <- terra::project(x = mu_WI4, 'EPSG:4326')

# Convert to data frame
df_IL1 <- raster::as.data.frame(x = ssc_IL1, xy = TRUE)
df_IL2 <- raster::as.data.frame(x = ssc_IL2, xy = TRUE)
df_IL3 <- raster::as.data.frame(x = ssc_IL3, xy = TRUE)
df_IL4 <- raster::as.data.frame(x = ssc_IL4, xy = TRUE)

df_IN1 <- raster::as.data.frame(x = ssc_IN1, xy = TRUE)
df_IN2 <- raster::as.data.frame(x = ssc_IN2, xy = TRUE)
df_IN3 <- raster::as.data.frame(x = ssc_IN3, xy = TRUE)
df_IN4 <- raster::as.data.frame(x = ssc_IN4, xy = TRUE)

df_MI1 <- raster::as.data.frame(x = ssc_MI1, xy = TRUE)
df_MI2 <- raster::as.data.frame(x = ssc_MI2, xy = TRUE)
df_MI3 <- raster::as.data.frame(x = ssc_MI3, xy = TRUE)
df_MI4 <- raster::as.data.frame(x = ssc_MI4, xy = TRUE)

df_MN1 <- raster::as.data.frame(x = ssc_MN1, xy = TRUE)
df_MN2 <- raster::as.data.frame(x = ssc_MN2, xy = TRUE)
df_MN3 <- raster::as.data.frame(x = ssc_MN3, xy = TRUE)
df_MN4 <- raster::as.data.frame(x = ssc_MN4, xy = TRUE)

df_WI1 <- raster::as.data.frame(x = ssc_WI1, xy = TRUE)
df_WI2 <- raster::as.data.frame(x = ssc_WI2, xy = TRUE)
df_WI3 <- raster::as.data.frame(x = ssc_WI3, xy = TRUE)
df_WI4 <- raster::as.data.frame(x = ssc_WI4, xy = TRUE)

# Combine
df_IL <- rbind(df_IL1, df_IL2, df_IL3, df_IL4)
df_IN <- rbind(df_IN1, df_IN2, df_IN3, df_IN4)
df_MI <- rbind(df_MI1, df_MI2, df_MI3, df_MI4)
df_MN <- rbind(df_MN1, df_MN2, df_MN3, df_MN4)
df_WI <- rbind(df_WI1, df_WI2, df_WI3, df_WI4)

#### 6. Extract floodplain variable ####

# Extract mention of "floodplain" from geomdesc
df_IL <- df_IL |>
  dplyr::mutate(Floodplain = dplyr::if_else(grepl(pattern = 'flood', 
                                                 x = geomdesc,
                                                 ignore.case = TRUE), 'Yes', 'No'))

df_IN <- df_IN |>
  dplyr::mutate(Floodplain = dplyr::if_else(grepl(pattern = 'flood',
                                                  x = geomdesc,
                                                  ignore.case = TRUE), 'Yes', 'No'))

df_MI <- df_MI |>
  dplyr::mutate(Floodplain = dplyr::if_else(grepl(pattern = 'flood',
                                                  x = geomdesc,
                                                  ignore.case = TRUE), 'Yes', 'No'))

df_MN <- df_MN |>
  dplyr::mutate(Floodplain = dplyr::if_else(grepl(pattern = 'flood',
                                                  x = geomdesc,
                                                  ignore.case = TRUE), 'Yes', 'No'))

df_WI <- df_WI |>
  dplyr::mutate(Floodplain = dplyr::if_else(grepl(pattern = 'flood',
                                                  x = geomdesc,
                                                  ignore.case = TRUE), 'Yes', 'No'))

# Plot to make sure presence of floodplain follows expected pattern
states <- sf::st_as_sf(maps::map('state', region = 'illinois',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

df_IL |>
  dplyr::filter(Floodplain == 'Yes') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

states <- sf::st_as_sf(maps::map('state', region = 'indiana',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

df_IN |>
  dplyr::filter(Floodplain == 'Yes') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

states <- sf::st_as_sf(maps::map('state', region = 'michigan',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

df_MI |>
  dplyr::filter(Floodplain == 'Yes') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

states <- sf::st_as_sf(maps::map('state', region = 'minnesota',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

df_MN |>
  dplyr::filter(Floodplain == 'Yes') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

states <- sf::st_as_sf(maps::map('state', region = 'wisconsin',
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

df_WI |>
  dplyr::filter(Floodplain == 'Yes') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y), shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void()

#### 7. Save ####

# Everything looks good, save
save(df_IL, df_IN, df_MI, df_MN, df_WI,
     file = 'data/raw/soils/gssurgo_floodplain_030_700m.RData')
