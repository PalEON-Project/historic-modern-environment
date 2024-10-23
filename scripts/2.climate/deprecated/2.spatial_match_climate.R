## Matching climate and PLS spatially

rm(list = ls())

#### ILLINOIS ####
load('data/raw/PRISM/climate_summary.RData')

load('data/processed/PLS/illinois_format.RData')

# Rename climate data
df_IL <- clim_sum

# Format illinois data
illinois <- illinois |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide PLS illinois into 4 sections
illinois1 <- illinois[1:19808,]
illinois2 <- illinois[19809:39617,]
illinois3 <- illinois[39618:59426,]
illinois4 <- illinois[59427:79232,]

# Select coordinates
coords_illinois1 <- dplyr::select(illinois1, y, x)
coords_illinois2 <- dplyr::select(illinois2, y, x)
coords_illinois3 <- dplyr::select(illinois3, y, x)
coords_illinois4 <- dplyr::select(illinois4, y, x)

# Add column names
colnames(coords_illinois1) <- colnames(coords_illinois2) <-
  colnames(coords_illinois3) <- colnames(coords_illinois4) <-
  c('lat', 'long')

# Then extract only the lat/long columns from climate data
coords_df_IL <- df_IL |>
  dplyr::select(y, x)

# Find the distance between each pair of points in the PLS (1) and climate (2) dataframes
dists <- fields::rdist(coords_df_IL, coords_illinois1)
# Find the closest climate point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately remove dists because it's huge
rm(dists)

# Make new dataframe of corresponding climate data to the PLS points
# We'll add to this subsequently
select_df_IL <- df_IL[closest_points,]

# Column bind with PLS data
IL_clim_pls <- cbind(select_df_IL, illinois1)

# Repeat all these steps for the remaining 3 subsets

# Subset 2
dists <- fields::rdist(coords_df_IL, coords_illinois2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL[closest_points,]

temp <- cbind(select_df_IL, illinois2)
IL_clim_pls <- rbind(IL_clim_pls, temp)

# Subset 3
dists <- fields::rdist(coords_df_IL, coords_illinois3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL[closest_points,]

temp <- cbind(select_df_IL, illinois3)
IL_clim_pls <- rbind(IL_clim_pls, temp)

# Subset 4
dists <- fields::rdist(coords_df_IL, coords_illinois4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IL <- df_IL[closest_points,]

temp <- cbind(select_df_IL, illinois4)
IL_clim_pls <- rbind(IL_clim_pls, temp)

# Update column names
colnames(IL_clim_pls) <- c('prism_x', 'prism_y', 'ppt_mean',
                           'ppt_sd', 'tmean_mean', 'tmean_sd',
                           'tmin', 'tmax', 'vpdmin', 'vpdmax',
                           'state', 'pls_x', 'pls_y', 'L1_tree1',
                           'L1_tree2', 'L1_tree3', 'L1_tree4', 'uniqueID')

# Save IL climate. Confirmed in same order as illinois and illinois_ecosystem
IL_clim <- dplyr::select(IL_clim_pls, prism_x, prism_y, ppt_mean,
                         ppt_sd, tmean_mean, tmean_sd, tmin, tmax,
                         vpdmin, vpdmax, pls_x, pls_y)
save(IL_clim, file = 'data/processed/climate/processed_climate_il.RData')

#### INDIANA ####
rm(list = ls())

load('data/raw/PRISM/climate_summary.RData')

load('data/processed/PLS/indiana_format.RData')

# Rename climate data
df_IN <- clim_sum

# Format indiana data
indiana <- indiana |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide PLS indiana into 4 sections
indiana1 <- indiana[1:19794,]
indiana2 <- indiana[19795:39589,]
indiana3 <- indiana[39590:59383,]
indiana4 <- indiana[59384:79177,]

# Select coordinates
coords_indiana1 <- dplyr::select(indiana1, y, x)
coords_indiana2 <- dplyr::select(indiana2, y, x)
coords_indiana3 <- dplyr::select(indiana3, y, x)
coords_indiana4 <- dplyr::select(indiana4, y, x)

# Add column names
colnames(coords_indiana1) <- colnames(coords_indiana2) <-
  colnames(coords_indiana3) <- colnames(coords_indiana4) <-
  c('lat', 'long')

# Then extract only the lat/long columns from climate data
coords_df_IN <- dplyr::select(df_IN, y, x)

# Find the distance between each pair of points in the PLS (1) and climate (2) dataframes
dists <- fields::rdist(coords_df_IN, coords_indiana1)
# Find the closest climate point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately remove dists because it's huge
rm(dists)

# Make new dataframe of corresponding climate data to the PLS points
# We'll add to this subsequently
select_df_IN <- df_IN[closest_points,]

# Column bind with PLS data
IN_clim_pls <- cbind(select_df_IN, indiana1)

# Repeat all these steps for the remaining 3 subsets

# Subset 2
dists <- fields::rdist(coords_df_IN, coords_indiana2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN[closest_points,]

temp <- cbind(select_df_IN, indiana2)
IN_clim_pls <- rbind(IN_clim_pls, temp)

# Subset 3
dists <- fields::rdist(coords_df_IN, coords_indiana3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN[closest_points,]

temp <- cbind(select_df_IN, indiana3)
IN_clim_pls <- rbind(IN_clim_pls, temp)

# Subset 4
dists <- fields::rdist(coords_df_IN, coords_indiana4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_IN <- df_IN[closest_points,]

temp <- cbind(select_df_IN, indiana4)
IN_clim_pls <- rbind(IN_clim_pls, temp)

# Update column names
colnames(IN_clim_pls) <- c('prism_x', 'prism_y', 'ppt_mean',
                           'ppt_sd', 'tmean_mean', 'tmean_sd',
                           'tmin', 'tmax', 'vpdmin', 'vpdmax',
                           'state', 'pls_x', 'pls_y', 'L1_tree1',
                           'L1_tree2', 'uniqueID')

# Save IN climate. Confirmed in same order as indiana and indiana_ecosystem
IN_clim <- dplyr::select(IN_clim_pls, prism_x, prism_y, ppt_mean,
                         ppt_sd, tmean_mean, tmean_sd, tmin, tmax,
                         vpdmin, vpdmax, pls_x, pls_y)
save(IN_clim, file = 'data/processed/climate/processed_climate_in.RData')

#### MICHIGAN ####
rm(list = ls())

load('data/raw/PRISM/climate_summary.RData')

load('data/processed/PLS/lowmichigan_process.RData')
load('data/processed/PLS/upmichigan_process.RData')

# Rename climate data
df_MI <- clim_sum

# Format lower michigan data
lowmichigan <- lowmichigan |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species',
                     values_fn = list) |>
  tidyr::unnest(cols = everything()) |>
  dplyr::rename(SPP1 = species1,
                SPP2 = species2)

# Format upper michigan data
upmichigan <- upmichigan |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species',
                     values_fn = list) |>
  tidyr::unnest(cols = everything())

michigan <- rbind(lowmichigan, upmichigan)

michigan_ecosystem <- rbind(lowmichigan_ecosystem,
                            upmichigan_ecosystem)

# Divide PLS michigan into 4 sections
michigan1 <- michigan[1:30138,]
michigan2 <- michigan[30139:60277,]
michigan3 <- michigan[60278:90416,]
michigan4 <- michigan[90417:120554,]

# Select coordinates
coords_michigan1 <- dplyr::select(michigan1, y, x)
coords_michigan2 <- dplyr::select(michigan2, y, x)
coords_michigan3 <- dplyr::select(michigan3, y, x)
coords_michigan4 <- dplyr::select(michigan4, y, x)

# Add column names
colnames(coords_michigan1) <- colnames(coords_michigan2) <-
  colnames(coords_michigan3) <- colnames(coords_michigan4) <-
  c('lat', 'long')

# Then extract only the lat/long columns from climate data
coords_df_MI <- dplyr::select(df_MI, y, x)

# Find the distance between each pair of points in the PLS (1) and the climate dataframes
dists <- fields::rdist(coords_df_MI, coords_michigan1)
# Find the closest climate point to each PLS point
# (should have length = nrow(pls subsest))
closest_points <- apply(dists, 2, which.min)
# Immediately remove dists because it's huge
rm(dists)

# Make new dataframe of corresponding climate data to the PLS points
# We'll add to this subsequently
select_df_MI <- df_MI[closest_points,]

# Column bind with PLS data
MI_clim_pls <- cbind(select_df_MI, michigan1)

# Repeat all these steps for the remaining 3 subsets

# Subset 3
dists <- fields::rdist(coords_df_MI, coords_michigan2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI[closest_points,]

temp <- cbind(select_df_MI, michigan2)
MI_clim_pls <- rbind(MI_clim_pls, temp)

# Subset 3
dists <- fields::rdist(coords_df_MI, coords_michigan3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI[closest_points,]

temp <- cbind(select_df_MI, michigan3)
MI_clim_pls <- rbind(MI_clim_pls, temp)

# Subset 4
dists <- fields::rdist(coords_df_MI, coords_michigan4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MI <- df_MI[closest_points,]

temp <- cbind(select_df_MI, michigan4)
MI_clim_pls <- rbind(MI_clim_pls, temp)

# Update column names
colnames(MI_clim_pls) <- c('prism_x', 'prism_y', 'ppt_mean',
                           'ppt_sd', 'tmean_mean', 'tmean_sd',
                           'tmin', 'tmax', 'vpdmin', 'vpdmax',
                           'state', 'pls_x', 'pls_y', 'L1_tree1',
                           'L1_tree2')

# Save MI climate. Confirmed in same order as michigan and michigan_ecosystem
MI_clim <- dplyr::select(MI_clim_pls, prism_x, prism_y, ppt_mean,
                         ppt_sd, tmean_mean, tmean_sd, tmin, tmax,
                         vpdmin, vpdmax, pls_x, pls_y)
save(MI_clim, file = 'data/processed/climate/processed_climate_mi.RData')

#### MINNESEOTA ####
rm(list = ls())

load('data/raw/PRISM/climate_summary.RData')

load('data/processed/PLS/minnesota_process.RData')

# Rename climate data
df_MN <- clim_sum

# Format minnesota data
minnesota <- minnesota |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species',
                     values_fn = list) |>
  tidyr::unnest(cols = everything()) |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide PLS minnesota into 5 sections
minnesota1 <- minnesota[1:28786,]
minnesota2 <- minnesota[28787:57573,]
minnesota3 <- minnesota[57574:86360,]
minnesota4 <- minnesota[86361:115147,]
minnesota5 <- minnesota[115148:143932,]

# Select coordinates
coords_minnesota1 <- dplyr::select(minnesota1, y, x)
coords_minnesota2 <- dplyr::select(minnesota2, y, x)
coords_minnesota3 <- dplyr::select(minnesota3, y, x)
coords_minnesota4 <- dplyr::select(minnesota4, y, x)
coords_minnesota5 <- dplyr::select(minnesota5, y, x)

# Add column names
colnames(coords_minnesota1) <- colnames(coords_minnesota2) <-
  colnames(coords_minnesota3) <- colnames(coords_minnesota4) <-
  colnames(coords_minnesota5) <- c('lat', 'long')

# Then extract only the lat/long columns from climate data
coords_df_MN <- dplyr::select(df_MN, y, x)

# Find the distance between each pair of points in the PLS (1) and climate (2) dataframes
dists <- fields::rdist(coords_df_MN, coords_minnesota1)
# Find the closest climate point to each PLS point
# (should have length = nrow(pls, subset))
closest_points <- apply(dists, 2, which.min)
# Immediately remove dists because it's huge
rm(dists)

# Make new dataframe of corresponding climate data to the PLS points
# We'll add to this subsequently
select_df_MN <- df_MN[closest_points,]

# Column bind with PLS data
MN_clim_pls <- cbind(select_df_MN, minnesota1)

# Repeat all these steps for the remaining 3 subsets

# Subset 2
dists <- fields::rdist(coords_df_MN, coords_minnesota2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN[closest_points,]

temp <- cbind(select_df_MN, minnesota2)
MN_clim_pls <- rbind(MN_clim_pls, temp)

# Subset 3
dists <- fields::rdist(coords_df_MN, coords_minnesota3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN[closest_points,]

temp <- cbind(select_df_MN, minnesota3)
MN_clim_pls <- rbind(MN_clim_pls, temp)

# Subset 4
dists <- fields::rdist(coords_df_MN, coords_minnesota4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN[closest_points,]

temp <- cbind(select_df_MN, minnesota4)
MN_clim_pls <- rbind(MN_clim_pls, temp)

# Subset 5
dists <- fields::rdist(coords_df_MN, coords_minnesota5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_MN <- df_MN[closest_points,]

temp <- cbind(select_df_MN, minnesota5)
MN_clim_pls <- rbind(MN_clim_pls, temp)

# Update column names
colnames(MN_clim_pls) <- c('prism_x', 'prism_y', 'ppt_mean',
                           'ppt_sd', 'tmean_mean', 'tmean_sd',
                           'tmin', 'tmax', 'vpdmin', 'vpdmax',
                           'state', 'pls_x', 'pls_y', 'L1_tree1',
                           'L1_tree2', 'L1_tree3', 'L1_tree4', 'uniqueID')

# Save MN climate. Confirmed in same order as minnesota and minnesota_ecosystem
MN_clim <- dplyr::select(MN_clim_pls, prism_x, prism_y, ppt_mean,
                         ppt_sd, tmean_mean, tmean_sd, tmin, tmax,
                         vpdmin, vpdmax, pls_x, pls_y)
save(MN_clim, file = 'data/processed/climate/processed_climate_mn.RData')

#### WISCONSIN ####
rm(list = ls())

load('data/raw/PRISM/climate_summary.RData')

load('data/processed/PLS/wisconsin_process.RData')

# Rename climate data
df_WI <- clim_sum

# Format wisconsin data
wisconsin <- wisconsin |>
  tidyr::pivot_wider(names_from = 'tree', values_from = 'species') |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Divide PLS wisconsin into 6 sections
wisconsin1 <- wisconsin[1:27772,]
wisconsin2 <- wisconsin[27773:55545,]
wisconsin3 <- wisconsin[55546:83318,]
wisconsin4 <- wisconsin[83319:111091,]
wisconsin5 <- wisconsin[111092:138864,]
wisconsin6 <- wisconsin[138865:166636,]

# Select coordinates
coords_wisconsin1 <- dplyr::select(wisconsin1, y, x)
coords_wisconsin2 <- dplyr::select(wisconsin2, y, x)
coords_wisconsin3 <- dplyr::select(wisconsin3, y, x)
coords_wisconsin4 <- dplyr::select(wisconsin4, y, x)
coords_wisconsin5 <- dplyr::select(wisconsin5, y, x)
coords_wisconsin6 <- dplyr::select(wisconsin6, y, x)

# Add column names
colnames(coords_wisconsin1) <- colnames(coords_wisconsin2) <-
  colnames(coords_wisconsin3) <- colnames(coords_wisconsin4) <-
  colnames(coords_wisconsin5) <- colnames(coords_wisconsin6) <-
  c('lat', 'long')

# Then extract only the lat/long columns from climate data
coords_df_WI <- dplyr::select(df_WI, y, x)

# Find the  distance between each pair of points in the PLS (1) and climate (2) dataframes
dists <- fields::rdist(coords_df_WI, coords_wisconsin1)
# Find the closest climate point to each PLS point
# (should have length = nrow(pls subset))
closest_points <- apply(dists, 2, which.min)
# Immediately remove dists because it's huge
rm(dists)

# Make new dataframe of corresponding climate data to the PLS points
# We'll add to this subsequently
select_df_WI <- df_WI[closest_points,]

# Column bind with PLS data
WI_clim_pls <- cbind(select_df_WI, wisconsin1)

# Repeat all these steps for the remaining 4 subsets

# Subset 2
dists <- fields::rdist(coords_df_WI, coords_wisconsin2)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI[closest_points,]

temp <- cbind(select_df_WI, wisconsin2)
WI_clim_pls <- rbind(WI_clim_pls, temp)

# Subset 3
dists <- fields::rdist(coords_df_WI, coords_wisconsin3)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI[closest_points,]

temp <- cbind(select_df_WI, wisconsin3)
WI_clim_pls <- rbind(WI_clim_pls, temp)

# Subset 4
dists <- fields::rdist(coords_df_WI, coords_wisconsin4)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI[closest_points,]

temp <- cbind(select_df_WI, wisconsin4)
WI_clim_pls <- rbind(WI_clim_pls, temp)

# Subset 5
dists <- fields::rdist(coords_df_WI, coords_wisconsin5)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI[closest_points,]

temp <- cbind(select_df_WI, wisconsin5)
WI_clim_pls <- rbind(WI_clim_pls, temp)

# Subset 6
dists <- fields::rdist(coords_df_WI, coords_wisconsin6)
closest_points <- apply(dists, 2, which.min)
rm(dists)

select_df_WI <- df_WI[closest_points,]

temp <- cbind(select_df_WI, wisconsin6)
WI_clim_pls <- rbind(WI_clim_pls, temp)

# Update column names
colnames(WI_clim_pls) <- c('prism_x', 'prism_y', 'ppt_mean',
                           'ppt_sd', 'tmean_mean', 'tmean_sd',
                           'tmin', 'tmax', 'vpdmin', 'vpdmax',
                           'state', 'pls_x', 'pls_y', 'L1_tree1',
                           'L1_tree2', 'L1_tree3', 'L1_tree4', 'uniqueID')

# Save WI climate. Confirmed in same order as wisconsin and wisconsin_ecosystem
WI_clim <- dplyr::select(WI_clim_pls, prism_x, prism_y, ppt_mean,
                         ppt_sd, tmean_mean, tmean_sd, tmin, tmax,
                         vpdmin, vpdmax, pls_x, pls_y)
save(WI_clim, file = 'data/processed/climate/processed_climate_wi.RData')

#### CHECKS ####

# Check to make sure we're not too far off of the 1:1 line with any of our coordinate matching

rm(list = ls())

load('data/processed/climate/processed_climate_il.RData')

IL_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = prism_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()
IL_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = prism_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

IL_clim |>
  dplyr::mutate(deviance_x = abs(pls_x - prism_x),
                deviance_y = abs(pls_y - prism_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/climate/processed_climate_in.RData')

IN_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = prism_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()
IN_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = prism_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

IN_clim |>
  dplyr::mutate(deviance_x = abs(pls_x - prism_x),
                deviance_y = abs(pls_y - prism_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/climate/processed_climate_mi.RData')

MI_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = prism_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()
MI_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = prism_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MI_clim |>
  dplyr::mutate(deviance_x = abs(pls_x - prism_x),
                deviance_y = abs(pls_y - prism_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/climate/processed_climate_mn.RData')

MN_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = prism_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()
MN_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = prism_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

MN_clim |>
  dplyr::mutate(deviance_x = abs(pls_x - prism_x),
                deviance_y = abs(pls_y - prism_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

load('data/processed/climate/processed_climate_wi.RData')

WI_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_x, y = prism_x)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()
WI_clim |>
  ggplot2::ggplot(ggplot2::aes(x = pls_y, y = prism_y)) +
  ggplot2::geom_point(shape = 21) +
  ggplot2::geom_abline()

WI_clim |>
  dplyr::mutate(deviance_x = abs(pls_x - prism_x),
                deviance_y = abs(pls_y - prism_y)) |>
  dplyr::summarize(mean_deviance_x = mean(deviance_x),
                   mean_deviance_y = mean(deviance_y),
                   max_deviance_x = max(deviance_x),
                   max_deviance_y = max(deviance_y),
                   med_deviance_x = median(deviance_x),
                   med_deviance_y = median(deviance_y))

# Plot distribution of climate variables
states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana',
                                                     'michigan', 'minnesota',
                                                     'wisconsin'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

clim <- rbind(IL_clim, IN_clim, MI_clim, MN_clim, WI_clim)

clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = ppt_mean), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = ppt_sd), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tmean_mean), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tmean_sd), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tmin), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = tmax), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = vpdmin), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()

clim |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = pls_x, y = pls_y, color = vpdmax), alpha = 0.7, shape = '.') +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme_void()
